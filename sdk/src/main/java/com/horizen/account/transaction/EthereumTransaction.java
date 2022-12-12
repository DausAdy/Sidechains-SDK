package com.horizen.account.transaction;

import com.fasterxml.jackson.annotation.*;
import com.horizen.account.proof.SignatureSecp256k1;
import com.horizen.account.proposition.AddressProposition;
import com.horizen.account.state.GasUintOverflowException;
import com.horizen.account.state.GasUtil;
import com.horizen.account.state.Message;
import com.horizen.account.utils.BigIntegerUtil;
import com.horizen.account.utils.EthereumTransactionEncoder;
import com.horizen.serialization.Views;
import com.horizen.transaction.TransactionSerializer;
import com.horizen.transaction.exception.TransactionSemanticValidityException;
import com.horizen.utils.BytesUtils;
import org.apache.logging.log4j.LogManager;
import org.jetbrains.annotations.NotNull;
import org.web3j.crypto.*;
import org.web3j.utils.Numeric;
import javax.annotation.Nullable;
import java.math.BigInteger;
import java.util.Optional;
import static com.horizen.account.utils.Secp256k1.PUBLIC_KEY_SIZE;


@JsonPropertyOrder({
        "id", "from", "to", "value", "nonce", "data",
        "gasPrice", "gasLimit", "maxFeePerGas", "maxPriorityFeePerGas",
        "eip1559", "version", "chainId", "signed", "signature"
})
@JsonIgnoreProperties({"transaction", "encoder", "modifierTypeId"})
@JsonView(Views.Default.class)
public class EthereumTransaction extends AccountTransaction<AddressProposition, SignatureSecp256k1> {

    //  The 3 versions of tx are supported by go eth and we have test vectors generated using all of them
    //  We are using elsewhere the enum from w3j, which just supports 0 and 2:
    //   org/web3j/crypto/transaction/type/TransactionType.java
    public enum EthereumTransactionType {
        LegacyTxType,     // Legacy
        AccessListTxType, // - not supported
        DynamicFeeTxType  // eip1559
    }

    private final EthereumTransactionType type;
    private final BigInteger nonce;

    @JsonProperty("to")
    private final AddressProposition to;

    @JsonProperty("gasPrice")
    private final BigInteger gasPrice;

    private final BigInteger gasLimit;
    private final BigInteger value;
    private final Long chainId;
    private final byte[] data;

    @JsonProperty("maxPriorityFeePerGas")
    private final BigInteger maxPriorityFeePerGas;
    @JsonProperty("maxFeePerGas")
    private final BigInteger maxFeePerGas;

    private final SignatureSecp256k1 signature;

    private AddressProposition from;
    private String hashString;
    private BigInteger txCost;

    private synchronized String getTxHash() {
        if (this.hashString == null) {
            byte[] encodedMessage = encode(isSigned());
            this.hashString = BytesUtils.toHexString(Hash.sha3(encodedMessage, 0, encodedMessage.length));
        }
        return this.hashString;
    }

    @Override
    public synchronized BigInteger maxCost() {
        if (this.txCost == null) {
            this.txCost = super.maxCost();
        }
        return this.txCost;
    }

    // creates a legacy transaction
    public EthereumTransaction(
            @NotNull Optional<AddressProposition> to,
            @NotNull BigInteger nonce,
            @NotNull BigInteger gasPrice,
            @NotNull BigInteger gasLimit,
            @NotNull BigInteger value,
            @NotNull byte[] data,
            @Nullable SignatureSecp256k1 inSignature
    ) {
        this.type = EthereumTransactionType.LegacyTxType;
        this.nonce = nonce;
        this.gasPrice = gasPrice;
        this.gasLimit = gasLimit;
        this.value = value;

        this.chainId = null;
        this.maxPriorityFeePerGas = null;
        this.maxFeePerGas = null;

        this.to = to.orElse(null);
        this.data = data;
        this.signature = inSignature;
    }


    // creates a legacy eip155 transaction
    public EthereumTransaction(
            @NotNull Long chainId,
            @NotNull Optional<AddressProposition> to,
            @NotNull BigInteger nonce,
            @NotNull BigInteger gasPrice,
            @NotNull BigInteger gasLimit,
            @NotNull BigInteger value,
            @NotNull byte[] data,
            @Nullable SignatureSecp256k1 inSignature
    ) {
        this.type = EthereumTransactionType.LegacyTxType;
        this.nonce = nonce;
        this.gasPrice = gasPrice;
        this.gasLimit = gasLimit;
        this.value = value;
        this.chainId = chainId;

        this.maxPriorityFeePerGas = null;
        this.maxFeePerGas = null;

        this.to = to.orElse(null);
        this.data = data;
        this.signature = inSignature;
    }

    // creates an eip1559 transaction
    public EthereumTransaction(
            @NotNull Long chainId,
            @NotNull Optional<AddressProposition> to,
            @NotNull BigInteger nonce,
            @NotNull BigInteger gasLimit,
            @NotNull BigInteger maxPriorityFeePerGas,
            @NotNull BigInteger maxFeePerGas,
            @NotNull BigInteger value,
            @NotNull byte[] data,
            @Nullable SignatureSecp256k1 inSignature
    ) {
        this.type = EthereumTransactionType.DynamicFeeTxType;
        this.nonce = nonce;
        this.gasPrice = null;
        this.gasLimit = gasLimit;
        this.value = value;

        this.chainId = chainId;
        this.maxPriorityFeePerGas = maxPriorityFeePerGas;
        this.maxFeePerGas = maxFeePerGas;

        this.to = to.orElse(null);
        this.data = data;
        this.signature = inSignature;
    }

    // creates a signed transaction from an existing one
    public EthereumTransaction(
            EthereumTransaction txToSign,
            @Nullable SignatureSecp256k1 inSignature
    ) {
        this.type = txToSign.type;
        this.nonce = txToSign.nonce;
        this.gasPrice = txToSign.gasPrice;
        this.gasLimit = txToSign.gasLimit;
        this.to = txToSign.to;
        this.value = txToSign.value;
        this.data = txToSign.data;

        this.chainId = txToSign.chainId;
        this.maxPriorityFeePerGas = txToSign.maxPriorityFeePerGas;
        this.maxFeePerGas = txToSign.maxFeePerGas;

        this.signature = inSignature;
    }

    public boolean isSigned() {
        return (signature != null);
    }

    @Override
    public byte transactionTypeId() {
        return AccountTransactionsIdsEnum.EthereumTransactionId.id();
    }

    @Override
    @JsonProperty("id")
    public String id() {
        return getTxHash();
    }


    @Override
    @JsonProperty("version")
    public byte version() {
        return (byte)this.type.ordinal();
    }

    @Override
    public TransactionSerializer serializer() {
        return EthereumTransactionSerializer.getSerializer();
    }

    @Override
    public void semanticValidity() throws TransactionSemanticValidityException {

        if (!isSigned()) {
            throw new TransactionSemanticValidityException(String.format("Transaction [%s] is not signed", id()));
        }

        if (getChainId() != null && getChainId() < 1L) {
            throw new TransactionSemanticValidityException(String.format("Transaction [%s] has invalid chainId set: %d", id(), getChainId()));
        }

        // for 'to' address, all checks have been performed during obj initialization
        if (this.getTo().isEmpty()) {
            // contract creation
            if (this.getData().length == 0)
                throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                        "smart contract declaration transaction without data", id()));
        }

        if (getValue().signum() < 0)
            throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                    "negative value", id()));
        if (getNonce().signum() < 0)
            throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                    "negative nonce", id()));
        if (getGasLimit().signum() <= 0)
            throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                    "non-positive gas limit", id()));
        if (!BigIntegerUtil.isUint64(getGasLimit()))
            throw new GasUintOverflowException();

        if (isEIP1559()) {
            if (getMaxFeePerGas().signum() < 0)
                throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                        "eip1559 transaction with negative maxFeePerGas", id()));
            if (getMaxPriorityFeePerGas().signum() < 0)
                throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                        "eip1559 transaction with negative maxPriorityFeePerGas", id()));
            if (!BigIntegerUtil.isUint256(getMaxFeePerGas()))
                throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                        "eip1559 transaction maxFeePerGas bit length [%d] is too high", id(), getMaxFeePerGas().bitLength()));
            if (!BigIntegerUtil.isUint256(getMaxPriorityFeePerGas()))
                throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                        "eip1559 transaction maxPriorityFeePerGas bit length [%d] is too high", id(), getMaxPriorityFeePerGas().bitLength()));
            if (getMaxFeePerGas().compareTo(getMaxPriorityFeePerGas()) < 0)
                throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                                "eip1559 transaction maxPriorityFeePerGas [%s] higher than maxFeePerGas [%s]",
                        id(), getMaxPriorityFeePerGas(), getMaxFeePerGas()));
        } else {
            if (getGasPrice().signum() < 0)
                throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                        "legacy transaction with negative gasPrice", id()));
            if (!BigIntegerUtil.isUint256(getGasPrice()))
                throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                        "legacy transaction gasPrice bit length [%d] is too high", id(), getGasPrice().bitLength()));
        }
        if (getGasLimit().compareTo(GasUtil.intrinsicGas(getData(), getTo().isEmpty())) < 0) {
            throw new TransactionSemanticValidityException(String.format("Transaction [%s] is semantically invalid: " +
                    "gas limit %s is below intrinsic gas %s",
                    id(), getGasLimit(), GasUtil.intrinsicGas(getData(), getTo().isEmpty())));
        }
        try {
            if (!getSignature().isValid(getFrom(), messageToSign()))
                throw new TransactionSemanticValidityException("Cannot create signed transaction with invalid " +
                        "signature");
        } catch (Throwable t) {
            // in case of really malformed signature we can not even compute the id()
            throw new TransactionSemanticValidityException(String.format("Transaction signature not readable: %s", t.getMessage()));
        }

    }

    @Override
    public long size() {
        return serializer().toBytes(this).length;
    }

    @Override
    public BigInteger getNonce() {
        return this.nonce;
    }

    @Override
    @JsonIgnore
    public BigInteger getGasPrice() {
        if (isLegacy())
            return this.gasPrice;
        //in Geth for EIP1559 tx gasPrice returns gasFeeCap
        return this.maxFeePerGas;
    }

    @Override
    @JsonIgnore
    public BigInteger getMaxFeePerGas() {
        if (isEIP1559())
            return this.maxFeePerGas;
        //in Geth for Legacy tx gasFeeCap is equal to gasPrice
        return this.gasPrice;
    }

    @Override
    @JsonIgnore
    public BigInteger getMaxPriorityFeePerGas() {
        if (isEIP1559())
            return this.maxPriorityFeePerGas;
        //in Geth for Legacy tx MaxPriorityFee is equal to gasPrice
        return this.gasPrice;
    }

    @Override
    @JsonIgnore
    public BigInteger getPriorityFeePerGas(BigInteger base) {
        if (isEIP1559()) {
            return getMaxFeePerGas().subtract(base).min(getMaxPriorityFeePerGas());
        } else {
            return getGasPrice().subtract(base);
        }
    }

    @Override
    @JsonIgnore
    public BigInteger getEffectiveGasPrice(BigInteger base) {
        if (isEIP1559())
            return base.add(getMaxPriorityFeePerGas()).min(getMaxFeePerGas());
        else
            return getGasPrice();
    }

    public Long getChainId() {
        if (isEIP1559() || isEIP155())
            return this.chainId;
        else {
            return null;
        }
    }

    public boolean isEIP1559() {
        return this.type == EthereumTransactionType.DynamicFeeTxType;
    }

    public boolean isLegacy() {
        return this.type == EthereumTransactionType.LegacyTxType;
    }

    public boolean isEIP155() {
        return (isLegacy() && this.chainId != null);
    }

    @Override
    public BigInteger getGasLimit() {
        return this.gasLimit;
    }

    @Override
    @JsonIgnore
    public Optional<AddressProposition> getTo() {
        return Optional.ofNullable(this.to);
    }

    @JsonIgnore
    public String getToAddressString() {
        if (this.to != null)
            return BytesUtils.toHexString(this.to.address());
        return "";
    }

    @Override
    public synchronized AddressProposition getFrom() {
        if (this.from == null && this.signature != null) {
            try {
                byte[] encodedTransaction = encode(false);

                BigInteger pubKey = Sign.signedMessageToKey(encodedTransaction, this.signature.getSignatureData());
                this.from = new AddressProposition(Keys.getAddress(Numeric.toBytesPadded(pubKey, PUBLIC_KEY_SIZE)));
            } catch (Exception e) {
                // whatever exception may result in processing the signature, we can not tell the from address
                LogManager.getLogger().info("Could not find from address, Signature not valid:", e);
                this.from = null;
            }
        }
        return this.from;
    }

    @JsonIgnore
    public String getFromString() {
        if (this.getFrom() != null)
            return BytesUtils.toHexString(this.from.address());
        return "";
    }

    @Override
    public BigInteger getValue() {
        return this.value;
    }

    @Override
    public byte[] getData() {
        return this.data;
    }

    @JsonIgnore
    public String getDataString() {
        if (this.data != null)
          return BytesUtils.toHexString(this.data);
        return "";
    }

    @Override
    public SignatureSecp256k1 getSignature() {
        return this.signature;
    }

    @Override
    public String toString() {
        if (isEIP1559())
            return String.format(
                "EthereumTransaction{id=%s, from=%s, nonce=%s, gasLimit=%s, to=%s, value=%s, data=%s, " +
                        "maxFeePerGas=%s, maxPriorityFeePerGas=%s, chainId=%s, version=%d, Signature=%s}",
                id(),
                getFromString(),
                Numeric.toHexStringWithPrefix(getNonce() != null ? getNonce() : BigInteger.ONE.negate()),
                Numeric.toHexStringWithPrefix(getGasLimit() != null ? getGasLimit() : BigInteger.ZERO),
                getToAddressString(),
                Numeric.toHexStringWithPrefix(getValue() != null ? getValue() : BigInteger.ZERO),
                getDataString(),
                Numeric.toHexStringWithPrefix(getMaxFeePerGas() != null ? getMaxFeePerGas() : BigInteger.ZERO),
                Numeric.toHexStringWithPrefix(getMaxPriorityFeePerGas() != null ? getMaxPriorityFeePerGas() : BigInteger.ZERO),
                getChainId() != null ? getChainId() : "",
                (int)version(),
                isSigned() ? this.signature.toString() : ""
            );
        else
            return String.format(
                "EthereumTransaction{id=%s, from=%s, nonce=%s, gasPrice=%s, gasLimit=%s, to=%s, value=%s, data=%s, " +
                        "chainId=%s, version=%d, Signature=%s}",
                id(),
                getFromString(),
                Numeric.toHexStringWithPrefix(getNonce() != null ? getNonce() : BigInteger.ONE.negate()),
                Numeric.toHexStringWithPrefix(getGasPrice() != null ? getGasPrice() : BigInteger.ZERO),
                Numeric.toHexStringWithPrefix(getGasLimit() != null ? getGasLimit() : BigInteger.ZERO),
                getToAddressString(),
                Numeric.toHexStringWithPrefix(getValue() != null ? getValue() : BigInteger.ZERO),
                getDataString(),
                getChainId() != null ? getChainId() : "",
                (int)version(),
                isSigned() ? this.signature.toString() : ""
        );
    }

    @Override
    public byte[] messageToSign() {
       return encode(false);
    }

    public Message asMessage(BigInteger baseFee) {
        var gasFeeCap = isEIP1559() ? getMaxFeePerGas() : getGasPrice();
        var gasTipCap = isEIP1559() ? getMaxPriorityFeePerGas() : getGasPrice();
        // calculate effective gas price as baseFee + tip capped at the fee cap
        // this will default to gasPrice if the transaction is not EIP-1559
        var effectiveGasPrice = getEffectiveGasPrice(baseFee);
        return new Message(
                Optional.ofNullable(this.from),
                Optional.ofNullable(this.to),
                effectiveGasPrice,
                gasFeeCap,
                gasTipCap,
                getGasLimit(),
                getValue(),
                getNonce(),
                getData(),
                false
        );
    }

    public byte[] encode(boolean accountSignature) {
        if (this.isEIP1559()) {
            return EthereumTransactionEncoder.encodeEip1559AsRlpValues(this, accountSignature);
        } else {
            return EthereumTransactionEncoder.encodeLegacyAsRlpValues(this, accountSignature);
        }
    }
}
