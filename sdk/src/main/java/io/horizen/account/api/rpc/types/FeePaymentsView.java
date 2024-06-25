package io.horizen.account.api.rpc.types;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.horizen.account.chain.AccountFeePaymentsInfo;
import io.horizen.account.utils.AccountPayment;
import io.horizen.evm.Address;
import scala.collection.JavaConverters;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class FeePaymentsView {
    public final List<FeePaymentData> payments;

    public FeePaymentsView(AccountFeePaymentsInfo info) {
        payments = JavaConverters
            .seqAsJavaList(info.payments())
            .stream()
            .map(payment -> FeePaymentData.fromAccountFeePayment(payment))
            .collect(Collectors.toList());
    }

    @JsonInclude(JsonInclude.Include.NON_ABSENT)
    private static class FeePaymentData {
        public final Address address;
        public final BigInteger value;
        public final Optional<BigInteger> valueFromMainchain;
        public final Optional<BigInteger> valueFromFees;

        public FeePaymentData(Address address, BigInteger value, Optional<BigInteger> valueFromMainchain, Optional<BigInteger> valueFromFees) {
            this.address = address;
            this.value = value;
            this.valueFromMainchain = valueFromMainchain;
            this.valueFromFees = valueFromFees;
        }

        public static FeePaymentData fromAccountFeePayment(AccountPayment payment) {
            return new FeePaymentData(
                payment.address().address(),
                payment.value(),
                Optional.ofNullable(payment.valueFromMainchain().getOrElse(() -> null)),
                Optional.ofNullable(payment.valueFromFees().getOrElse(() -> null))
            );
        }
    }
}
