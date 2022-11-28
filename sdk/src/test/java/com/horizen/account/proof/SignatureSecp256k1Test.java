package com.horizen.account.proof;

import com.horizen.account.proposition.AddressProposition;
import com.horizen.utils.BytesUtils;
import org.junit.Before;
import org.junit.Test;
import org.web3j.crypto.ECKeyPair;
import org.web3j.crypto.Keys;
import org.web3j.crypto.RawTransaction;
import org.web3j.crypto.Sign;
import org.web3j.utils.Numeric;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.SignatureException;

import static org.junit.Assert.*;

public class SignatureSecp256k1Test {
    SignatureSecp256k1 signatureSecp256k1;
    Sign.SignatureData signatureData;
    byte[] message;
    AddressProposition addressProposition;

    @Before
    public void BeforeEachTest() throws InvalidAlgorithmParameterException, NoSuchAlgorithmException, NoSuchProviderException {
        // Create the raw Transaction
        String payload = "This is string to sign";
        message = payload.getBytes(StandardCharsets.UTF_8);

        // Create a key pair and create the signature
        ECKeyPair pair = Keys.createEcKeyPair();
        signatureData = Sign.signMessage(message, pair, true);
        signatureSecp256k1 = new SignatureSecp256k1(signatureData);
        addressProposition = new AddressProposition(BytesUtils.fromHexString(Keys.getAddress(pair)));
    }

    @Test
    public void signatureSecp256k1Test() throws InvalidAlgorithmParameterException, NoSuchAlgorithmException, NoSuchProviderException, SignatureException {
        // Test 1: Illegal Argument Exception while creating new PrivateKeySecp256k1 expected
        assertThrows(IllegalArgumentException.class, () -> new SignatureSecp256k1(signatureData.getV(), signatureSecp256k1.getR(), null));

        // Test 2: Illegal Argument Exception while creating new PrivateKeySecp256k1 expected
        assertThrows(IllegalArgumentException.class, () -> new SignatureSecp256k1(signatureData.getV(), null, signatureSecp256k1.getS()));

        // Test 3: Illegal Argument Exception while creating new PrivateKeySecp256k1 expected
        assertThrows(IllegalArgumentException.class, () -> new SignatureSecp256k1(null, signatureSecp256k1.getR(), signatureSecp256k1.getS()));

        // Test 4: Illegal Argument Exception while creating new PrivateKeySecp256k1 expected
        assertThrows(IllegalArgumentException.class, () -> new SignatureSecp256k1(new byte[20], signatureSecp256k1.getR(), signatureSecp256k1.getS()));

        // Test 5: Successful creation expected
        assertNotNull(new SignatureSecp256k1(signatureData.getV(), signatureSecp256k1.getR(), signatureSecp256k1.getS()));

        // Test 6: Returns signature data string correctly
        assertEquals(String.format("SignatureSecp256k1{v=%s, r=%s, s=%s}", Numeric.toHexString(signatureData.getV()), Numeric.toHexString(signatureData.getR()), Numeric.toHexString(signatureData.getS())), signatureSecp256k1.toString());

        // Test 7: Returns false because of invalid signature
        AddressProposition newAddressProposition = new AddressProposition(Keys.getAddress(Keys.createEcKeyPair().getPublicKey().toByteArray()));
        assertFalse(signatureSecp256k1.isValid(newAddressProposition, message));

        // Test 8: Returns true for valid signature
        assertTrue(signatureSecp256k1.isValid(addressProposition, message));
    }
}
