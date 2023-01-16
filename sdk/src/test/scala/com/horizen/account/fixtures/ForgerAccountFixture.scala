package com.horizen.account.fixtures

import com.google.common.primitives.Longs
import com.horizen.account.proposition.AddressProposition
import com.horizen.account.secret.PrivateKeySecp256k1
import com.horizen.account.utils.{Account, AccountPayment, Secp256k1}

import java.util.Random
import com.horizen.consensus.ForgingStakeInfo
import com.horizen.fixtures.SecretFixture
import com.horizen.proposition.VrfPublicKey
import com.horizen.secret.{PrivateKey25519, VrfKeyGenerator, VrfSecretKey}
import com.horizen.utils
import com.horizen.utils.Ed25519

import java.math.BigInteger
import java.util


case class ForgerAccountGenerationMetadata(propositionSecret: PrivateKey25519, blockSignSecret: PrivateKey25519, vrfSecret: VrfSecretKey,
                                       forgingStakeInfo: ForgingStakeInfo)

object ForgerAccountFixture extends SecretFixture {
  def generateForgerAccountData(seed: Long): (AccountPayment, ForgerAccountGenerationMetadata) = generateForgerAccountData(seed, None)

  def generateForgerAccountData(seed: Long,
                        vrfKeysOpt: Option[(VrfSecretKey, VrfPublicKey)]): (AccountPayment, ForgerAccountGenerationMetadata) = {
    val randomGenerator = new Random(seed)
    val byteSeed = new Array[Byte](32)
    randomGenerator.nextBytes(byteSeed)
    val signerKeyPair: utils.Pair[Array[Byte], Array[Byte]] = Ed25519.createKeyPair(byteSeed)
    val signerPrivKey: PrivateKey25519 = new PrivateKey25519(signerKeyPair.getKey, signerKeyPair.getValue)
    val value: Long = Math.abs(randomGenerator.nextLong)
    val (vrfSecret, vrfPubKey) = vrfKeysOpt.getOrElse{
      val vrfSecretKey = VrfKeyGenerator.getInstance().generateSecret(signerPrivKey.bytes())
      val vrfPublicKey = vrfSecretKey.publicImage()
        (vrfSecretKey, vrfPublicKey)
    }
    val blockSignProposition = signerPrivKey.publicImage()

    // TODO get a deterministic value with createEcKeyPair
    val ownerAddressProposition = new AddressProposition(util.Arrays.copyOf(byteSeed, Account.ADDRESS_SIZE))

    /*
    val ownerKeyPair = Secp256k1.createKeyPair(Longs.toByteArray(seed));
    val ownerPrivateKeyBytes = util.Arrays.copyOf(ownerKeyPair.getKey, Secp256k1.PRIVATE_KEY_SIZE)
    val ownerPrivateKey = new PrivateKeySecp256k1(ownerPrivateKeyBytes)
    val ownerAddressProposition = ownerPrivateKey.publicImage()
    */

    val accountPayment = AccountPayment(ownerAddressProposition, BigInteger.valueOf(value))
    val forgingStakeInfo: ForgingStakeInfo = ForgingStakeInfo(blockSignProposition, vrfPubKey, value)
    (accountPayment, ForgerAccountGenerationMetadata(signerPrivKey, signerPrivKey, vrfSecret, forgingStakeInfo))
  }

  def getAccountPayment(seed: Long): AccountPayment = {
    AccountPayment(getAddressProposition(seed), BigInteger.valueOf(scala.util.Random.nextInt(100)))
  }
}
