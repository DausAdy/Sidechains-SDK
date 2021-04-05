package com.horizen.block

import com.horizen.cryptolibprovider.CryptoLibProvider
import com.horizen.librustsidechains.FieldElement
import com.horizen.utils.{BytesUtils, Utils, VarInt}

import scala.util.Try

case class MainchainTxCswCrosschainInput(cswInputBytes: Array[Byte],
                                         amount: Long,                    // CAmount (int64_t)
                                         sidechainId: Array[Byte],        // uint256
                                         nullifier: Array[Byte],          // ScFieldElement
                                         pubKeyHash: Array[Byte],         // uint160
                                         scProof: Array[Byte],            // ScProof
                                         redeemScript: Array[Byte]        // CScript
                                        ) {

  lazy val hash: Array[Byte] = BytesUtils.reverseBytes(Utils.doubleSHA256Hash(cswInputBytes))

  def size: Int = cswInputBytes.length
}


object MainchainTxCswCrosschainInput {
  def create(cswInputBytes: Array[Byte], offset: Int): Try[MainchainTxCswCrosschainInput] = Try {
    if(offset < 0)
      throw new IllegalArgumentException("Input data corrupted.")

    var currentOffset: Int = offset

    val amount: Long = BytesUtils.getReversedLong(cswInputBytes, currentOffset)
    currentOffset += 8

    val sidechainId: Array[Byte] = BytesUtils.reverseBytes(cswInputBytes.slice(currentOffset, currentOffset + 32))
    currentOffset += 32

    val nullifierSize: VarInt = BytesUtils.getReversedVarInt(cswInputBytes, currentOffset)
    currentOffset += nullifierSize.size()
    if(nullifierSize.value() != FieldElement.FIELD_ELEMENT_LENGTH)
      throw new IllegalArgumentException(s"Input data corrupted: nullifier size ${nullifierSize.value()} " +
        s"is expected to be FieldElement size ${FieldElement.FIELD_ELEMENT_LENGTH}")
    val nullifier: Array[Byte] = BytesUtils.reverseBytes(cswInputBytes.slice(currentOffset, currentOffset + nullifierSize.value().intValue()))
    currentOffset += nullifierSize.value().intValue()

    val pubKeyHash: Array[Byte] = BytesUtils.reverseBytes(cswInputBytes.slice(currentOffset, currentOffset + 20))
    currentOffset += 20

    val scProofSize: VarInt = BytesUtils.getReversedVarInt(cswInputBytes, currentOffset)
    currentOffset += scProofSize.size()
    if(scProofSize.value() != CryptoLibProvider.sigProofThresholdCircuitFunctions.proofSizeLength())
      throw new IllegalArgumentException(s"Input data corrupted: scProof size ${scProofSize.value()} " +
        s"is expected to be ScProof size ${CryptoLibProvider.sigProofThresholdCircuitFunctions.proofSizeLength()}")

    val scProof: Array[Byte] = cswInputBytes.slice(currentOffset, currentOffset + scProofSize.value().intValue())
    currentOffset += scProofSize.value().intValue()

    val scriptLength: VarInt = BytesUtils.getReversedVarInt(cswInputBytes, currentOffset)
    currentOffset += scriptLength.size()

    val redeemScript: Array[Byte] = cswInputBytes.slice(currentOffset, currentOffset + scriptLength.value().intValue())
    currentOffset += scriptLength.value().intValue()

    new MainchainTxCswCrosschainInput(cswInputBytes.slice(offset, currentOffset),
      amount, sidechainId, nullifier, pubKeyHash, scProof, redeemScript)
  }
}