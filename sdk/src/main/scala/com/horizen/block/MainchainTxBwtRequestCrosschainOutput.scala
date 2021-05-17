package com.horizen.block

import com.horizen.cryptolibprovider.CryptoLibProvider
import com.horizen.librustsidechains.FieldElement
import com.horizen.utils.{BytesUtils, Utils, VarInt}

import scala.util.Try

case class MainchainTxBwtRequestCrosschainOutput(bwtRequestOutputBytes: Array[Byte],
                                                 override val sidechainId: Array[Byte], // uint256
                                                 scRequestData: Array[Array[Byte]],     // vector<ScFieldElement>
                                                 mcDestinationAddress: Array[Byte],     // uint160
                                                 scFee: Long                            // CAmount (int64_t)
                                                ) extends MainchainTxCrosschainOutput {

  override lazy val hash: Array[Byte] = BytesUtils.reverseBytes(Utils.doubleSHA256Hash(bwtRequestOutputBytes))

  def size: Int = bwtRequestOutputBytes.length
}

object MainchainTxBwtRequestCrosschainOutput {
  def create(bwtRequestOutputBytes: Array[Byte], offset: Int): Try[MainchainTxBwtRequestCrosschainOutput] = Try {
    if(offset < 0)
      throw new IllegalArgumentException("Input data corrupted. Offset is negative.")

    var currentOffset: Int = offset

    val sidechainId: Array[Byte] = BytesUtils.reverseBytes(bwtRequestOutputBytes.slice(currentOffset, currentOffset + 32))
    currentOffset += 32

    val scRequestDataSize: VarInt = BytesUtils.getReversedVarInt(bwtRequestOutputBytes, currentOffset)
    currentOffset += scRequestDataSize.size()

    val scRequestDataSeq: Seq[Array[Byte]] = (1 to scRequestDataSize.value().intValue()).map(idx => {
      val dataSize = BytesUtils.getReversedVarInt(bwtRequestOutputBytes, currentOffset)
      currentOffset += dataSize.size()

      if(dataSize.value() != FieldElement.FIELD_ELEMENT_LENGTH)
        throw new IllegalArgumentException(s"Input data corrupted: scRequestData[$idx] size ${dataSize.value()} " +
          s"is expected to be FieldElement size ${FieldElement.FIELD_ELEMENT_LENGTH}")

      val scRequestData: Array[Byte] = BytesUtils.reverseBytes(bwtRequestOutputBytes.slice(currentOffset, currentOffset + dataSize.value().intValue()))
      currentOffset += dataSize.value().intValue()

      scRequestData
    })

    val mcDestinationAddress: Array[Byte] = BytesUtils.reverseBytes(bwtRequestOutputBytes.slice(currentOffset, currentOffset + 20))
    currentOffset += 20

    val scFee: Long = BytesUtils.getReversedLong(bwtRequestOutputBytes, currentOffset)
    currentOffset += 8

    new MainchainTxBwtRequestCrosschainOutput(bwtRequestOutputBytes.slice(offset, currentOffset),
      sidechainId, scRequestDataSeq.toArray, mcDestinationAddress, scFee)
  }
}
