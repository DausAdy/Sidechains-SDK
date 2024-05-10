package io.horizen.account.storage

import io.horizen.account.proposition.AddressProposition
import io.horizen.account.state.ForgerPublicKeys
import io.horizen.account.state.receipt.EthereumReceipt
import io.horizen.account.utils.{AccountBlockFeeInfo, ForgerIdentifier}
import io.horizen.block.WithdrawalEpochCertificate
import io.horizen.consensus.ConsensusEpochNumber
import io.horizen.utils.WithdrawalEpochInfo
import sparkz.util.ModifierId

import java.math.BigInteger

// expect this storage to be passed by the app during SidechainApp initialization
trait AccountStateMetadataStorageReader {

  def getWithdrawalEpochInfo: WithdrawalEpochInfo

  def getFeePayments(withdrawalEpochNumber: Int): Seq[AccountBlockFeeInfo]

  def getTopQualityCertificate(referencedWithdrawalEpoch: Int): Option[WithdrawalEpochCertificate]

  def lastCertificateReferencedEpoch: Option[Int]

  def lastCertificateSidechainBlockId: Option[ModifierId]

  def getConsensusEpochNumber: Option[ConsensusEpochNumber]

  def getTransactionReceipt(txHash: Array[Byte]): Option[EthereumReceipt]

  def hasCeased: Boolean

  // tip height
  def getHeight: Int

  // zero bytes when storage is empty
  def getAccountStateRoot: Array[Byte] // 32 bytes, kessack hash

  def getForgerBlockCounters: Map[ForgerIdentifier, Long]

  def getMcForgerPoolRewards: Map[ForgerIdentifier, BigInteger]

  def getForgerRewards(
    forgerPublicKeys: ForgerPublicKeys,
    consensusEpochStart: Int,
    maxNumOfEpochs: Int,
  ): Seq[BigInteger]
}
