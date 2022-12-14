package com.horizen.certificatesubmitter.strategies

import com.horizen._
import com.horizen.block.{SidechainBlockBase, SidechainBlockHeaderBase}
import com.horizen.certificatesubmitter.AbstractCertificateSubmitter.SignaturesStatus
import com.horizen.certificatesubmitter.dataproof.CertificateData
import com.horizen.chain.{AbstractFeePaymentsInfo, MainchainHeaderInfo, SidechainBlockInfo}
import com.horizen.consensus.ConsensusEpochNumber
import com.horizen.fork.ForkManager
import com.horizen.params.NetworkParams
import com.horizen.storage.AbstractHistoryStorage
import com.horizen.transaction.Transaction
import com.horizen.utils.{BytesUtils, TimeToEpochUtils}
import scorex.util.ScorexLogging
import sparkz.core.NodeViewHolder.CurrentView
import sparkz.core.transaction.MemoryPool

import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.reflect.ClassTag
import scala.util.Try

abstract class CircuitStrategy[
  TX <: Transaction,
  H <: SidechainBlockHeaderBase,
  PM <: SidechainBlockBase[TX, H] : ClassTag,
  T <: CertificateData](settings: SidechainSettings, params: NetworkParams) extends ScorexLogging{
  
  def generateProof(certificateData: T, provingFileAbsolutePath: String): com.horizen.utils.Pair[Array[Byte], java.lang.Long]

  type FPI <: AbstractFeePaymentsInfo
  type HSTOR <: AbstractHistoryStorage[PM, FPI, HSTOR]
  type HIS <: AbstractHistory[TX, H, PM, FPI, HSTOR, HIS]
  type MS <: AbstractState[TX, H, PM, MS]
  type VL <: Wallet[SidechainTypes#SCS, SidechainTypes#SCP, TX, PM, VL]
  type MP <: MemoryPool[TX, MP]

  type View = CurrentView[HIS, MS, VL, MP]

  def buildCertificateData(sidechainNodeView: View, status: SignaturesStatus): T

  def getMessageToSign(view: View, referencedWithdrawalEpochNumber: Int): Try[Array[Byte]]

  // No MBTRs support, so no sense to specify btrFee different to zero.
  def getBtrFee(referencedWithdrawalEpochNumber: Int): Long = 0

  // Every positive value FT is allowed.
  protected [certificatesubmitter] def getFtMinAmount(consensusEpochNumber: Int): Long = {
    ForkManager.getSidechainConsensusEpochFork(consensusEpochNumber).ftMinAmount
  }

  protected def lastMainchainBlockCumulativeCommTreeHashForWithdrawalEpochNumber(history: HIS, withdrawalEpochNumber: Int): Array[Byte] = {
    val headerInfo: MainchainHeaderInfo = getLastMainchainBlockInfoForWithdrawalEpochNumber(history, withdrawalEpochNumber)
    headerInfo.cumulativeCommTreeHash
  }

  protected def lastConsensusEpochNumberForWithdrawalEpochNumber(history: HIS, withdrawalEpochNumber: Int): ConsensusEpochNumber = {
    val headerInfo: MainchainHeaderInfo = getLastMainchainBlockInfoForWithdrawalEpochNumber(history, withdrawalEpochNumber)

    val parentBlockInfo: SidechainBlockInfo = history.storage.blockInfoById(headerInfo.sidechainBlockId)
    TimeToEpochUtils.timeStampToEpochNumber(params, parentBlockInfo.timestamp)
  }

  protected def getLastMainchainBlockInfoForWithdrawalEpochNumber(history: HIS, withdrawalEpochNumber: Int): MainchainHeaderInfo = {
    val mcBlockHash = withdrawalEpochNumber match {
      case -1 => params.parentHashOfGenesisMainchainBlock
      case _ =>
        val mcHeight = params.mainchainCreationBlockHeight + (withdrawalEpochNumber + 1) * params.withdrawalEpochLength - 1
        history.getMainchainBlockReferenceInfoByMainchainBlockHeight(mcHeight).asScala
          .map(_.getMainchainHeaderHash).getOrElse(throw new IllegalStateException("Information for Mc is missed"))
    }
    log.debug(s"Last MC block hash for withdrawal epoch number $withdrawalEpochNumber is ${
      BytesUtils.toHexString(mcBlockHash)
    }")

    history.mainchainHeaderInfoByHash(mcBlockHash).getOrElse(throw new IllegalStateException("Missed MC Cumulative Hash"))
  }
}
