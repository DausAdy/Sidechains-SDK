package io.horizen.account.storage

import io.horizen.account.state.ForgerPublicKeys
import io.horizen.account.state.receipt.EthereumReceipt
import io.horizen.account.utils.{AccountBlockFeeInfo, ForgerIdentifier}
import io.horizen.block.WithdrawalEpochCertificate
import io.horizen.consensus.ConsensusEpochNumber
import io.horizen.storage.{SidechainStorageInfo, Storage}
import io.horizen.utils.{ByteArrayWrapper, WithdrawalEpochInfo}
import sparkz.util.{ModifierId, SparkzLogging}

import java.math.BigInteger
import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters._
import scala.util.Try

// expect this storage to be passed by the app during SidechainApp initialization
class AccountStateMetadataStorage(storage: Storage)
  extends AccountStateMetadataStorageReader with SidechainStorageInfo with SparkzLogging
{
  def getView: AccountStateMetadataStorageView = new AccountStateMetadataStorageView(storage)

  def lastVersionId: Option[ByteArrayWrapper] = {
    storage.lastVersionID().asScala
  }

  def rollbackVersions: Seq[ByteArrayWrapper] = {
    storage.rollbackVersions().asScala.toList
  }

  def rollback(version: ByteArrayWrapper): Try[AccountStateMetadataStorage] = Try {
    require(version != null, "Version to rollback to must be NOT NULL.")
    storage.rollback(version)
    this
  }

  def isEmpty: Boolean = storage.isEmpty

  override def getWithdrawalEpochInfo: WithdrawalEpochInfo = getView.getWithdrawalEpochInfo

  override def getFeePayments(withdrawalEpochNumber: Int): Seq[AccountBlockFeeInfo] = getView.getFeePayments(withdrawalEpochNumber)

  override def getTopQualityCertificate(referencedWithdrawalEpoch: Int): Option[WithdrawalEpochCertificate] = getView.getTopQualityCertificate(referencedWithdrawalEpoch)

  override def lastCertificateReferencedEpoch: Option[Int] = getView.lastCertificateReferencedEpoch

  override def lastCertificateSidechainBlockId: Option[ModifierId] = getView.lastCertificateSidechainBlockId

  override def getConsensusEpochNumber: Option[ConsensusEpochNumber] = getView.getConsensusEpochNumber

  override def hasCeased: Boolean = getView.hasCeased

  override def getHeight: Int = getView.getHeight

  override def getAccountStateRoot: Array[Byte] = getView.getAccountStateRoot

  override def getTransactionReceipt(txHash: Array[Byte]): Option[EthereumReceipt] = getView.getTransactionReceipt(txHash)

  override def getForgerBlockCounters: Map[ForgerIdentifier, Long] = getView.getForgerBlockCounters

  override def getMcForgerPoolRewards: Map[ForgerIdentifier, BigInteger] = getView.getMcForgerPoolRewards

  override def getForgerRewards(
    forgerPublicKeys: ForgerPublicKeys,
    consensusEpochStart: Int,
    maxNumOfEpochs: Int,
  ): Seq[BigInteger] = getView.getForgerRewards(forgerPublicKeys, consensusEpochStart, maxNumOfEpochs)
}
