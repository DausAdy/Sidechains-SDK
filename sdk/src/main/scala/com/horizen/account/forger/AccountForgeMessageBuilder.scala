package com.horizen.account.forger

import com.horizen.SidechainTypes
import com.horizen.account.block.AccountBlock.calculateReceiptRoot
import com.horizen.account.block.{AccountBlock, AccountBlockHeader}
import com.horizen.account.companion.SidechainAccountTransactionsCompanion
import com.horizen.account.history.AccountHistory
import com.horizen.account.mempool.AccountMemoryPool
import com.horizen.account.proposition.AddressProposition
import com.horizen.account.receipt.EthereumConsensusDataReceipt
import com.horizen.account.secret.PrivateKeySecp256k1
import com.horizen.account.state._
import com.horizen.account.storage.AccountHistoryStorage
import com.horizen.account.transaction.EthereumTransaction
import com.horizen.account.utils.Account
import com.horizen.account.wallet.AccountWallet
import com.horizen.block._
import com.horizen.consensus._
import com.horizen.forge.{AbstractForgeMessageBuilder, MainchainSynchronizer}
import com.horizen.params.NetworkParams
import com.horizen.proof.{Signature25519, VrfProof}
import com.horizen.secret.{PrivateKey25519, Secret}
import com.horizen.transaction.TransactionSerializer
import com.horizen.utils.{ByteArrayWrapper, BytesUtils, ClosableResourceHandler, DynamicTypedSerializer, ForgingStakeMerklePathInfo, ListSerializer, MerklePath, MerkleTree}
import scorex.core.NodeViewModifier
import scorex.core.block.Block.{BlockId, Timestamp}
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class AccountForgeMessageBuilder(mainchainSynchronizer: MainchainSynchronizer,
                                 companion: SidechainAccountTransactionsCompanion,
                                 params: NetworkParams,
                                 allowNoWebsocketConnectionInRegtest: Boolean)
  extends AbstractForgeMessageBuilder[
    SidechainTypes#SCAT,
    AccountBlockHeader,
    AccountBlock](
    mainchainSynchronizer, companion, params, allowNoWebsocketConnectionInRegtest
  )
    with ClosableResourceHandler
    with ScorexLogging {
  type HSTOR = AccountHistoryStorage
  type VL = AccountWallet
  type HIS = AccountHistory
  type MS = AccountState
  type MP = AccountMemoryPool

  def computeStateRoot(view: AccountStateView, sidechainTransactions: Seq[SidechainTypes#SCAT],
                       mainchainBlockReferencesData: Seq[MainchainBlockReferenceData],
                       inputBlockSize: Int): (Array[Byte], Seq[EthereumConsensusDataReceipt], Seq[SidechainTypes#SCAT]) = {

    // we must ensure that all the tx we get from mempool are applicable to current state view
    // and we must stay below the block gas limit threshold, therefore we might have a subset of the input transactions
    val (receiptList, listOfAppliedTxHash) = tryApplyAndGetReceipts(view, mainchainBlockReferencesData, sidechainTransactions, inputBlockSize).get

    // get only the transactions for which we got a receipt
    val appliedTransactions = sidechainTransactions.filter {
      t => {
        val txHash = idToBytes(t.id)
        listOfAppliedTxHash.contains(new ByteArrayWrapper(txHash))
      }
    }

    (view.getIntermediateRoot, receiptList, appliedTransactions)
  }


  def blockSizeExceeded(blockSize: Int, txCounter: Int): Boolean = {
    if (txCounter > SidechainBlockBase.MAX_SIDECHAIN_TXS_NUMBER || blockSize > SidechainBlockBase.MAX_BLOCK_SIZE)
      true // stop data collection
    else {
      false // continue data collection
    }
  }

  private def tryApplyAndGetReceipts(stateView: AccountStateView,
                                     mainchainBlockReferencesData: Seq[MainchainBlockReferenceData],
                                     sidechainTransactions: Seq[SidechainTypes#SCAT],
                                     inputBlockSize: Int): Try[(Seq[EthereumConsensusDataReceipt], Seq[ByteArrayWrapper])] = Try {

    for (mcBlockRefData <- mainchainBlockReferencesData) {
      stateView.applyMainchainBlockReferenceData(mcBlockRefData).get
    }

    val receiptList = new ListBuffer[EthereumConsensusDataReceipt]()
    val txHashList = new ListBuffer[ByteArrayWrapper]()

    val blockGasPool = new GasPool(stateView.getBlockGasLimit)
    var txsCounter: Int = 0
    var blockSize: Int = inputBlockSize
    val listOfAccountsToSkip = new mutable.HashSet[SidechainTypes#SCP]

    for ((tx, txIndex) <- sidechainTransactions.zipWithIndex if !listOfAccountsToSkip.contains(tx.getFrom)) {

      blockSize = blockSize + tx.bytes.length + 4 // placeholder for Tx length
      txsCounter += 1

      if (blockSizeExceeded(blockSize, txsCounter))
        return Success(receiptList, txHashList)

      stateView.applyTransaction(tx, txIndex, blockGasPool) match {
        case Success(consensusDataReceipt) =>

          val ethTx = tx.asInstanceOf[EthereumTransaction]
          val txHash = BytesUtils.fromHexString(ethTx.id)

          receiptList += consensusDataReceipt
          txHashList += txHash

        case Failure(e: GasLimitReached) =>
          // block gas limit reached
          // keep trying to fit transactions into the block: this TX did not fit, but another one might
          log.debug(s"Could not apply tx, reason: ${e.getMessage}")
          // skip all txs from the same account
          listOfAccountsToSkip += tx.getFrom
        case Failure(e: FeeCapTooLowException) =>
          // stop forging because all the remaining txs cannot be executed for the nonce, if they are from the same account, or,
          // if they are from other accounts, they will have a lower fee cap
          log.debug(s"Could not apply tx, reason: ${e.getMessage}")
          return Success(receiptList, txHashList)
        case Failure(e: NonceTooLowException) =>
          // just skip this tx
         log.debug(s"Could not apply tx, reason: ${e.getMessage}")
        case Failure(e) =>
          // skip all txs from the same account
          listOfAccountsToSkip += tx.getFrom
          log.debug(s"Could not apply tx, reason: ${e.getMessage}. Skipping all next transactions from the same account because not executable anymore",e)
      }
    }
    (receiptList, txHashList)
  }

  override def createNewBlock(
                               nodeView: View,
                               branchPointInfo: BranchPointInfo,
                               isWithdrawalEpochLastBlock: Boolean,
                               parentId: BlockId,
                               timestamp: Timestamp,
                               mainchainBlockReferencesData: Seq[MainchainBlockReferenceData],
                               sidechainTransactions: Seq[SidechainTypes#SCAT],
                               mainchainHeaders: Seq[MainchainHeader],
                               ommers: Seq[Ommer[AccountBlockHeader]],
                               ownerPrivateKey: PrivateKey25519,
                               forgingStakeInfo: ForgingStakeInfo,
                               vrfProof: VrfProof,
                               forgingStakeInfoMerklePath: MerklePath,
                               companion: DynamicTypedSerializer[SidechainTypes#SCAT, TransactionSerializer[SidechainTypes#SCAT]],
                               inputBlockSize: Int,
                               signatureOption: Option[Signature25519]): Try[SidechainBlockBase[SidechainTypes#SCAT, AccountBlockHeader]] = {

    val feePaymentsHash: Array[Byte] = new Array[Byte](MerkleTree.ROOT_HASH_LENGTH)

    // 1. create a view and try to apply all transactions in the list.
    val (stateRoot, receiptList, appliedTxList)
    : (Array[Byte], Seq[EthereumConsensusDataReceipt], Seq[SidechainTypes#SCAT]) = using(nodeView.state.getView) { dummyView =>
      // the outputs will be:
      // - the resulting stateRoot
      // - the list of receipt of the transactions successfully applied ---> for getting the receiptsRoot
      // - the list of transactions successfully applied to the state ---> to be included in the forged block
      computeStateRoot(dummyView, sidechainTransactions, mainchainBlockReferencesData, inputBlockSize)
    }
    // 2. Compute the receipt root
    val receiptsRoot: Array[Byte] = calculateReceiptRoot(receiptList)

    // 3. As forger address take first address from the wallet
    val addressList = nodeView.vault.secretsOfType(classOf[PrivateKeySecp256k1])
    if (addressList.size() == 0)
      throw new IllegalArgumentException("No addresses in wallet!")

    val forgerAddress = addressList.get(0).publicImage().asInstanceOf[AddressProposition]

    // TODO: 4. calculate baseFee
    val baseFee = 0

    // 5. Get cumulativeGasUsed from last receipt in list if available
    val gasUsed = if (receiptList.nonEmpty) receiptList.last.cumulativeGasUsed.longValue() else 0

    // 6. Set gasLimit
    val gasLimit = Account.GAS_LIMIT

    val block = AccountBlock.create(
      parentId,
      AccountBlock.ACCOUNT_BLOCK_VERSION,
      timestamp,
      mainchainBlockReferencesData,
      appliedTxList,
      mainchainHeaders,
      ommers,
      ownerPrivateKey,
      forgingStakeInfo,
      vrfProof,
      forgingStakeInfoMerklePath,
      feePaymentsHash,
      stateRoot,
      receiptsRoot,
      forgerAddress,
      baseFee,
      gasUsed,
      gasLimit,
      companion.asInstanceOf[SidechainAccountTransactionsCompanion])

    block

  }

  override def precalculateBlockHeaderSize(parentId: ModifierId,
                                           timestamp: Long,
                                           forgingStakeMerklePathInfo: ForgingStakeMerklePathInfo,
                                           vrfProof: VrfProof): Int = {
    // Create block header template, setting dummy values where it is possible.
    // Note:  AccountBlockHeader length is not constant because of the forgingStakeMerklePathInfo.merklePath.
    val header = AccountBlockHeader(
      AccountBlock.ACCOUNT_BLOCK_VERSION,
      parentId,
      timestamp,
      forgingStakeMerklePathInfo.forgingStakeInfo,
      forgingStakeMerklePathInfo.merklePath,
      vrfProof,
      new Array[Byte](MerkleTree.ROOT_HASH_LENGTH),
      new Array[Byte](MerkleTree.ROOT_HASH_LENGTH),
      new Array[Byte](MerkleTree.ROOT_HASH_LENGTH),
      new Array[Byte](MerkleTree.ROOT_HASH_LENGTH), // stateRoot TODO add constant
      new AddressProposition(new Array[Byte](Account.ADDRESS_SIZE)), // forgerAddress: PublicKeySecp256k1Proposition TODO add constant,
      Long.MaxValue,
      Long.MaxValue,
      Long.MaxValue,
      new Array[Byte](MerkleTree.ROOT_HASH_LENGTH),
      Long.MaxValue,
      new Array[Byte](NodeViewModifier.ModifierIdSize),
      new Signature25519(new Array[Byte](Signature25519.SIGNATURE_LENGTH)) // empty signature
    )

    header.bytes.length
  }

  override def collectTransactionsFromMemPool(nodeView: View, blockSizeIn: Int): Seq[SidechainTypes#SCAT] = {
    // no checks of the block size here, these txes are the candidates and their inclusion
    // will be attempted by forger
    nodeView.pool.take(nodeView.pool.size).toSeq
  }

  override def getOmmersSize(ommers: Seq[Ommer[AccountBlockHeader]]): Int = {
    val ommersSerializer = new ListSerializer[Ommer[AccountBlockHeader]](AccountOmmerSerializer)
    ommersSerializer.toBytes(ommers.asJava).length
  }

  // TODO the stateRoot of the genesis block
  val getGenesisBlockRootHash =
    new Array[Byte](32)

  def getStateRoot(history: AccountHistory, forgedBlockTimestamp: Long, branchPointInfo: BranchPointInfo): Array[Byte] = {
    // For given epoch N we should get data from the ending of the epoch N-2.
    // genesis block is the single and the last block of epoch 1 - that is a special case:
    // Data from epoch 1 is also valid for epoch 2, so for epoch N==2, we should get info from epoch 1.
    val parentId = branchPointInfo.branchPointId
    val lastBlockId = history.getLastBlockIdOfPrePreviousEpochs(forgedBlockTimestamp, parentId)
    val lastBlock = history.getBlockById(lastBlockId)
    lastBlock.get().header.stateRoot
  }

  override def getForgingStakeMerklePathInfo(nextConsensusEpochNumber: ConsensusEpochNumber, wallet: AccountWallet, history: AccountHistory, state: AccountState, branchPointInfo: BranchPointInfo, nextBlockTimestamp: Long): Seq[ForgingStakeMerklePathInfo] = {

    // 1. get from history the state root from the header of the block of 2 epochs before
    val stateRoot = getStateRoot(history, nextBlockTimestamp, branchPointInfo)

    // 2. get from stateDb using root above the collection of all forger stakes (ordered)
    val forgingStakeInfoSeq: Seq[ForgingStakeInfo] = using(state.getStateDbViewFromRoot(stateRoot)) { stateViewFromRoot =>
      stateViewFromRoot.getOrderedForgingStakeInfoSeq
    }

    // 3. using wallet secrets, filter out the not-mine forging stakes
    val secrets: Seq[Secret] = wallet.allSecrets().asScala

    val walletPubKeys = secrets.map(e => e.publicImage())

    val filteredForgingStakeInfoSeq = forgingStakeInfoSeq.filter(p => {
      walletPubKeys.contains(p.blockSignPublicKey) &&
        walletPubKeys.contains(p.vrfPublicKey)
    })

    // return an empty seq if we do not have forging stake, that is a legal (negative) result.
    if (filteredForgingStakeInfoSeq.isEmpty)
      return Seq()

    // 4. prepare merkle tree of all forger stakes and extract path info of mine (what is left after 3)
    val forgingStakeInfoTree = MerkleTree.createMerkleTree(forgingStakeInfoSeq.map(info => info.hash).asJava)
    val merkleTreeLeaves = forgingStakeInfoTree.leaves().asScala.map(leaf => new ByteArrayWrapper(leaf))

    // Calculate merkle path for all delegated forgerBoxes
    val forgingStakeMerklePathInfoSeq: Seq[ForgingStakeMerklePathInfo] =
      filteredForgingStakeInfoSeq.flatMap(forgingStakeInfo => {
        merkleTreeLeaves.indexOf(new ByteArrayWrapper(forgingStakeInfo.hash)) match {
          case -1 => None // May occur in case Wallet doesn't contain information about blockSignKey and vrfKey
          case index => Some(ForgingStakeMerklePathInfo(
            forgingStakeInfo,
            forgingStakeInfoTree.getMerklePathForLeaf(index)
          ))
        }
      })

    forgingStakeMerklePathInfoSeq
  }
}




