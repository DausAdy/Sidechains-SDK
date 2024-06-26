package io.horizen.account.state.nativescdata.forgerstakev2

import com.google.common.primitives.Bytes
import io.horizen.account.network.{ForgerInfo, ForgerInfoSerializer}
import io.horizen.account.proposition.{AddressProposition, AddressPropositionSerializer}
import io.horizen.account.state.NativeSmartContractMsgProcessor.NULL_HEX_STRING_32
import io.horizen.account.state._
import io.horizen.account.state.nativescdata.forgerstakev2.StakeStorage.{ACCOUNT, ForgerKey}
import io.horizen.account.utils.WellKnownAddresses.FORGER_STAKE_V2_SMART_CONTRACT_ADDRESS
import io.horizen.account.utils.{BigIntegerUInt256, BigIntegerUtil, ZenWeiConverter}
import io.horizen.consensus.ForgingStakeInfo
import io.horizen.evm.Address
import io.horizen.proposition.{PublicKey25519Proposition, VrfPublicKey}
import io.horizen.utils.BytesUtils
import sparkz.core.serialization.{BytesSerializable, SparkzSerializer}
import sparkz.crypto.hash.Blake2b256
import sparkz.util.serialization.{Reader, Writer}

import java.math.BigInteger
import scala.collection.mutable.ListBuffer


object StakeStorage {

  val ACCOUNT: Address = FORGER_STAKE_V2_SMART_CONTRACT_ADDRESS

  val ActivationKey: Array[Byte] = Blake2b256.hash("Activate")

  def isActive(view: BaseAccountStateView): Boolean = {
    val activated = view.getAccountStorage(ACCOUNT, ActivationKey)
    new BigInteger(1, activated) == BigInteger.ONE
  }

  def setActive(view: BaseAccountStateView): Unit = {
    val activated = BigIntegerUtil.toUint256Bytes(BigInteger.ONE)
    view.updateAccountStorage(ACCOUNT, ActivationKey, activated)
  }


  def addForger(view: BaseAccountStateView,
                blockSignProposition: PublicKey25519Proposition,
                vrfPublicKey: VrfPublicKey,
                rewardShare: Int,
                rewardAddress: Address,
                epochNumber: Int,
                delegatorPublicKey: Address,
                stakedAmount: BigInteger): Unit = {

    val forgerKey = ForgerKey(blockSignProposition, vrfPublicKey)

    if (ForgerMap.existsForger(view, forgerKey))
      throw new ExecutionRevertedException(s"Forger already registered.")

    ForgerMap.addForger(view, forgerKey, blockSignProposition, vrfPublicKey, rewardShare, rewardAddress)

    val forgerHistory = ForgerStakeHistory(forgerKey)
    forgerHistory.addCheckpoint(view, epochNumber, stakedAmount)

    val delegatorChkSumAddress = DelegatorKey(delegatorPublicKey)
    val stakeHistory = StakeHistory(forgerKey, delegatorChkSumAddress)
    stakeHistory.addCheckpoint(view, epochNumber, stakedAmount)

    addNewDelegator(view, forgerKey, delegatorChkSumAddress)

  }

  def updateForger(view: BaseAccountStateView, blockSignProposition: PublicKey25519Proposition, vrfPublicKey: VrfPublicKey, rewardShare: Int, rewardAddress: Address): Unit = {
    val forgerKey = ForgerKey(blockSignProposition, vrfPublicKey)
    val forger = ForgerMap.getForgerOption(view, forgerKey).getOrElse(throw new ExecutionRevertedException("Forger doesn't exist."))
    if ((forger.rewardShare > 0) || (forger.rewardAddress.address() != Address.ZERO))
      throw new ExecutionRevertedException("Forger has already set reward share and reward address.")
    ForgerMap.updateForger(view, forgerKey, blockSignProposition, vrfPublicKey, rewardShare, rewardAddress)
  }

  private def addNewDelegator(view: BaseAccountStateView, forgerKey: ForgerKey, delegator: DelegatorKey): Unit = {
    val listOfDelegators = DelegatorList(forgerKey)
    listOfDelegators.addDelegator(view, delegator)

    val listOfForgers = DelegatorListOfForgerKeys(delegator)
    listOfForgers.addForgerKey(view, forgerKey)
  }

  def getPagedListOfForgers(view: BaseAccountStateView, startPos: Int, pageSize: Int): PagedForgersListResponse =
    ForgerMap.getPagedListOfForgers(view, startPos, pageSize)

  def getForger(view: BaseAccountStateView, signKey: PublicKey25519Proposition, vrfKey: VrfPublicKey): Option[ForgerInfo] = {
    val forgerKey = ForgerKey(signKey, vrfKey)
    ForgerMap.getForgerOption(view, forgerKey)
  }

  def addStake(view: BaseAccountStateView,
               signKey: PublicKey25519Proposition,
               vrfPublicKey: VrfPublicKey,
               epochNumber: Int,
               delegatorPublicKey: Address,
               stakedAmount: BigInteger): Unit = {

    val forgerKey = ForgerKey(signKey, vrfPublicKey)
    val forgerHistory = ForgerStakeHistory(forgerKey)
    val forgerHistorySize = forgerHistory.getSize(view)
    if (forgerHistorySize == 0)
      throw new ExecutionRevertedException(s"Forger doesn't exist.")

    val addToStake = (latestStake: BigInteger) => latestStake.add(stakedAmount)
    forgerHistory.updateOrAddCheckpoint(view, forgerHistorySize, epochNumber, addToStake)

    val delegatorChkSumAddress = DelegatorKey(delegatorPublicKey)
    val stakeHistory = StakeHistory(forgerKey, delegatorChkSumAddress)
    val stakeHistorySize = stakeHistory.getSize(view)
    stakeHistory.updateOrAddCheckpoint(view, stakeHistorySize, epochNumber, addToStake)
    if (stakeHistorySize == 0)
      addNewDelegator(view, forgerKey, delegatorChkSumAddress)
  }

  def removeStake(view: BaseAccountStateView,
                  blockSignProposition: PublicKey25519Proposition,
                  vrfPublicKey: VrfPublicKey,
                  epochNumber: Int,
                  delegatorPublicKey: Address,
                  stakedAmount: BigInteger): Unit = {
    val forgerKey = ForgerKey(blockSignProposition, vrfPublicKey)
    val forgerHistory = ForgerStakeHistory(forgerKey)
    val forgerHistorySize = forgerHistory.getSize(view)
    if (forgerHistorySize == 0)
      throw new ExecutionRevertedException("Forger doesn't exist.")
    val delegatorChkSumAddress = DelegatorKey(delegatorPublicKey)
    val stakeHistory = StakeHistory(forgerKey, delegatorChkSumAddress)
    val stakeHistorySize = stakeHistory.getSize(view)
    if (stakeHistorySize == 0)
      throw new ExecutionRevertedException(s"Delegator ${BytesUtils.toHexString(delegatorChkSumAddress.toBytes)} doesn't have stake with the forger.")


    def subtractStake(latestStake: BigInteger): BigInteger = {
      val newAmount = latestStake.subtract(stakedAmount)
      if (newAmount.signum() == -1)
        throw new ExecutionRevertedException(s"Not enough stake. Claimed $stakedAmount, available $latestStake")
      newAmount
    }

    stakeHistory.updateOrAddCheckpoint(view, stakeHistorySize, epochNumber, subtractStake)
    forgerHistory.updateOrAddCheckpoint(view, forgerHistorySize, epochNumber, subtractStake)
  }

  def getForgingStakes(view: BaseAccountStateView): Seq[ForgingStakeInfo] = {
    val listOfForgerKeys = ForgerMap.getForgerKeys(view)
    listOfForgerKeys.map { forgerKey =>
      val forger = ForgerMap.getForger(view, forgerKey)
      val amount = ForgerStakeHistory(forgerKey).getLatestAmount(view)
      ForgingStakeInfo(forger.forgerPublicKeys.blockSignPublicKey, forger.forgerPublicKeys.vrfPublicKey, ZenWeiConverter.convertWeiToZennies(amount))
    }
  }

  def getAllForgerStakes(view: BaseAccountStateView): Seq[ForgerStakeData] = {
    val listOfForgerKeys = ForgerMap.getForgerKeys(view)
    listOfForgerKeys.flatMap { forgerKey =>
      val forger = ForgerMap.getForger(view, forgerKey)
      val delegatorList = DelegatorList(forgerKey)
      val delegatorSize = delegatorList.getSize(view)
      val listOfStakes: ListBuffer[ForgerStakeData] = ListBuffer()
      for (idx <- 0 until delegatorSize) {
        val delegator = delegatorList.getDelegatorAt(view, idx)
        val stakeHistory = StakeHistory(forgerKey, delegator)
        val amount = stakeHistory.getLatestAmount(view)
        if (amount.signum() == 1) {
          listOfStakes.append(ForgerStakeData(forger.forgerPublicKeys, new AddressProposition(delegator), amount))
        }
      }
      listOfStakes
    }
  }

  def getStakeStart(view: BaseAccountStateView, forgerKeys: ForgerPublicKeys, delegator: Address): StakeStartCmdOutput = {
    val forgerKey = ForgerKey(forgerKeys.blockSignPublicKey, forgerKeys.vrfPublicKey)
    val delegatorKey = DelegatorKey(delegator)
    val stakeHistory = StakeHistory(forgerKey, delegatorKey)
    if (stakeHistory.getSize(view) == 0)
      StakeStartCmdOutput(-1)
    else
      StakeStartCmdOutput(stakeHistory.getCheckpoint(view, 0).fromEpochNumber)
  }

  def getStakeTotal(view: BaseAccountStateView,
                    forgerKeys: Option[ForgerPublicKeys],
                    delegator: Option[Address],
                    consensusEpochStart: Int,
                    consensusEpochEnd: Int): StakeTotalCmdOutput = {
    forgerKeys match {
      case Some(forgerKeys) =>
        val forgerKey = ForgerKey(forgerKeys.blockSignPublicKey, forgerKeys.vrfPublicKey)
        val history: BaseStakeHistory = delegator match {
          case Some(address) => StakeHistory(forgerKey, DelegatorKey(address))
          case None => ForgerStakeHistory(forgerKey)
        }
        StakeTotalCmdOutput(getForgerStakesPerEpoch(view, history, consensusEpochStart, consensusEpochEnd))

      case None =>
        // sum all forging stakes per epochs
        val totalStakesPerEpoch = ForgerMap.getForgerKeys(view).map(ForgerStakeHistory)
          .map(history => getForgerStakesPerEpoch(view, history, consensusEpochStart, consensusEpochEnd))
          .transpose
          .map(stakes => stakes.foldLeft(BigInteger.ZERO)((a, b) => a.add(b)))

        StakeTotalCmdOutput(totalStakesPerEpoch)
    }
  }

  private[forgerstakev2] def getForgerStakesPerEpoch(view: BaseAccountStateView, history: BaseStakeHistory, consensusEpochStart: Int, consensusEpochEnd: Int): Seq[BigInteger] = {
    // if requested slice completely before the first checkpoint
    if (history.getCheckpoint(view, 0).fromEpochNumber > consensusEpochEnd)
      return List.fill[BigInteger](consensusEpochEnd - consensusEpochStart + 1)(BigInteger.ZERO)

    var currIndex = checkpointBSearch(view, history, consensusEpochEnd)
    var currCheckpoint = history.getCheckpoint(view, currIndex)
    val result = ListBuffer[BigInteger]()
    for (currEpoch <- consensusEpochEnd to consensusEpochStart by -1) {
      result.prepend(currCheckpoint.stakedAmount)
      if (currCheckpoint.fromEpochNumber == currEpoch) {
        currIndex = currIndex - 1
        if (currIndex == -1)
          return List.fill[BigInteger](currEpoch - consensusEpochStart)(BigInteger.ZERO) ++ result
        if (currEpoch != consensusEpochStart)
          currCheckpoint = history.getCheckpoint(view, currIndex)
      }
    }
    result
  }

  /**
   * Binary search with optimization for most recent elements - prioritizes top 2√n elements.
   */
  private[forgerstakev2] def checkpointBSearch(view: BaseAccountStateView, history: BaseStakeHistory, epoch: Int): Int = {
    val length = history.getSize(view)
    var low = 0
    var high = length
    var mid = 0

    if (length > 5) {
      mid = high - Math.sqrt(high).round.toInt
      val checkpointEpochNumber = history.getCheckpoint(view, mid).fromEpochNumber
      if (checkpointEpochNumber == epoch) return mid
      else if (checkpointEpochNumber > epoch) high = mid
      else low = mid + 1
    }

    while (low < high) {
      mid = (high + low) / 2
      val checkpointEpochNumber = history.getCheckpoint(view, mid).fromEpochNumber
      if (checkpointEpochNumber == epoch) return mid
      else if (checkpointEpochNumber > epoch) high = mid
      else low = mid + 1
    }
    if (high == 0) 0 else high - 1
  }

  def getPagedForgersStakesByForger(view: BaseAccountStateView, forger: ForgerPublicKeys, startPos: Int, pageSize: Int): PagedStakesByForgerResponse = {
    if (startPos < 0)
      throw new IllegalArgumentException(s"Negative start position: $startPos can not be negative")
    if (pageSize <= 0)
      throw new IllegalArgumentException(s"Invalid page size $pageSize, must be positive")

    val forgerKey = ForgerKey(forger.blockSignPublicKey, forger.vrfPublicKey)
    val listOfDelegators = DelegatorList(forgerKey)
    val numOfDelegators = listOfDelegators.getSize(view)
    if (startPos == 0 && numOfDelegators == 0)
      return PagedStakesByForgerResponse(-1, Seq())

    if (startPos > numOfDelegators - 1)
      throw new IllegalArgumentException(s"Invalid start position reading list of delegators: $startPos, delegators array size: $numOfDelegators")

    var endPos = startPos + pageSize
    if (endPos > numOfDelegators)
      endPos = numOfDelegators

    val resultList = (startPos until endPos).view.map(index => {
      val delegatorKey = listOfDelegators.getDelegatorAt(view, index)
      val stakeHistory = StakeHistory(forgerKey, delegatorKey)
      val amount = stakeHistory.getLatestAmount(view)
      StakeDataDelegator(new AddressProposition(delegatorKey), amount)
    }).filter(_.stakedAmount.signum() > 0).toList

    if (endPos == numOfDelegators) {
      // tell the caller we are done with the array
      endPos = -1
    }

    PagedStakesByForgerResponse(endPos, resultList)
  }

  def getPagedForgersStakesByDelegator(view: BaseAccountStateView,  delegator: Address, startPos: Int, pageSize: Int): PagedStakesByDelegatorResponse = {
    if (startPos < 0)
      throw new IllegalArgumentException(s"Negative start position: $startPos")
    if (pageSize <= 0)
      throw new IllegalArgumentException(s"Invalid page size $pageSize, must be positive")

    val delegatorKey = DelegatorKey(delegator)
    val listOfForgers = DelegatorListOfForgerKeys(delegatorKey)
    val numOfForgers = listOfForgers.getSize(view)
    if (startPos == 0 && numOfForgers == 0)
      return PagedStakesByDelegatorResponse(-1, Seq())

    if (startPos > numOfForgers - 1)
      throw new IllegalArgumentException(s"Invalid start position reading list of forgers: $startPos, forgers array size: $numOfForgers")

    var endPos = startPos + pageSize
    if (endPos > numOfForgers)
      endPos = numOfForgers

    val resultList = (startPos until endPos).view.map(index => {
      val forgerKey = listOfForgers.getForgerKey(view, index)
      val forger = ForgerMap.getForgerOption(view, forgerKey).getOrElse(throw new ExecutionRevertedException("Forger doesn't exist."))
      val stakeHistory = StakeHistory(forgerKey, delegatorKey)
      val amount = stakeHistory.getLatestAmount(view)
      StakeDataForger(forger.forgerPublicKeys, amount)
    }).filter(_.stakedAmount.signum() > 0).toList

    if (endPos == numOfForgers) {
      // tell the caller we are done with the array
      endPos = -1
    }
    PagedStakesByDelegatorResponse(endPos, resultList)
  }


  class BaseStakeHistory(uid: Array[Byte])
    extends StateDbArray(ACCOUNT, Blake2b256.hash(Bytes.concat(uid, "History".getBytes("UTF-8")))) {

    def addCheckpoint(view: BaseAccountStateView, epoch: Int, stakeAmount: BigInteger): Unit = {
      val checkpoint = StakeCheckpoint(epoch, stakeAmount)
      append(view, checkpointToPaddedBytes(checkpoint))
    }

    def updateOrAddCheckpoint(view: BaseAccountStateView, historySize: Int, epoch: Int, op: BigInteger => BigInteger): Unit = {
      if (historySize > 0) {
        val lastElemIndex = historySize - 1
        val lastCheckpoint = getCheckpoint(view, lastElemIndex)
        val newAmount = op(lastCheckpoint.stakedAmount)
        if (lastCheckpoint.fromEpochNumber == epoch) {
          // Let's check if the newAmount is the same of the previous checkpoint. In that case, this last checkpoint
          // is removed.
          val secondLastCheckpointIdx = lastElemIndex - 1
          if (secondLastCheckpointIdx > -1 && getCheckpoint(view, secondLastCheckpointIdx).stakedAmount == newAmount) {
            // Rollback to second last checkpoint
            updateSize(view, historySize - 1)
            // TODO It is not necessary to remove the last element in the history, because it will be overwritten sooner or later.
            // However, it may save some gas. To be checked.
          }
          else {
            val checkpoint = StakeCheckpoint(epoch, newAmount)
            updateValue(view, lastElemIndex, checkpointToPaddedBytes(checkpoint))
          }
        }
        else if (lastCheckpoint.fromEpochNumber < epoch)
          addCheckpoint(view, epoch, newAmount)
        else
          throw new ExecutionRevertedException(s"Epoch is in the past: epoch $epoch, last epoch: ${lastCheckpoint.fromEpochNumber}")
      }
      else if (historySize == 0) {
        val newAmount = op(BigInteger.ZERO)
        addCheckpoint(view, epoch, newAmount)
      }
      else
        throw new IllegalArgumentException(s"Size cannot be negative: $historySize")
    }

    def getCheckpoint(view: BaseAccountStateView, index: Int): StakeCheckpoint = {
      val paddedValue = getValue(view, index)
      StakeCheckpointSerializer.parseBytes(paddedValue)
    }

    def getLatestAmount(view: BaseAccountStateView): BigInteger = {
      val size = getSize(view)
      if (size == 0)
        BigInteger.ZERO
      else {
        val paddedValue = getValue(view, size - 1)
        StakeCheckpointSerializer.parseBytes(paddedValue).stakedAmount
      }
    }

    private[horizen] def checkpointToPaddedBytes(checkpoint: StakeCheckpoint): Array[Byte] = {
      BytesUtils.padRightWithZeroBytes(StakeCheckpointSerializer.toBytes(checkpoint), 32)
    }
  }

  case class StakeHistory(forgerKey: ForgerKey, delegator: DelegatorKey)
    extends BaseStakeHistory(Blake2b256.hash(Bytes.concat(forgerKey.bytes, delegator.toBytes)))

  case class ForgerStakeHistory(forgerKey: ForgerKey) extends BaseStakeHistory(forgerKey.bytes)

  case class DelegatorList(forgerKey: ForgerKey)
    extends StateDbArray(ACCOUNT, Blake2b256.hash(Bytes.concat(forgerKey.bytes, "Delegators".getBytes("UTF-8")))) {

    def addDelegator(view: BaseAccountStateView, delegator: DelegatorKey): Unit = {
      append(view, BytesUtils.padRightWithZeroBytes(AddressPropositionSerializer.getSerializer.toBytes(new AddressProposition(delegator)), 32))
    }

    def getDelegatorAt(view: BaseAccountStateView, index: Int): DelegatorKey = {
      DelegatorKey(AddressPropositionSerializer.getSerializer.parseBytes(getValue(view, index)).address())
    }

  }

  case class DelegatorListOfForgerKeys(delegator: DelegatorKey)
    extends StateDbArray(ACCOUNT, Blake2b256.hash(Bytes.concat(delegator.toBytes, "Forgers".getBytes("UTF-8")))) {

    def addForgerKey(view: BaseAccountStateView, forgerKey: ForgerKey): Unit = {
      append(view, forgerKey.bytes)
    }

    def getForgerKey(view: BaseAccountStateView, idx: Int): ForgerKey = {
      ForgerKey(getValue(view, idx))
    }

  }

  case class DelegatorKey(address: Address)
    extends Address(new AddressProposition(address).checksumAddress())

  case class ForgerKey(bytes: Array[Byte]) {

    override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

    override def equals(obj: Any): Boolean = {
      obj match {
        case obj: ForgerKey => bytes.sameElements(obj.bytes)
        case _ => false
      }
    }

  }

  object ForgerKey {
    def apply(blockSignProposition: PublicKey25519Proposition, vrfPublicKey: VrfPublicKey): ForgerKey =
      ForgerKey(Blake2b256.hash(Bytes.concat(blockSignProposition.pubKeyBytes(), vrfPublicKey.pubKeyBytes())))
  }
}


case class StakeCheckpoint(
                            fromEpochNumber: Int,
                            stakedAmount: BigInteger)
  extends BytesSerializable {

  require(fromEpochNumber >= 0, s"Epoch cannot be a negative number. Passed value: $fromEpochNumber")
  require(stakedAmount.signum() != -1, s"Stake cannot be a negative number. Passed value: $stakedAmount")

  override type M = StakeCheckpoint

  override def serializer: SparkzSerializer[StakeCheckpoint] = StakeCheckpointSerializer

  override def toString: String = "%s(fromEpochNumber: %s, stakedAmount: %s)"
    .format(this.getClass.toString, fromEpochNumber, stakedAmount)

}

object StakeCheckpointSerializer extends SparkzSerializer[StakeCheckpoint] {

  override def serialize(s: StakeCheckpoint, w: Writer): Unit = {
    w.putInt(s.fromEpochNumber)
    w.putInt(s.stakedAmount.toByteArray.length)
    w.putBytes(s.stakedAmount.toByteArray)
  }

  override def parse(r: Reader): StakeCheckpoint = {
    val fromEpochNumber = r.getInt()
    val stakeAmountLength = r.getInt()
    val stakeAmount = new BigIntegerUInt256(r.getBytes(stakeAmountLength)).getBigInt
    StakeCheckpoint(fromEpochNumber, stakeAmount)
  }
}

object ForgerMap {
  private object ListOfForgers extends StateDbArray(ACCOUNT, "Forgers".getBytes("UTF-8")) {
    def getForgerKey(view: BaseAccountStateView, idx: Int): ForgerKey = {
      ForgerKey(getValue(view, idx))
    }

    def addForgerKey(view: BaseAccountStateView, forgerKey: ForgerKey): Int = {
      append(view, forgerKey.bytes)
    }

  }

  def existsForger(view: BaseAccountStateView, forgerKey: ForgerKey): Boolean = {
    val forgerData = view.getAccountStorage(ACCOUNT, forgerKey.bytes)
    // getting a not existing key from state DB using RAW strategy
    // gives an array of 32 bytes filled with 0, while using CHUNK strategy
    // gives an empty array instead
    !forgerData.sameElements(NULL_HEX_STRING_32)
  }

  def addForger(view: BaseAccountStateView,
                forgerKey: ForgerKey,
                blockSignProposition: PublicKey25519Proposition,
                vrfPublicKey: VrfPublicKey,
                rewardShare: Int,
                rewardAddress: Address,
               ): Unit = {
    ListOfForgers.addForgerKey(view, forgerKey)

    val forgerStakeData = ForgerInfo(
      ForgerPublicKeys(blockSignProposition, vrfPublicKey), rewardShare, new AddressProposition(rewardAddress))

    // store the forger stake data
    view.updateAccountStorageBytes(ACCOUNT, forgerKey.bytes,
      ForgerInfoSerializer.toBytes(forgerStakeData))
  }

  def updateForger(view: BaseAccountStateView,
                   forgerKey: ForgerKey,
                   blockSignProposition: PublicKey25519Proposition,
                   vrfPublicKey: VrfPublicKey,
                   rewardShare: Int,
                   rewardAddress: Address,
                  ): Unit = {

    val forgerStakeData = ForgerInfo(
      ForgerPublicKeys(blockSignProposition, vrfPublicKey), rewardShare, new AddressProposition(rewardAddress))

    // store the forger stake data
    view.updateAccountStorageBytes(ACCOUNT, forgerKey.bytes,
      ForgerInfoSerializer.toBytes(forgerStakeData))
  }


  def getSize(view: BaseAccountStateView): Int = ListOfForgers.getSize(view)

  def getForgerKeys(view: BaseAccountStateView): Seq[ForgerKey] = {
    val listSize = getSize(view)
    (0 until listSize).map(index => {
      ListOfForgers.getForgerKey(view, index)
    })
  }

  def getForgerOption(view: BaseAccountStateView, forgerKey: ForgerKey): Option[ForgerInfo] = {
    if (existsForger(view, forgerKey))
      Some(ForgerInfoSerializer.parseBytes(view.getAccountStorageBytes(ACCOUNT, forgerKey.bytes)))
    else
      None
  }

  def getForger(view: BaseAccountStateView, forgerKey: ForgerKey): ForgerInfo = {
    ForgerInfoSerializer.parseBytes(view.getAccountStorageBytes(ACCOUNT, forgerKey.bytes))
  }

  def getPagedListOfForgers(view: BaseAccountStateView, startPos: Int, pageSize: Int): PagedForgersListResponse = {

    if (startPos < 0)
      throw new ExecutionRevertedException(s"Invalid startPos input: $startPos can not be negative")
    if (pageSize <= 0)
      throw new ExecutionRevertedException(s"Invalid page size $pageSize, must be positive")

    val listSize = getSize(view)

    if (startPos == 0 && listSize == 0)
      return PagedForgersListResponse(-1, Seq.empty[ForgerInfo])

    if (startPos > listSize - 1)
      throw new ExecutionRevertedException(s"Invalid start position: $startPos, array size: $listSize")

    var endPos = startPos + pageSize
    if (endPos > listSize)
      endPos = listSize

    val listOfElems = (startPos until endPos).map(index => {
      val currentKey = ListOfForgers.getForgerKey(view, index)
      getForger(view, currentKey)
    })

    if (endPos == listSize) {
      // tell the caller we are done with the array
      endPos = -1
    }

    PagedForgersListResponse(endPos, listOfElems)
  }

}

case class PagedForgersListResponse(nextStartPos: Int, forgers: Seq[ForgerInfo])

case class PagedStakesByForgerResponse(nextStartPos: Int, stakesData: Seq[StakeDataDelegator])

case class PagedStakesByDelegatorResponse(nextStartPos: Int, stakesData: Seq[StakeDataForger])
