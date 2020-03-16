package com.horizen.consensus

import com.horizen.SidechainHistory
import com.horizen.block.{Ommer, SidechainBlock, SidechainBlockHeader}
import com.horizen.chain.SidechainBlockInfo
import com.horizen.fixtures.{CompanionsFixture, SidechainBlockFixture, TransactionFixture}
import com.horizen.params.{MainNetParams, NetworkParams}
import com.horizen.validation.ConsensusValidator
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatest.mockito.MockitoSugar
import org.junit.Assert.{assertEquals, assertFalse, assertTrue, fail => jFail}
import scorex.util.ModifierId

import scala.util.{Failure, Success, Try}

class ConsensusValidatorOmmersTest
  extends JUnitSuite
    with MockitoSugar
  with CompanionsFixture
  with TransactionFixture
  with SidechainBlockFixture{

  val consensusValidator = new ConsensusValidator {
    // always successful
    override def verifyVrf(history: SidechainHistory, header: SidechainBlockHeader, nonceInfo: NonceConsensusEpochInfo): Unit = {}
    // always successful
    override private[horizen] def verifyForgerBox(header: SidechainBlockHeader, stakeConsensusEpochInfo: StakeConsensusEpochInfo): Unit = {}
  }


  @Test
  def emptyOmmersValidation(): Unit = {
    // Mock history
    val history = mockHistory()

    // Mock block with no ommers
    val verifiedBlock: SidechainBlock = mock[SidechainBlock]
    Mockito.when(verifiedBlock.ommers).thenReturn(Seq())

    // Mock other data
    val verifiedBlockInfo: SidechainBlockInfo = mock[SidechainBlockInfo]
    val verifierBlockFullConsensusEpochInfo: FullConsensusEpochInfo = mock[FullConsensusEpochInfo]

    Try {
      consensusValidator.verifyOmmers(verifiedBlock, verifiedBlockInfo, verifierBlockFullConsensusEpochInfo, history)
    } match {
      case Success(_) =>
      case Failure(e) => throw e //jFail("Block with no ommers expected to be Valid.")
    }
  }

  @Test
  def ommersTimestampsValidation(): Unit = {
    // Mock history
    val history = mockHistory()
    // Mock other data
    val verifiedBlockInfo: SidechainBlockInfo = mock[SidechainBlockInfo]
    val verifierBlockFullConsensusEpochInfo = FullConsensusEpochInfo(mock[StakeConsensusEpochInfo], mock[NonceConsensusEpochInfo])

    // Mock ommers
    var ommers: Seq[Ommer] = Seq(
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 7)),
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 11)) // after verifiedBlock Slot
    )

    // Mock block with ommers
    val verifiedBlockTimestamp = history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 10)
    var verifiedBlock: SidechainBlock = mock[SidechainBlock]
    Mockito.when(verifiedBlock.timestamp).thenReturn(verifiedBlockTimestamp)
    Mockito.when(verifiedBlock.ommers).thenReturn(ommers)


    // Test 1: One of the ommers Slot is after verifiedBlock Slot
    Try {
      consensusValidator.verifyOmmers(verifiedBlock, verifiedBlockInfo, verifierBlockFullConsensusEpochInfo, history)
    } match {
      case Success(_) => jFail("Block with no ommers expected to be Invalid.")
      case Failure(_) =>
    }


    // Test 2: One of the ommers Slot is equal to verifiedBlock Slot
    ommers = Seq(
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 7)),
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 11)) // equals to verifiedBlock Slot
    )
    verifiedBlock = mock[SidechainBlock]
    Mockito.when(verifiedBlock.timestamp).thenReturn(verifiedBlockTimestamp)
    Mockito.when(verifiedBlock.ommers).thenReturn(ommers)

    Try {
      consensusValidator.verifyOmmers(verifiedBlock, verifiedBlockInfo, verifierBlockFullConsensusEpochInfo, history)
    } match {
      case Success(_) => jFail("Block with no ommers expected to be Invalid.")
      case Failure(_) =>
    }


    // Test 3: Ommers slot order is wrong
    ommers = Seq(
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 8)),
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 7)) // lower than previous Ommer slot
    )
    verifiedBlock = mock[SidechainBlock]
    Mockito.when(verifiedBlock.timestamp).thenReturn(verifiedBlockTimestamp)
    Mockito.when(verifiedBlock.ommers).thenReturn(ommers)

    Try {
      consensusValidator.verifyOmmers(verifiedBlock, verifiedBlockInfo, verifierBlockFullConsensusEpochInfo, history)
    } match {
      case Success(_) => jFail("Block with no wrong ommers slot order expected to be Invalid.")
      case Failure(_) =>
    }


    // Test 4: Ommers Epoch is 2 or more epoch before verifiedBlock Epoch
    ommers = Seq(
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 1, ConsensusSlotNumber @@ 20)),
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 1, ConsensusSlotNumber @@ 22)) // much lower than previous Ommer Epoch
    )
    verifiedBlock = mock[SidechainBlock]
    Mockito.when(verifiedBlock.timestamp).thenReturn(verifiedBlockTimestamp)
    Mockito.when(verifiedBlock.ommers).thenReturn(ommers)

    Try {
      consensusValidator.verifyOmmers(verifiedBlock, verifiedBlockInfo, verifierBlockFullConsensusEpochInfo, history)
    } match {
      case Success(_) => jFail("Block with no wrong ommers slot order expected to be Invalid.")
      case Failure(_) =>
    }
  }

  @Test
  def sameEpochOmmersValidation(): Unit = {
    // Mock history
    val history = mockHistory()
    // Mock other data
    val verifiedBlockInfo: SidechainBlockInfo = mock[SidechainBlockInfo]
    val verifierBlockFullConsensusEpochInfo = FullConsensusEpochInfo(mock[StakeConsensusEpochInfo], mock[NonceConsensusEpochInfo])

    // Test 1: Valid Ommers in correct order from the same epoch as VerifiedBlock
    // Mock ommers
    var ommers: Seq[Ommer] = Seq(
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 7)),
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 8))
    )

    // Mock block with ommers
    val verifiedBlockTimestamp = history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 10)
    var verifiedBlock: SidechainBlock = mock[SidechainBlock]
    Mockito.when(verifiedBlock.timestamp).thenReturn(verifiedBlockTimestamp)
    Mockito.when(verifiedBlock.ommers).thenReturn(ommers)

    Try {
      consensusValidator.verifyOmmers(verifiedBlock, verifiedBlockInfo, verifierBlockFullConsensusEpochInfo, history)
    } match {
      case Success(_) =>
      case Failure(e) => throw e //jFail("Block with no ommers expected to be Valid.")
    }


    // Test 2: Same as above, but Ommers contains invalid forger box data
    val fbException = new Exception("ForgerBoxExpection")
    val forgerBoxFailConsensusValidator = new ConsensusValidator {
      // always successful
      override def verifyVrf(history: SidechainHistory, header: SidechainBlockHeader, nonceInfo: NonceConsensusEpochInfo): Unit = {}
      // always fail
      override private[horizen] def verifyForgerBox(header: SidechainBlockHeader, stakeConsensusEpochInfo: StakeConsensusEpochInfo): Unit = throw fbException
    }

    Try {
      forgerBoxFailConsensusValidator.verifyOmmers(verifiedBlock, verifiedBlockInfo, verifierBlockFullConsensusEpochInfo, history)
    } match {
      case Success(_) => jFail("Block with no ommers expected to be invalid.")
      case Failure(e) => assertEquals("Different exception expected.", fbException, e)
    }


    // Test 3: Same as above, but Ommers contains invalid VRF data
    val vrfException = new Exception("VRFExpcetion")
    val vrfFailConsensusValidator = new ConsensusValidator {
      // always successful
      override def verifyVrf(history: SidechainHistory, header: SidechainBlockHeader, nonceInfo: NonceConsensusEpochInfo): Unit = throw vrfException
      // always fail
      override private[horizen] def verifyForgerBox(header: SidechainBlockHeader, stakeConsensusEpochInfo: StakeConsensusEpochInfo): Unit = {}
    }

    Try {
      vrfFailConsensusValidator.verifyOmmers(verifiedBlock, verifiedBlockInfo, verifierBlockFullConsensusEpochInfo, history)
    } match {
      case Success(_) => jFail("Block with no ommers expected to be invalid.")
      case Failure(e) => assertEquals("Different exception expected.", vrfException, e)
    }
  }

  @Test
  def previousEpochOmmersValidation(): Unit = {
    val verifiedBlockId: ModifierId = getRandomBlockId(1000L)
    val previousEpochId: ConsensusEpochId = blockIdToEpochId(getRandomBlockId(2000L))
    val previousEpochFullConsensusEpoch = FullConsensusEpochInfo(mock[StakeConsensusEpochInfo], mock[NonceConsensusEpochInfo])

    // Mock other data
    val verifiedBlockInfo: SidechainBlockInfo = mock[SidechainBlockInfo]
    val verifierBlockFullConsensusEpochInfo = FullConsensusEpochInfo(mock[StakeConsensusEpochInfo], mock[NonceConsensusEpochInfo])
    // Mock history
    val history = mockHistory()
    // Expect for verified block data as input
    Mockito.when(history.getPreviousConsensusEpochIdForBlock(ArgumentMatchers.any[ModifierId], ArgumentMatchers.any[SidechainBlockInfo])).thenAnswer(answer => {
      assertEquals("Block id is different. VerifiedBlock id expected.", verifiedBlockId, answer.getArgument(0))
      assertEquals("Block info is different. VerifiedBlock info expected.", verifiedBlockInfo, answer.getArgument(1))
      previousEpochId
    })
    // Expect for previous epoch last block id as input
    Mockito.when(history.blockInfoById(ArgumentMatchers.any[ModifierId])).thenAnswer(answer => {
      assertEquals("Block id is different. Previous epoch last block id expected.", lastBlockIdInEpochId(previousEpochId), answer.getArgument(0))
      mock[SidechainBlockInfo]
    })
    // Expect for previous epoch last block data info retrieving
    Mockito.when(history.getFullConsensusEpochInfoForBlock(ArgumentMatchers.any[ModifierId], ArgumentMatchers.any[SidechainBlockInfo])).thenAnswer(answer => {
      assertEquals("Block id is different. Previous epoch last block id expected.", lastBlockIdInEpochId(previousEpochId), answer.getArgument(0))
      previousEpochFullConsensusEpoch
    })

    // Test 1: Valid Ommers in correct order from the previous epoch as VerifiedBlock
    // Mock ommers
    val ommers: Seq[Ommer] = Seq(
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 2, ConsensusSlotNumber @@ 20)),
      getMockedOmmer(history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 2, ConsensusSlotNumber @@ 21))
    )

    // Mock block with ommers
    val verifiedBlockTimestamp = history.getTimeStampForEpochAndSlot(ConsensusEpochNumber @@ 3, ConsensusSlotNumber @@ 10)
    var verifiedBlock: SidechainBlock = mock[SidechainBlock]
    Mockito.when(verifiedBlock.id).thenReturn(verifiedBlockId)
    Mockito.when(verifiedBlock.timestamp).thenReturn(verifiedBlockTimestamp)
    Mockito.when(verifiedBlock.ommers).thenReturn(ommers)

    val previousEpochConsensusValidator = new ConsensusValidator {
      override def verifyVrf(history: SidechainHistory, header: SidechainBlockHeader, nonceInfo: NonceConsensusEpochInfo): Unit = {
        assertEquals("Different nonceInfo expected", previousEpochFullConsensusEpoch.nonceConsensusEpochInfo, nonceInfo)
      }
      override private[horizen] def verifyForgerBox(header: SidechainBlockHeader, stakeConsensusEpochInfo: StakeConsensusEpochInfo): Unit = {
        assertEquals("Different stakeConsensusEpochInfo expected", previousEpochFullConsensusEpoch.stakeConsensusEpochInfo, stakeConsensusEpochInfo)
      }
    }

    Try {
      previousEpochConsensusValidator.verifyOmmers(verifiedBlock, verifiedBlockInfo, verifierBlockFullConsensusEpochInfo, history)
    } match {
      case Success(_) =>
      case Failure(e) => throw e //jFail("Block with no ommers expected to be Valid.")
    }
  }

  @Test
  def switchingEpochOmmersValidation(): Unit = {
    // TODO: Case when some ommers are part of previous epoch, some - of current epoch
  }

  private def getMockedOmmer(timestamp: Long): Ommer = {
    val header = mock[SidechainBlockHeader]
    Mockito.when(header.timestamp).thenReturn(timestamp)

    Ommer(header, None, Seq(), Seq())
  }

  private def mockHistory(): SidechainHistory = {
    val params: NetworkParams = MainNetParams()
    // Because TimeToEpochSlotConverter is a trait, we need to do this dirty stuff to use its methods as a part of mocked SidechainHistory
    class TimeToEpochSlotConverterImpl(val params: NetworkParams) extends TimeToEpochSlotConverter
    val converter = new TimeToEpochSlotConverterImpl(params)

    val history: SidechainHistory = mock[SidechainHistory]
    Mockito.when(history.timeStampToEpochNumber(ArgumentMatchers.any[Long])).thenAnswer(answer => {
      converter.timeStampToEpochNumber(answer.getArgument(0))
    })

    Mockito.when(history.timeStampToSlotNumber(ArgumentMatchers.any[Long])).thenAnswer(answer => {
      converter.timeStampToSlotNumber(answer.getArgument(0))
    })

    Mockito.when(history.timeStampToAbsoluteSlotNumber(ArgumentMatchers.any[Long])).thenAnswer(answer => {
      converter.timeStampToAbsoluteSlotNumber(answer.getArgument(0))
    })

    Mockito.when(history.getTimeStampForEpochAndSlot(ArgumentMatchers.any[ConsensusEpochNumber], ArgumentMatchers.any[ConsensusSlotNumber])).thenAnswer(answer => {
      converter.getTimeStampForEpochAndSlot(answer.getArgument(0), answer.getArgument(1))
    })

    Mockito.when(history.params).thenReturn(params)

    history
  }
}
