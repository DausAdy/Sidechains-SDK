package io.horizen.account.state

import com.google.common.primitives.Bytes
import io.horizen.account.fork.GasFeeFork.DefaultGasFeeFork
import io.horizen.account.fork.{Version1_3_0Fork, Version1_4_0Fork}
import io.horizen.account.network.{ForgerInfo, GetForgerOutputDecoder, PagedForgersOutputDecoder}
import io.horizen.account.proposition.AddressProposition
import io.horizen.account.secret.{PrivateKeySecp256k1, PrivateKeySecp256k1Creator}
import io.horizen.account.state.ForgerStakeMsgProcessor.{AddNewStakeCmd => AddNewStakeCmdV1, GetListOfForgersCmd => GetListOfForgersCmdV1}
import io.horizen.account.state.ForgerStakeV2MsgProcessor._
import io.horizen.account.state.nativescdata.forgerstakev2.RegisterOrUpdateForgerCmdInputDecoder.NULL_ADDRESS_WITH_PREFIX_HEX_STRING
import io.horizen.account.state.nativescdata.forgerstakev2.StakeStorage._
import io.horizen.account.state.nativescdata.forgerstakev2._
import io.horizen.account.state.nativescdata.forgerstakev2.events.{DelegateForgerStake, RegisterForger, UpdateForger, WithdrawForgerStake}
import io.horizen.account.state.receipt.EthereumConsensusDataLog
import io.horizen.account.utils.ZenWeiConverter
import io.horizen.consensus.intToConsensusEpochNumber
import io.horizen.evm.{Address, Hash}
import io.horizen.fixtures.StoreFixture
import io.horizen.fork.{ForkConfigurator, ForkManagerUtil, OptionalSidechainFork, SidechainForkConsensusEpoch}
import io.horizen.params.NetworkParams
import io.horizen.proof.{Signature25519, VrfProof}
import io.horizen.proposition.{PublicKey25519Proposition, VrfPublicKey}
import io.horizen.secret.{PrivateKey25519, PrivateKey25519Creator, VrfSecretKey}
import io.horizen.utils.{BytesUtils, Pair, ZenCoinsUtils}
import io.horizen.vrf.VrfGeneratedDataProvider
import org.junit.Assert._
import org.junit._
import org.mockito.Mockito.when
import org.mockito._
import org.scalatestplus.junit.JUnitSuite
import org.scalatestplus.mockito._
import org.web3j.abi.datatypes.Type
import org.web3j.abi.{FunctionReturnDecoder, TypeReference}
import org.web3j.crypto.Keys
import org.web3j.utils.Numeric.hexStringToByteArray
import sparkz.core.bytesToVersion
import sparkz.crypto.hash.Keccak256

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.util
import java.util.Optional
import scala.annotation.tailrec
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.language.implicitConversions

class ForgerStakeV2MsgProcessorTest
  extends JUnitSuite
    with MockitoSugar
    with MessageProcessorFixture
    with StoreFixture {

  val dummyBigInteger: BigInteger = BigInteger.ONE
  val negativeAmount: BigInteger = BigInteger.valueOf(-1)

  val invalidWeiAmount: BigInteger = new BigInteger("10000000001")
  val validWeiAmount: BigInteger = new BigInteger("10000000000")
  val minimumStakeWeiAmount: BigInteger = new BigInteger("10000000000000000000")
  val validStakeWeiAmount: BigInteger = minimumStakeWeiAmount.multiply(2)

  val mockNetworkParams: NetworkParams = mock[NetworkParams]
  val forgerStakeV2MessageProcessor: ForgerStakeV2MsgProcessor.type = ForgerStakeV2MsgProcessor
  val forgerStakeMessageProcessor: ForgerStakeMsgProcessor = ForgerStakeMsgProcessor(mockNetworkParams)

  /** short hand: forger state native contract address */
  val contractAddress: Address = forgerStakeV2MessageProcessor.contractAddress

  // create private/public key pair
  val privateKey: PrivateKeySecp256k1 = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest".getBytes(StandardCharsets.UTF_8))
  val ownerAddressProposition: AddressProposition = privateKey.publicImage()

  val RegisterForgerEventSig: Array[Byte] = getEventSignature("RegisterForger(address,bytes32,bytes32,bytes1,uint256,uint32,address)")
  val UpdateForgerEventSig: Array[Byte] = getEventSignature("UpdateForger(address,bytes32,bytes32,bytes1,uint32,address)")
  val DelegateForgerStakeEventSig: Array[Byte] = getEventSignature("DelegateForgerStake(address,bytes32,bytes32,bytes1,uint256)")
  val NumOfIndexedRegisterForgerEvtParams = 3
  val NumOfIndexedDelegateStakeEvtParams = 3
  val WithdrawForgerStakeEventSig: Array[Byte] = getEventSignature("WithdrawForgerStake(address,bytes32,bytes32,bytes1,uint256)")
  val NumOfIndexedRemoveForgerStakeEvtParams = 1
  val OpenForgerStakeListEventSig: Array[Byte] = getEventSignature("OpenForgerList(uint32,address,bytes32)")
  val NumOfIndexedOpenForgerStakeListEvtParams = 1
  val ActivateStakeV2EventSig: Array[Byte] = getEventSignature("ActivateStakeV2()")

  val scAddrStr1: String = "00C8F107a09cd4f463AFc2f1E6E5bF6022Ad4600"
  val scAddressObj1 = new Address("0x" + scAddrStr1)

  val V1_4_MOCK_FORK_POINT: Int = 300
  val V1_3_MOCK_FORK_POINT: Int = 200

  val blockContextForkV1_4 =  new BlockContext(
    Address.ZERO,
    0,
    0,
    DefaultGasFeeFork.blockGasLimit,
    0,
    V1_4_MOCK_FORK_POINT,
    0,
    1,
    MockedHistoryBlockHashProvider,
    Hash.ZERO
  )

  val blockContextForkV1_4_plus10 =  new BlockContext(
    Address.ZERO,
    0,
    0,
    DefaultGasFeeFork.blockGasLimit,
    0,
    V1_4_MOCK_FORK_POINT + 10,
    0,
    1,
    MockedHistoryBlockHashProvider,
    Hash.ZERO
  )

  val blockContextForkV1_3 =  new BlockContext(
    Address.ZERO,
    0,
    0,
    DefaultGasFeeFork.blockGasLimit,
    0,
    V1_3_MOCK_FORK_POINT,
    0,
    1,
    MockedHistoryBlockHashProvider,
    Hash.ZERO
  )


  class TestOptionalForkConfigurator extends ForkConfigurator {
    override val forkActivation: SidechainForkConsensusEpoch = SidechainForkConsensusEpoch(0, 0, 0)
    override def getOptionalSidechainForks: util.List[Pair[SidechainForkConsensusEpoch, OptionalSidechainFork]] =
      Seq[Pair[SidechainForkConsensusEpoch, OptionalSidechainFork]](
        new Pair(SidechainForkConsensusEpoch(V1_3_MOCK_FORK_POINT, V1_3_MOCK_FORK_POINT, V1_3_MOCK_FORK_POINT), Version1_3_0Fork(true)),
        new Pair(SidechainForkConsensusEpoch(V1_4_MOCK_FORK_POINT, V1_4_MOCK_FORK_POINT, V1_4_MOCK_FORK_POINT), Version1_4_0Fork(true)),
      ).asJava
  }


  @Before
  def init(): Unit = {
    ForkManagerUtil.initializeForkManager(new TestOptionalForkConfigurator, "regtest")
    // by default start with fork active
    Mockito.when(metadataStorageView.getConsensusEpochNumber).thenReturn(Option(intToConsensusEpochNumber(V1_4_MOCK_FORK_POINT)))
  }


  def getDefaultMessage(opCode: Array[Byte], arguments: Array[Byte], nonce: BigInteger, value: BigInteger = negativeAmount): Message = {
    val data = Bytes.concat(opCode, arguments)
    new Message(
      origin,
      Optional.of(contractAddress), // to
      dummyBigInteger, // gasPrice
      dummyBigInteger, // gasFeeCap
      dummyBigInteger, // gasTipCap
      dummyBigInteger, // gasLimit
      value,
      nonce,
      data,
      false)
  }

  def randomNonce: BigInteger = randomU256


  @Test
  def testInit(): Unit = {
    usingView(forgerStakeV2MessageProcessor) { view =>
      // we have to call init beforehand
      assertTrue(forgerStakeV2MessageProcessor.isForkActive(view.getConsensusEpochNumberAsInt))
      assertFalse(view.accountExists(contractAddress))
      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)
      assertTrue(view.accountExists(contractAddress))
      assertTrue(view.isSmartContractAccount(contractAddress))
      view.commit(bytesToVersion(getVersion.data()))
    }
  }

  @Test
  def testMethodIds(): Unit = {
    //The expected methodIds were calculated using this site: https://emn178.github.io/online-tools/keccak_256.html
    assertEquals("Wrong MethodId for RegisterForgerCmd", "408abed9", ForgerStakeV2MsgProcessor.RegisterForgerCmd)
    assertEquals("Wrong MethodId for UpdateForgerCmd", "baed8f01", ForgerStakeV2MsgProcessor.UpdateForgerCmd)
    assertEquals("Wrong MethodId for Delegatemd", "431abc18", ForgerStakeV2MsgProcessor.DelegateCmd)
    assertEquals("Wrong MethodId for WithdrawCmd", "5639b873", ForgerStakeV2MsgProcessor.WithdrawCmd)
    assertEquals("Wrong MethodId for StakeTotalCmd", "895117b1", ForgerStakeV2MsgProcessor.StakeTotalCmd)
    assertEquals("Wrong MethodId for GetPagedForgersStakesByForgerCmd", "23359a85", ForgerStakeV2MsgProcessor.GetPagedForgersStakesByForgerCmd)
    assertEquals("Wrong MethodId for GetPagedForgersStakesByDelegatorCmd", "e99e75ac", ForgerStakeV2MsgProcessor.GetPagedForgersStakesByDelegatorCmd)
    assertEquals("Wrong MethodId for ActivateCmd", "0f15f4c0", ForgerStakeV2MsgProcessor.ActivateCmd)
    assertEquals("Wrong MethodId for GetForgerCmd", "7d8589fd", ForgerStakeV2MsgProcessor.GetForgerCmd)
    assertEquals("Wrong MethodId for GetPagedForgersCmd", "c1bf3d56", ForgerStakeV2MsgProcessor.GetPagedForgersCmd)
    assertEquals("Wrong MethodId for GetCurrentConsensusEpochCmd", "cf94a955", ForgerStakeV2MsgProcessor.GetCurrentConsensusEpochCmd)
  }


  @Test
  def testInitBeforeFork(): Unit = {

    Mockito.when(metadataStorageView.getConsensusEpochNumber).thenReturn(
      Option(intToConsensusEpochNumber(V1_4_MOCK_FORK_POINT-1)))

    usingView(forgerStakeV2MessageProcessor) { view =>

      assertFalse(view.accountExists(contractAddress))
      assertFalse(forgerStakeV2MessageProcessor.initDone(view))

      assertFalse(forgerStakeV2MessageProcessor.isForkActive(view.getConsensusEpochNumberAsInt))

      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)

      // assert no initialization took place
      assertFalse(view.accountExists(contractAddress))
      assertFalse(forgerStakeV2MessageProcessor.initDone(view))
    }
  }


  @Test
  def testDoubleInit(): Unit = {

    usingView(forgerStakeV2MessageProcessor) { view =>

      assertTrue(forgerStakeV2MessageProcessor.isForkActive(view.getConsensusEpochNumberAsInt))

      assertFalse(view.accountExists(contractAddress))
      assertFalse(forgerStakeV2MessageProcessor.initDone(view))

      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)

      assertTrue(view.accountExists(contractAddress))
      assertTrue(forgerStakeV2MessageProcessor.initDone(view))

      view.commit(bytesToVersion(getVersion.data()))

      val ex = intercept[MessageProcessorInitializationException] {
        forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)
      }
      assertTrue(ex.getMessage.contains("already init"))
    }
  }


  @Test
  def testCanProcess(): Unit = {
    usingView(forgerStakeV2MessageProcessor) { view =>

      // assert no initialization took place yet
      assertFalse(view.accountExists(contractAddress))
      assertFalse(forgerStakeV2MessageProcessor.initDone(view))

      assertTrue(forgerStakeV2MessageProcessor.isForkActive(view.getConsensusEpochNumberAsInt))

      // correct contract address
      assertTrue(TestContext.canProcess(forgerStakeV2MessageProcessor, getMessage(forgerStakeV2MessageProcessor.contractAddress), view, view.getConsensusEpochNumberAsInt))

      // check initialization took place
      assertTrue(view.accountExists(contractAddress))
      assertTrue(view.isSmartContractAccount(contractAddress))
      assertFalse(view.isEoaAccount(contractAddress))

      // call a second time for checking it does not do init twice (would assert)
      assertTrue(TestContext.canProcess(forgerStakeV2MessageProcessor, getMessage(forgerStakeV2MessageProcessor.contractAddress), view, view.getConsensusEpochNumberAsInt))

      // wrong address
      assertFalse(TestContext.canProcess(forgerStakeV2MessageProcessor, getMessage(randomAddress), view, view.getConsensusEpochNumberAsInt))
      // contract deployment: to == null
      assertFalse(TestContext.canProcess(forgerStakeV2MessageProcessor, getMessage(null), view, view.getConsensusEpochNumberAsInt))

      view.commit(bytesToVersion(getVersion.data()))
    }
  }

  @Test
  def testCanNotProcessBeforeFork(): Unit = {

    Mockito.when(metadataStorageView.getConsensusEpochNumber).thenReturn(
      Option(intToConsensusEpochNumber(1)))

    usingView(forgerStakeV2MessageProcessor) { view =>

      // create sender account with some fund in it
      val initialAmount = BigInteger.valueOf(100).multiply(validWeiAmount)
      val txHash1 = Keccak256.hash("tx")
      view.setupTxContext(txHash1, 10)
      createSenderAccount(view, initialAmount, scAddressObj1)


      assertFalse(forgerStakeV2MessageProcessor.isForkActive(view.getConsensusEpochNumberAsInt))

      // correct contract address and message but fork not yet reached
      assertFalse(TestContext.canProcess(forgerStakeV2MessageProcessor, getMessage(forgerStakeV2MessageProcessor.contractAddress), view, view.getConsensusEpochNumberAsInt))

      // the init did not take place
      assertFalse(view.accountExists(contractAddress))
      assertFalse(forgerStakeV2MessageProcessor.initDone(view))

      view.commit(bytesToVersion(getVersion.data()))
    }
  }

  @Test
  def testActivateBase(): Unit = {

    val processors = Seq(forgerStakeV2MessageProcessor, forgerStakeMessageProcessor)
    usingView(processors) { view =>
      // Initialize old forger stake directly in V2. The upgrade is made automatically in the init, in this case.
      forgerStakeMessageProcessor.init(view, V1_3_MOCK_FORK_POINT)
      assertEquals(ForgerStakeStorageVersion.VERSION_2, ForgerStakeStorage.getStorageVersionFromDb(view))

      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)

      // create sender account with some fund in it
      val initialAmount = BigInteger.valueOf(10).multiply(ZenWeiConverter.MAX_MONEY_IN_WEI)
      createSenderAccount(view, initialAmount)

      val nonce = 0

      // Test "activate" before reaching the fork point. It should fail.

      var msg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(ActivateCmd), nonce, ownerAddressProposition.address())

      // should fail because, before Version 1.4 fork, ActivateCmd is not a valid function signature
      val blockContextBeforeFork = new BlockContext(
        Address.ZERO,
        0,
        0,
        DefaultGasFeeFork.blockGasLimit,
        0,
        V1_4_MOCK_FORK_POINT - 1,
        0,
        1,
        MockedHistoryBlockHashProvider,
        Hash.ZERO
      )

      var exc = intercept[ExecutionRevertedException] {
        assertGas(0, msg, view, forgerStakeV2MessageProcessor, blockContextBeforeFork)
      }
      assertTrue(exc.getMessage.contains("fork not active"))
      assertEquals(ForgerStakeStorageVersion.VERSION_2, ForgerStakeStorage.getStorageVersionFromDb(view))


      // Test after fork.

      //Setting the context
      val txHash1 = Keccak256.hash("first tx")
      view.setupTxContext(txHash1, 10)

      assertGasInterop(0, msg, view, processors, blockContextForkV1_4)

      // Checking log
      val listOfLogs = view.getLogs(txHash1)
      checkActivateEvents(listOfLogs)

      // Check that old forger stake message processor cannot be used anymore

      msg = getMessage(forgerStakeMessageProcessor.contractAddress, 0, BytesUtils.fromHexString(GetListOfForgersCmdV1), randomNonce)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeMessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      assertTrue(s"Wrong error message ${exc.getMessage}", exc.getMessage.contains("disabled"))


      // Negative tests
      msg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(ActivateCmd), nonce, ownerAddressProposition.address())
      // Check that it cannot be called twice
      exc = intercept[ExecutionRevertedException] {
        assertGasInterop(0, msg, view, processors, blockContextForkV1_4)
      }
      assertEquals(s"Forger stake V2 already activated", exc.getMessage)

      // Check that it is not payable
      val value = validWeiAmount
      msg = getMessage(
        contractAddress, value, BytesUtils.fromHexString(ActivateCmd), nonce, ownerAddressProposition.address())

      val excPayable = intercept[ExecutionRevertedException] {
        assertGasInterop(0, msg, view, processors, blockContextForkV1_4)
      }
      assertEquals("Call value must be zero", excPayable.getMessage)

       // try processing a msg with a trailing byte in the arguments
      val badData = new Array[Byte](1)
      val msgBad = getMessage(contractAddress, 0, BytesUtils.fromHexString(ActivateCmd) ++ badData, randomNonce)

      // should fail because input has a trailing byte
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msgBad, view, blockContextForkV1_4, _, view))
      }
      assertTrue(s"Wrong exc message: ${exc.getMessage}, expected:invalid msg data length", exc.getMessage.contains("invalid msg data length"))
      view.commit(bytesToVersion(getVersion.data()))
    }
  }

  @Test
  def testActivate(): Unit = {
    val processors = Seq(forgerStakeV2MessageProcessor, forgerStakeMessageProcessor)
    usingView(processors) { view =>

      forgerStakeMessageProcessor.init(view, V1_3_MOCK_FORK_POINT)

      // create sender account with some fund in it
      val initialAmount = ZenWeiConverter.MAX_MONEY_IN_WEI
      createSenderAccount(view, initialAmount)

      val listOfExpectedResults = (1 to 5).map {idx =>

        val blockSignerProposition = new PublicKey25519Proposition(BytesUtils.fromHexString(s"112233445566778811223344556677881122334455667788112233445566778$idx")) // 32 bytes
        val vrfPublicKey = new VrfPublicKey(BytesUtils.fromHexString(s"d6b775fd4cefc7446236683fdde9d0464bba43cc565fa066b0b3ed1b888b9d118$idx")) // 33 bytes

        Mockito.when(mockNetworkParams.restrictForgers).thenReturn(true)
        Mockito.when(mockNetworkParams.allowedForgersList).thenReturn(Seq((blockSignerProposition, vrfPublicKey)))

        // Create some stakes with old storage model
        val privateKey1: PrivateKeySecp256k1 = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest1".getBytes(StandardCharsets.UTF_8))
        val owner1: AddressProposition = privateKey1.publicImage()
        val amount1 = addStakesV2(view, blockSignerProposition, vrfPublicKey, owner1, 400, blockContextForkV1_3)

        val privateKey2: PrivateKeySecp256k1 = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest2".getBytes(StandardCharsets.UTF_8))
        val owner2: AddressProposition = privateKey2.publicImage()
        val amount2 = addStakesV2(view, blockSignerProposition, vrfPublicKey, owner2, 350, blockContextForkV1_3)

        val privateKey3: PrivateKeySecp256k1 = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest3".getBytes(StandardCharsets.UTF_8))
        val owner3: AddressProposition = privateKey3.publicImage()
        val amount3 = addStakesV2(view, blockSignerProposition, vrfPublicKey, owner3, 250, blockContextForkV1_3)
        val listOfStakes = (owner3, amount3) :: (owner2, amount2) :: (owner1, amount1) :: Nil
        (ForgerPublicKeys(blockSignerProposition, vrfPublicKey), listOfStakes)
      }


      // The balance of forgerStakeMessageProcessor corresponds to the total staked amount. Note that this is not always
      // true, e.g. a forward transfer can increase the balance.

      val forgerStakeBalanceBeforeActivate = view.getBalance(forgerStakeMessageProcessor.contractAddress)

      // Check that before activate the balance of ForgerStakeV2 is zero
      assertEquals(BigInteger.ZERO, view.getBalance(contractAddress))

      //Setting the context
      val txHash1 = Keccak256.hash("first tx")
      view.setupTxContext(txHash1, 10)

      val activateMsg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(ActivateCmd), randomNonce, ownerAddressProposition.address())
      assertGasInterop(0, activateMsg, view, processors, blockContextForkV1_4)

      val listOfStakes = StakeStorage.getAllForgerStakes(view)
      val expNumOfStakes = listOfExpectedResults.foldLeft(0){(sum, res) => sum + res._2.size }
      assertEquals(expNumOfStakes, listOfStakes.size)

      listOfExpectedResults.foreach{ case (forgerKeys, expListOfStakes) =>
        val forgerOpt = StakeStorage.getForger(view, forgerKeys.blockSignPublicKey, forgerKeys.vrfPublicKey)
        assertFalse(forgerOpt.isEmpty)
        assertEquals(forgerKeys.blockSignPublicKey, forgerOpt.get.forgerPublicKeys.blockSignPublicKey)
        assertEquals(forgerKeys.vrfPublicKey, forgerOpt.get.forgerPublicKeys.vrfPublicKey)
        assertEquals(0, forgerOpt.get.rewardShare)
        assertEquals(Address.ZERO, forgerOpt.get.rewardAddress.address())

        val forgerKey = ForgerKey(forgerKeys.blockSignPublicKey, forgerKeys.vrfPublicKey)
        val forgerHistory = ForgerStakeHistory(forgerKey)
        assertEquals(1, forgerHistory.getSize(view))
        assertEquals(blockContextForkV1_4.consensusEpochNumber, forgerHistory.getCheckpoint(view, 0).fromEpochNumber)
        assertEquals(expListOfStakes.foldLeft(BigInteger.ZERO){(sum, pair) => sum.add(pair._2)}, forgerHistory.getCheckpoint(view, 0).stakedAmount)

        val listOfDelegators = DelegatorList(forgerKey)
        assertEquals(expListOfStakes.size, listOfDelegators.getSize(view))

        expListOfStakes.foreach{ case (expDelegator, expAmount) =>
          val stake1 = listOfStakes.find(stake => (stake.ownerPublicKey == expDelegator) && (stake.forgerPublicKeys == forgerKeys))
          assertTrue(stake1.isDefined)
          assertEquals(expAmount, stake1.get.stakedAmount)
          assertEquals(forgerOpt.get.forgerPublicKeys, stake1.get.forgerPublicKeys)
          val stakeHistory = StakeHistory(forgerKey, DelegatorKey(expDelegator.address()))
          assertEquals(1, stakeHistory.getSize(view))
          assertEquals(blockContextForkV1_4.consensusEpochNumber, stakeHistory.getCheckpoint(view, 0).fromEpochNumber)
          assertEquals(expAmount, stakeHistory.getCheckpoint(view, 0).stakedAmount)

        }

      }

      // Checking log
      val listOfLogs = view.getLogs(txHash1)
      checkActivateEvents(listOfLogs)

      assertEquals(BigInteger.ZERO, view.getBalance(forgerStakeMessageProcessor.contractAddress))
      assertEquals(forgerStakeBalanceBeforeActivate, view.getBalance(contractAddress))

      view.commit(bytesToVersion(getVersion.data()))

    }
  }

  def getBlockContextForEpoch(epochNum: Int): BlockContext = new BlockContext(
    Address.ZERO,
    0,
    0,
    DefaultGasFeeFork.blockGasLimit,
    0,
    epochNum,
    0,
    1,
    MockedHistoryBlockHashProvider,
    Hash.ZERO
  )


  @Test
  def testRegisterAndUpdateForger(): Unit = {

    val processors = Seq(forgerStakeV2MessageProcessor, forgerStakeMessageProcessor)

    usingView(processors) { view =>
      forgerStakeMessageProcessor.init(view, V1_3_MOCK_FORK_POINT)
      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)

      // create sender account with some fund in it
      val initialAmount = BigInteger.valueOf(100).multiply(validStakeWeiAmount)
      val senderAddress = new Address("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      createSenderAccount(view, initialAmount, inAddress = senderAddress)

      //Setting the context for forger 1
      val pair25519_f1 = get25519KeyPair(1)
      val pairVrf_f1 = getVrfKeyPair(1)
      val blockSignerProposition_f1 = pair25519_f1._2
      val vrfPublicKey_f1 = pairVrf_f1._2
      val rewardShare_f1: Int = 0
      val rewardAddress_f1 = new AddressProposition(hexStringToByteArray(NULL_ADDRESS_WITH_PREFIX_HEX_STRING))
      val msg_f1 = ForgerStakeV2MsgProcessor.getHashedMessageToSign(
        BytesUtils.toHexString(pair25519_f1._2.pubKeyBytes()),
        BytesUtils.toHexString(pairVrf_f1._2.pubKeyBytes()),
        rewardShare_f1,
        Keys.toChecksumAddress(BytesUtils.toHexString(rewardAddress_f1.address().toBytes)))
      val (signature25519, signatureVrf) = getSignatures(pair25519_f1._1, pairVrf_f1._1, msg_f1)

      /////////////////////////////////////////////////////////////////////////////////////////////
      //  Before activate tests
      /////////////////////////////////////////////////////////////////////////////////////////////

      var regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_f1, rewardAddress_f1.address(), signature25519, signatureVrf
      )

      var registerForgerData: Array[Byte] = BytesUtils.fromHexString(RegisterForgerCmd) ++ regCmdInput.encode()
      var msg = getMessage(contractAddress, validStakeWeiAmount, registerForgerData, randomNonce, from = senderAddress)

      // Check that register forger cannot be called before activate

      var exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      var expectedErr = "Forger stake V2 has not been activated yet"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Call activate
      val initialOwnerBalance = BigInteger.valueOf(200).multiply(validStakeWeiAmount)
      createSenderAccount(view, initialOwnerBalance, ownerAddressProposition.address())

      val activateMsg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(ActivateCmd), randomNonce, ownerAddressProposition.address())
      assertGasInterop(0, activateMsg, view, processors, blockContextForkV1_4)

      // first negative tests for testing the signature validity and stake amount
      //------------------------------------------------------------------
      // Try register with an invalid signature 25519. It should fail.
      val signature25519Bad: Signature25519 = new Signature25519(BytesUtils.fromHexString("074c8f9c17a54ffc661376b5cd8baf7fbcdfc009f5b8106c14bcf022214ad0db164e5fbbb6e1f6d5b44945c81ed6d113fcf58caec47adc7e4cf84a2070416c09"))

      regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_f1, rewardAddress_f1.address(), signature25519Bad, signatureVrf
      )
      registerForgerData = BytesUtils.fromHexString(RegisterForgerCmd) ++ regCmdInput.encode()
      msg = getMessage(contractAddress, validStakeWeiAmount, registerForgerData, randomNonce, from = senderAddress)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Invalid signature, could not validate against blockSignerProposition"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Try register with an invalid signature vrf. It should fail.
      val signatureVrfBad: VrfProof = new VrfProof(BytesUtils.fromHexString("03380183fea2c1d43a064cfeda6e4bc92ae5ab855a2388606b3b9d9f4dc9b90d8014eb09085d22f03c0c7fdd7b9864fcb5c3b31b187281a9eefccc98ce4b0c69008222de8501b929dc1d08f67c29033ac352671e11d4e8037cf192f05cbe584d24"))

      regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_f1, rewardAddress_f1.address(), signature25519, signatureVrfBad
      )
      registerForgerData = BytesUtils.fromHexString(RegisterForgerCmd) ++ regCmdInput.encode()
      msg = getMessage(contractAddress, validStakeWeiAmount, registerForgerData, randomNonce, from = senderAddress)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Invalid signature, could not validate against vrfKey"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Try register with too low a stake amount. It should fail.
      regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_f1, rewardAddress_f1.address(), signature25519, signatureVrf
      )
      registerForgerData = BytesUtils.fromHexString(RegisterForgerCmd) ++ regCmdInput.encode()
      msg = getMessage(contractAddress, validWeiAmount, registerForgerData, randomNonce, from = senderAddress)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "is below the minimum stake amount threshold"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Try register with an illegal stake amount. It should fail.
      regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_f1, rewardAddress_f1.address(), signature25519, signatureVrf
      )
      registerForgerData = BytesUtils.fromHexString(RegisterForgerCmd) ++ regCmdInput.encode()
      msg = getMessage(contractAddress, validStakeWeiAmount.add(BigInteger.ONE), registerForgerData, randomNonce, from = senderAddress)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "is not a legal wei amount"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))


      // verify we can register a forger after the activation with the proper parameters

      val initialSenderBalance = view.getBalance(senderAddress)
      val initialNscBalance = view.getBalance(contractAddress)

      val txHash_f1 = Keccak256.hash("first forger tx")
      view.setupTxContext(txHash_f1, 10)

      regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_f1, rewardAddress_f1.address(), signature25519, signatureVrf
      )
      registerForgerData = BytesUtils.fromHexString(RegisterForgerCmd) ++ regCmdInput.encode()
      msg = getMessage(contractAddress, validStakeWeiAmount, registerForgerData, randomNonce, from = senderAddress)
      assertGas(294999, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

      // Check log event
      val listOfLogs_f1 = view.getLogs(txHash_f1)
      assertEquals("Wrong number of logs", 1, listOfLogs_f1.length)
      val expectedEvent_f1 = RegisterForger(msg.getFrom, regCmdInput.forgerPublicKeys.blockSignPublicKey,
        regCmdInput.forgerPublicKeys.vrfPublicKey, validStakeWeiAmount, rewardShare_f1, rewardAddress_f1.address())

      checkRegisterForgerEvent(expectedEvent_f1, listOfLogs_f1(0))

      // check balances
      val finaleSenderBalance = view.getBalance(senderAddress)
      val finalNscBalance = view.getBalance(contractAddress)

      assertEquals(initialNscBalance.add(validStakeWeiAmount), finalNscBalance)
      assertEquals(initialSenderBalance.subtract(validStakeWeiAmount), finaleSenderBalance)

      // Negative tests
      // -------------------------------------------------------------------------------
      // Try register the same forger twice. It should fail.
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Can not register an already existing forger"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Try register with an inconsistent reward share and reward address. It should fail.
      var rewardShareTest = 1

      regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShareTest, rewardAddress_f1.address(), signature25519, signatureVrf
      )
      registerForgerData = BytesUtils.fromHexString(RegisterForgerCmd) ++ regCmdInput.encode()
      msg = getMessage(contractAddress, validStakeWeiAmount, registerForgerData, randomNonce, from = senderAddress)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Reward share cannot be different from 0 if reward address is not defined"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Try register with an inconsistent reward share and reward address. It should fail.
      rewardShareTest = 0
      val smartContractAddressTest = new AddressProposition(hexStringToByteArray("0011223344556677889900112233445566778899"))
      regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_f1, smartContractAddressTest.address(), signature25519, signatureVrf
      )
      registerForgerData = BytesUtils.fromHexString(RegisterForgerCmd) ++ regCmdInput.encode()
      msg = getMessage(contractAddress, validStakeWeiAmount, registerForgerData, randomNonce, from = senderAddress)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Reward share cannot be 0 if reward address is defined"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // we guard the validity range of reward share in the RegisterForgerCmdInput ctor
      rewardShareTest = -1
      var exc2 = intercept[IllegalArgumentException] {
        RegisterOrUpdateForgerCmdInput(
          ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShareTest, rewardAddress_f1.address(), signature25519, signatureVrf
        )
      }
      expectedErr = "reward share expected to be non negative"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc2.getMessage}", exc2.getMessage.contains(expectedErr))

      rewardShareTest = 1001
      exc2 = intercept[IllegalArgumentException] {
        RegisterOrUpdateForgerCmdInput(
          ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShareTest, rewardAddress_f1.address(), signature25519, signatureVrf
        )
      }
      expectedErr = "reward share expected to be 1000 at most"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc2.getMessage}", exc2.getMessage.contains(expectedErr))

      // Try register from a sender with not enough funds. It should fail.
      rewardShareTest = 0
      regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_f1, rewardAddress_f1.address(), signature25519, signatureVrf
      )
      registerForgerData = BytesUtils.fromHexString(RegisterForgerCmd) ++ regCmdInput.encode()
      msg = getMessage(contractAddress, validStakeWeiAmount, registerForgerData, randomNonce, from = origin)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Not enough balance"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))


      //Setting the context for forger 2
      val pair25519_f2 = get25519KeyPair(2)
      val pairVrf_f2 = getVrfKeyPair(2)
      val blockSignerProposition_f2 = pair25519_f2._2
      val vrfPublicKey_f2 = pairVrf_f2._2
      val rewardShare_f2: Int = 123
      val rewardAddress_f2 = new AddressProposition(hexStringToByteArray("ca12fcb886cbf73a39d87aac9610f8a303536642"))
      val msg_f2 = ForgerStakeV2MsgProcessor.getHashedMessageToSign(
        BytesUtils.toHexString(pair25519_f2._2.pubKeyBytes()),
        BytesUtils.toHexString(pairVrf_f2._2.pubKeyBytes()),
        rewardShare_f2,
        Keys.toChecksumAddress(BytesUtils.toHexString(rewardAddress_f2.address().toBytes)))
      val (signature25519_f2, signatureVrf_f2) = getSignatures(pair25519_f2._1, pairVrf_f2._1, msg_f2)


      val txHash_f2 = Keccak256.hash("second forger tx")
      view.setupTxContext(txHash_f2, 11)

      // add one more forger 2
      regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f2, vrfPublicKey_f2), rewardShare_f2, rewardAddress_f2.address(), signature25519_f2, signatureVrf_f2
      )
      registerForgerData = BytesUtils.fromHexString(RegisterForgerCmd) ++ regCmdInput.encode()
      msg = getMessage(contractAddress, validStakeWeiAmount, registerForgerData, randomNonce, from = senderAddress)
      assertGas(275099, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

      // Check log event
      val listOfLogs_f2 = view.getLogs(txHash_f2)
      assertEquals("Wrong number of logs", 1, listOfLogs_f2.length)
      val expectedEvent_f2 = RegisterForger(msg.getFrom, regCmdInput.forgerPublicKeys.blockSignPublicKey,
        regCmdInput.forgerPublicKeys.vrfPublicKey, validStakeWeiAmount, rewardShare_f2, rewardAddress_f2.address())

      checkRegisterForgerEvent(expectedEvent_f2, listOfLogs_f2(0))

      // Try getForger, with first forger
      var getForgerCmdInput = SelectByForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1)
      )

      var getForgerData: Array[Byte] = BytesUtils.fromHexString(GetForgerCmd) ++ getForgerCmdInput.encode()
      var msgGetForger = getMessage(contractAddress, BigInteger.ZERO,  getForgerData, randomNonce)
      val res1 = assertGas(10600, msgGetForger, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

      var getForgerOutput = GetForgerOutputDecoder.decode(res1)
      assertEquals(getForgerCmdInput.forgerPublicKeys, getForgerOutput.forgerPublicKeys)
      assertEquals(0, getForgerOutput.rewardShare)
      assertEquals(Address.ZERO, getForgerOutput.rewardAddress.address())

      // Try getForger, with second forger
      getForgerCmdInput = SelectByForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f2, vrfPublicKey_f2)
      )

      getForgerData = BytesUtils.fromHexString(GetForgerCmd) ++ getForgerCmdInput.encode()
      msgGetForger = getMessage(contractAddress, BigInteger.ZERO,  getForgerData, randomNonce)
      val res2 = assertGas(10600, msgGetForger, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

      getForgerOutput = GetForgerOutputDecoder.decode(res2)
      assertEquals(getForgerCmdInput.forgerPublicKeys, getForgerOutput.forgerPublicKeys)
      assertEquals(rewardShare_f2, getForgerOutput.rewardShare)
      assertEquals(rewardAddress_f2.address(), getForgerOutput.rewardAddress.address())

      // forger update
      //---------------------------------------
      val rewardShare_update: Int = 33
      val reward_address_update = new AddressProposition(hexStringToByteArray("3333333333333333333333333333333333333333"))

      // update first forger
      // - Try updating a forger setting an invalid reward share (null)
      var rewardShare_update_bad: Int = 0
      var msg_u1_bad = ForgerStakeV2MsgProcessor.getHashedMessageToSign(
        BytesUtils.toHexString(pair25519_f1._2.pubKeyBytes()),
        BytesUtils.toHexString(pairVrf_f1._2.pubKeyBytes()),
        rewardShare_update_bad,
        Keys.toChecksumAddress(BytesUtils.toHexString(reward_address_update.address().toBytes)))
      var (signature25519_u1_bad, signatureVrf_u1_bad) = getSignatures(pair25519_f1._1, pairVrf_f1._1, msg_u1_bad)
      var regCmdInput_bad = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_update_bad, reward_address_update.address(), signature25519_u1_bad, signatureVrf_u1_bad
      )
      var updateForgerData_bad = BytesUtils.fromHexString(UpdateForgerCmd) ++ regCmdInput_bad.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, updateForgerData_bad, randomNonce, from = senderAddress)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Illegal reward share value"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // - Try updating a forger setting an invalid reward share (too big)
      rewardShare_update_bad = 1001
      msg_u1_bad = ForgerStakeV2MsgProcessor.getHashedMessageToSign(
        BytesUtils.toHexString(pair25519_f1._2.pubKeyBytes()),
        BytesUtils.toHexString(pairVrf_f1._2.pubKeyBytes()),
        rewardShare_update_bad,
        Keys.toChecksumAddress(BytesUtils.toHexString(reward_address_update.address().toBytes)))
      var sigs_tuple = getSignatures(pair25519_f1._1, pairVrf_f1._1, msg_u1_bad)
      signature25519_u1_bad = sigs_tuple._1
      signatureVrf_u1_bad = sigs_tuple._2
      var exc_ill = intercept[IllegalArgumentException] {
        RegisterOrUpdateForgerCmdInput(
          ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_update_bad, reward_address_update.address(), signature25519_u1_bad, signatureVrf_u1_bad
        )
      }
      expectedErr = "reward share expected to be 1000 at most"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc_ill.getMessage.contains(expectedErr))

      // - Try updating a forger setting an invalid reward share (negative)
      rewardShare_update_bad = -10
      msg_u1_bad = ForgerStakeV2MsgProcessor.getHashedMessageToSign(
        BytesUtils.toHexString(pair25519_f1._2.pubKeyBytes()),
        BytesUtils.toHexString(pairVrf_f1._2.pubKeyBytes()),
        rewardShare_update_bad,
        Keys.toChecksumAddress(BytesUtils.toHexString(reward_address_update.address().toBytes)))
      sigs_tuple = getSignatures(pair25519_f1._1, pairVrf_f1._1, msg_u1_bad)
      signature25519_u1_bad = sigs_tuple._1
      signatureVrf_u1_bad = sigs_tuple._2
      exc_ill = intercept[IllegalArgumentException] {
        RegisterOrUpdateForgerCmdInput(
          ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_update_bad, reward_address_update.address(), signature25519_u1_bad, signatureVrf_u1_bad
        )
      }
      expectedErr = "reward share expected to be non negative"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc_ill.getMessage.contains(expectedErr))

      // - Try updating a forger setting an invalid reward address (null)
      val reward_address_update_bad = new AddressProposition(hexStringToByteArray("0000000000000000000000000000000000000000"))
      msg_u1_bad = ForgerStakeV2MsgProcessor.getHashedMessageToSign(
        BytesUtils.toHexString(pair25519_f1._2.pubKeyBytes()),
        BytesUtils.toHexString(pairVrf_f1._2.pubKeyBytes()),
        rewardShare_update,
        Keys.toChecksumAddress(BytesUtils.toHexString(reward_address_update_bad.address().toBytes)))
      sigs_tuple = getSignatures(pair25519_f1._1, pairVrf_f1._1, msg_u1_bad)
      signature25519_u1_bad = sigs_tuple._1
      signatureVrf_u1_bad = sigs_tuple._2
      regCmdInput_bad = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_update, reward_address_update_bad.address(), signature25519_u1_bad, signatureVrf_u1_bad
      )
      updateForgerData_bad = BytesUtils.fromHexString(UpdateForgerCmd) ++ regCmdInput_bad.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, updateForgerData_bad, randomNonce, from = senderAddress)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Reward address cannot be the ZERO address"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // - Try updating a forger that does not exist
      msg_u1_bad = ForgerStakeV2MsgProcessor.getHashedMessageToSign(
        BytesUtils.toHexString(pair25519_f1._2.pubKeyBytes()),
        BytesUtils.toHexString(pairVrf_f2._2.pubKeyBytes()), // mix of f1/f2 keys
        rewardShare_update,
        Keys.toChecksumAddress(BytesUtils.toHexString(reward_address_update.address().toBytes)))
      sigs_tuple = getSignatures(pair25519_f1._1, pairVrf_f1._1, msg_u1_bad)
      signature25519_u1_bad = sigs_tuple._1
      signatureVrf_u1_bad = sigs_tuple._2
      regCmdInput_bad = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f2), rewardShare_update, reward_address_update.address(), signature25519_u1_bad, signatureVrf_u1_bad
      )
      updateForgerData_bad = BytesUtils.fromHexString(UpdateForgerCmd) ++ regCmdInput_bad.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, updateForgerData_bad, randomNonce, from = senderAddress)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Forger does not exist"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      val msg_u1 = ForgerStakeV2MsgProcessor.getHashedMessageToSign(
        BytesUtils.toHexString(pair25519_f1._2.pubKeyBytes()),
        BytesUtils.toHexString(pairVrf_f1._2.pubKeyBytes()),
        rewardShare_update,
        Keys.toChecksumAddress(BytesUtils.toHexString(reward_address_update.address().toBytes)))
      val (signature25519_u1, signatureVrf_u1) = getSignatures(pair25519_f1._1, pairVrf_f1._1, msg_u1)

      val txHash_u1 = Keccak256.hash("update1 tx")
      view.setupTxContext(txHash_u1, 20)

      regCmdInput = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f1, vrfPublicKey_f1), rewardShare_update, reward_address_update.address(), signature25519_u1, signatureVrf_u1
      )
      val updateForgerData = BytesUtils.fromHexString(UpdateForgerCmd) ++ regCmdInput.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, updateForgerData, randomNonce, from = senderAddress)
      assertGas(34143, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

      // Check log event
      val listOfLogs2 = view.getLogs(txHash_u1)
      assertEquals("Wrong number of logs", 1, listOfLogs2.length)
      val expectedEvent2 = UpdateForger(msg.getFrom, regCmdInput.forgerPublicKeys.blockSignPublicKey,
        regCmdInput.forgerPublicKeys.vrfPublicKey, rewardShare_update, reward_address_update.address())

      checkUpdateForgerEvent(expectedEvent2, listOfLogs2(0))


      // negative test
      // - Try updating a forger which already has a reward share/address
      val msg_u2_bad = ForgerStakeV2MsgProcessor.getHashedMessageToSign(
        BytesUtils.toHexString(pair25519_f2._2.pubKeyBytes()),
        BytesUtils.toHexString(pairVrf_f2._2.pubKeyBytes()),
        rewardShare_update,
        Keys.toChecksumAddress(BytesUtils.toHexString(reward_address_update.address().toBytes)))
      sigs_tuple = getSignatures(pair25519_f2._1, pairVrf_f2._1, msg_u2_bad)
      val signature25519_u2_bad = sigs_tuple._1
      val signatureVrf_u2_bad = sigs_tuple._2
      regCmdInput_bad = RegisterOrUpdateForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition_f2, vrfPublicKey_f2), rewardShare_update, reward_address_update.address(), signature25519_u2_bad, signatureVrf_u2_bad
      )
      updateForgerData_bad = BytesUtils.fromHexString(UpdateForgerCmd) ++ regCmdInput_bad.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, updateForgerData_bad, randomNonce, from = senderAddress)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Reward share or reward address are not null"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

    }
  }

  def getVrfKeyPair(seed: Int): (VrfSecretKey, VrfPublicKey) = {
    val secret: VrfSecretKey = VrfGeneratedDataProvider.getVrfSecretKey(seed)
    val publicKey: VrfPublicKey = secret.publicImage()
    (secret, publicKey)
  }

  def get25519KeyPair(seed: Int): (PrivateKey25519, PublicKey25519Proposition) = {
    val secret: PrivateKey25519 = PrivateKey25519Creator.getInstance.generateSecret(BigInteger.valueOf(seed).toByteArray)
    val publicKey: PublicKey25519Proposition = secret.publicImage()
    (secret, publicKey)
  }

  def getSignatures(blockSignKey: PrivateKey25519, vrfKey: VrfSecretKey, msg: Array[Byte]): (Signature25519, VrfProof)  = {
    (blockSignKey.sign(msg), vrfKey.sign(msg))
  }

  @Test
  def testSignatures() {
    val generatedDataSeed = 908

    val pairVrfKey: (VrfSecretKey, VrfPublicKey) = getVrfKeyPair(generatedDataSeed)
    val pair25519: (PrivateKey25519, PublicKey25519Proposition) = get25519KeyPair(generatedDataSeed)

    val msg = ForgerStakeV2MsgProcessor.getHashedMessageToSign(
      pair25519._2.pubKeyBytes().toString, pairVrfKey._2.pubKeyBytes().toString, 0, "ab")

    val (signature25519, signatureVrf) = getSignatures(pair25519._1, pairVrfKey._1, msg)

    assertTrue(signatureVrf.isValid(pairVrfKey._2, msg))
    assertTrue(signature25519.isValid(pair25519._2, msg))
  }

  @Test
  def testAddAndRemoveStake(): Unit = {

    val processors = Seq(forgerStakeV2MessageProcessor, forgerStakeMessageProcessor)

    usingView(processors) { view =>
      forgerStakeMessageProcessor.init(view, V1_3_MOCK_FORK_POINT)
      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)

      // create sender account with some fund in it
      val initialAmount = BigInteger.valueOf(100).multiply(validWeiAmount)
      createSenderAccount(view, initialAmount)

      //Setting the context

      val blockSignerProposition = new PublicKey25519Proposition(BytesUtils.fromHexString("1122334455667788112233445566778811223344556677881122334455667788")) // 32 bytes
      val vrfPublicKey = new VrfPublicKey(BytesUtils.fromHexString("d6b775fd4cefc7446236683fdde9d0464bba43cc565fa066b0b3ed1b888b9d1180")) // 33 bytes

      /////////////////////////////////////////////////////////////////////////////////////////////
      //  Before activate tests
      /////////////////////////////////////////////////////////////////////////////////////////////

      val delegateCmdInput = SelectByForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey)
      )

      val delegateData: Array[Byte] = BytesUtils.fromHexString(DelegateCmd) ++ delegateCmdInput.encode()
      var msg = getMessage(contractAddress, validWeiAmount,  delegateData, randomNonce)

      // Check that delegate and withdraw cannot be called before activate

      var exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      var expectedErr = "Forger stake V2 has not been activated yet"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      val withdrawCmdInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey),
        BigInteger.ONE
      )

      msg = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(WithdrawCmd) ++ withdrawCmdInput.encode(), randomNonce)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      assertTrue(exc.getMessage.contains("Forger stake V2 has not been activated yet"))

      // Call activate

      val initialOwnerBalance = BigInteger.valueOf(200).multiply(validWeiAmount)
      createSenderAccount(view, initialOwnerBalance, ownerAddressProposition.address())

      msg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(ActivateCmd), randomNonce, ownerAddressProposition.address())

      assertGasInterop(0, msg, view, processors, blockContextForkV1_4)

      /////////////////////////////////////////////////////////////////////////////////////////////
      //  Delegate tests
      /////////////////////////////////////////////////////////////////////////////////////////////

      // Try delegate to a non-existing forger. It should fail.
      msg = getMessage(contractAddress, validWeiAmount, delegateData, randomNonce)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Forger doesn't exist."
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Register the forger
      val initialStake = new BigInteger("1000000000000")

      StakeStorage.addForger(view, delegateCmdInput.forgerPublicKeys.blockSignPublicKey, delegateCmdInput.forgerPublicKeys.vrfPublicKey,
        0, Address.ZERO, blockContextForkV1_4.consensusEpochNumber, ownerAddressProposition.address(), initialStake)

      //TODO we're using directly StakeStorage.addForger here because registerForger is not implemented yet. So we need
      // to update the contract balance by hand with the initial stake
      view.addBalance(contractAddress, initialStake)

      // Add the first stake by the same delegator
      val txHash1 = Keccak256.hash("first tx")
      view.setupTxContext(txHash1, 10)

      var stakeAmount = validWeiAmount
      msg = getMessage(contractAddress, stakeAmount, delegateData, randomNonce, ownerAddressProposition.address())
      assertGas(13587, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

      var listOfStakes = StakeStorage.getAllForgerStakes(view)
      assertEquals(1, listOfStakes.size)
      assertEquals(delegateCmdInput.forgerPublicKeys, listOfStakes.head.forgerPublicKeys)
      assertEquals(ownerAddressProposition, listOfStakes.head.ownerPublicKey)
      var expectedOwnerStakeAmount = initialStake.add(stakeAmount)
      assertEquals(expectedOwnerStakeAmount, listOfStakes.head.stakedAmount)

      // Check that the balances of the delegator and of the forger smart contract have changed
      var expectedOwnerBalance = initialOwnerBalance.subtract(stakeAmount)
      assertEquals(expectedOwnerBalance, view.getBalance(ownerAddressProposition.address()))
      var expectedForgerContractBalance = initialStake.add(stakeAmount)
      assertEquals(expectedForgerContractBalance, view.getBalance(contractAddress))

      // Check log event
      var listOfLogs = view.getLogs(txHash1)
      assertEquals("Wrong number of logs", 1, listOfLogs.length)
      var expectedDelegateEvent = DelegateForgerStake(ownerAddressProposition.address(), delegateCmdInput.forgerPublicKeys.blockSignPublicKey,
        delegateCmdInput.forgerPublicKeys.vrfPublicKey, stakeAmount)

      checkDelegateForgerStakeEvent(expectedDelegateEvent, listOfLogs(0))

      // Add with same delegator but different epoch
      val txHash2 = Keccak256.hash("tx2")
      view.setupTxContext(txHash2, 10)
      stakeAmount = validWeiAmount.multiply(2)

      var blockContext = getBlockContextForEpoch(V1_4_MOCK_FORK_POINT + 1)


      msg = getMessage(contractAddress, stakeAmount, delegateData, randomNonce, ownerAddressProposition.address())
      assertGas(57787, msg, view, forgerStakeV2MessageProcessor, blockContext)

      listOfStakes = StakeStorage.getAllForgerStakes(view)
      assertEquals(1, listOfStakes.size)
      assertEquals(delegateCmdInput.forgerPublicKeys, listOfStakes.head.forgerPublicKeys)
      assertEquals(ownerAddressProposition, listOfStakes.head.ownerPublicKey)
      expectedOwnerStakeAmount = expectedOwnerStakeAmount.add(stakeAmount)
      assertEquals(expectedOwnerStakeAmount, listOfStakes.head.stakedAmount)

      // Check that the balances of the delegator and of the forger smart contract have changed
      expectedOwnerBalance = expectedOwnerBalance.subtract(stakeAmount)
      assertEquals(expectedOwnerBalance, view.getBalance(ownerAddressProposition.address()))

      expectedForgerContractBalance = expectedForgerContractBalance.add(stakeAmount)
      assertEquals(expectedForgerContractBalance, view.getBalance(contractAddress))

      // Check log event
      listOfLogs = view.getLogs(txHash2)
      assertEquals("Wrong number of logs", 1, listOfLogs.length)
      expectedDelegateEvent = DelegateForgerStake(ownerAddressProposition.address(), delegateCmdInput.forgerPublicKeys.blockSignPublicKey,
        delegateCmdInput.forgerPublicKeys.vrfPublicKey, stakeAmount)

      checkDelegateForgerStakeEvent(expectedDelegateEvent, listOfLogs(0))

      // Add with different delegator but same epoch
      val privateKey1: PrivateKeySecp256k1 = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest1".getBytes(StandardCharsets.UTF_8))
      val owner1: AddressProposition = privateKey1.publicImage()

      val initialOwner1Balance = BigInteger.valueOf(200).multiply(validWeiAmount)
      createSenderAccount(view, initialOwner1Balance, owner1.address())

      val txHash3 = Keccak256.hash("tx3")
      view.setupTxContext(txHash3, 10)
      stakeAmount = validWeiAmount.multiply(3)

      msg = getMessage(contractAddress, stakeAmount, delegateData, randomNonce, owner1.address())
      assertGas(124087, msg, view, forgerStakeV2MessageProcessor, blockContext)

      listOfStakes = StakeStorage.getAllForgerStakes(view)
      assertEquals(2, listOfStakes.size)
      assertEquals(delegateCmdInput.forgerPublicKeys, listOfStakes.head.forgerPublicKeys)
      assertEquals(ownerAddressProposition, listOfStakes.head.ownerPublicKey)
      assertEquals(expectedOwnerStakeAmount, listOfStakes.head.stakedAmount)
      assertEquals(owner1, listOfStakes(1).ownerPublicKey)
      val expectedOwner1StakeAmount = stakeAmount
      assertEquals(expectedOwner1StakeAmount, listOfStakes(1).stakedAmount)

      // Check that the balances of the delegator and of the forger smart contract have changed
      assertEquals(expectedOwnerBalance, view.getBalance(ownerAddressProposition.address()))
      expectedForgerContractBalance = expectedForgerContractBalance.add(stakeAmount)
      assertEquals(expectedForgerContractBalance, view.getBalance(contractAddress))

      var expectedOwner1Balance = initialOwner1Balance.subtract(stakeAmount)
      assertEquals(expectedOwner1Balance, view.getBalance(owner1.address()))

      // Check log event
      listOfLogs = view.getLogs(txHash3)
      assertEquals("Wrong number of logs", 1, listOfLogs.length)
      expectedDelegateEvent = DelegateForgerStake(owner1.address(), delegateCmdInput.forgerPublicKeys.blockSignPublicKey,
        delegateCmdInput.forgerPublicKeys.vrfPublicKey, stakeAmount)
      checkDelegateForgerStakeEvent(expectedDelegateEvent, listOfLogs(0))


      // Add with different delegator and different epoch
      val txHash4 = Keccak256.hash("tx4")
      view.setupTxContext(txHash4, 10)
      stakeAmount = validWeiAmount

      val privateKey2: PrivateKeySecp256k1 = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest2".getBytes(StandardCharsets.UTF_8))
      val owner2: AddressProposition = privateKey2.publicImage()

      blockContext = getBlockContextForEpoch(blockContext.consensusEpochNumber + 1)
      val initialOwner2Balance = BigInteger.valueOf(300).multiply(validWeiAmount)
      createSenderAccount(view, initialOwner2Balance, owner2.address())

      msg = getMessage(contractAddress, stakeAmount, delegateData, randomNonce, owner2.address())
      assertGas(144087, msg, view, forgerStakeV2MessageProcessor, blockContext)

      listOfStakes = StakeStorage.getAllForgerStakes(view)
      assertEquals(3, listOfStakes.size)
      assertEquals(delegateCmdInput.forgerPublicKeys, listOfStakes.head.forgerPublicKeys)
      assertEquals(ownerAddressProposition, listOfStakes.head.ownerPublicKey)
      assertEquals(expectedOwnerStakeAmount, listOfStakes.head.stakedAmount)
      assertEquals(owner1, listOfStakes(1).ownerPublicKey)
      assertEquals(expectedOwner1StakeAmount, listOfStakes(1).stakedAmount)
      assertEquals(owner2, listOfStakes(2).ownerPublicKey)
      val expectedOwner2StakeAmount = stakeAmount
      assertEquals(expectedOwner2StakeAmount, listOfStakes(2).stakedAmount)

      // Check that the balances of the delegators and of the forger smart contract have changed
      assertEquals(expectedOwnerBalance, view.getBalance(ownerAddressProposition.address()))
      expectedForgerContractBalance = expectedForgerContractBalance.add(stakeAmount)
      assertEquals(expectedForgerContractBalance, view.getBalance(contractAddress))
      assertEquals(expectedOwner1Balance, view.getBalance(owner1.address()))

      var expectedOwner2Balance = initialOwner2Balance.subtract(stakeAmount)
      assertEquals(expectedOwner2Balance, view.getBalance(owner2.address()))

      // Check log event
      listOfLogs = view.getLogs(txHash4)
      assertEquals("Wrong number of logs", 1, listOfLogs.length)
      expectedDelegateEvent = DelegateForgerStake(owner2.address(), delegateCmdInput.forgerPublicKeys.blockSignPublicKey,
        delegateCmdInput.forgerPublicKeys.vrfPublicKey, stakeAmount)
      checkDelegateForgerStakeEvent(expectedDelegateEvent, listOfLogs(0))

      //////////////////////////////////////////////////////////
      // Negative tests
      //////////////////////////////////////////////////////////

      // Add stake without enough balance

      val privateKey3: PrivateKeySecp256k1 = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest3".getBytes(StandardCharsets.UTF_8))
      val owner3: AddressProposition = privateKey3.publicImage()
      assertEquals(BigInteger.ZERO, view.getBalance(owner3.address()))

      msg = getMessage(contractAddress, validWeiAmount, delegateData, randomNonce, owner3.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(2300, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Insufficient funds."
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Add Stake with value 0.

      msg = getMessage(contractAddress, BigInteger.ZERO, delegateData, randomNonce, owner2.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(2100, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Value must not be zero"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Add Stake with invalid zen amount.

      msg = getMessage(contractAddress, invalidWeiAmount, delegateData, randomNonce, owner2.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(2100, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Value is not a legal wei amount"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Add Stake in a epoch in the past.

      blockContext = getBlockContextForEpoch(blockContext.consensusEpochNumber - 1)
      msg = getMessage(contractAddress, validWeiAmount, delegateData, randomNonce, owner2.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(6400, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Epoch is in the past"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // try processing a msg with a trailing byte in the arguments
      val badData = Bytes.concat(delegateData, new Array[Byte](1))
      val msgBad = getMessage(contractAddress, stakeAmount, badData, randomNonce)

      // should fail because input has a trailing byte
      blockContext = getBlockContextForEpoch(blockContext.consensusEpochNumber + 10)
      val ex = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msgBad, view, blockContext, _, view))
      }
      assertTrue(ex.getMessage.contains("Wrong message data field length"))

      //////////////////////////////////////////////////////////
      // Withdrawal tests
      //////////////////////////////////////////////////////////

      // remove all the stakes for owner2
      stakeAmount = expectedOwner2StakeAmount
      val withdrawCmdBytes: Array[Byte] = BytesUtils.fromHexString(WithdrawCmd)
      var withdrawInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey), stakeAmount
      )
      val txHash5 = Keccak256.hash("tx5")
      view.setupTxContext(txHash5, 10)

      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, owner2.address())
      assertGas(57687, msg, view, forgerStakeV2MessageProcessor, blockContext)

      listOfStakes = StakeStorage.getAllForgerStakes(view)
      assertEquals(2, listOfStakes.size)
      assertEquals(delegateCmdInput.forgerPublicKeys, listOfStakes.head.forgerPublicKeys)
      assertEquals(ownerAddressProposition, listOfStakes.head.ownerPublicKey)
      assertEquals(expectedOwnerStakeAmount, listOfStakes.head.stakedAmount)
      assertEquals(owner1, listOfStakes(1).ownerPublicKey)
      assertEquals(expectedOwner1StakeAmount, listOfStakes(1).stakedAmount)

      assertEquals(expectedOwnerBalance, view.getBalance(ownerAddressProposition.address()))
      expectedForgerContractBalance = expectedForgerContractBalance.subtract(stakeAmount)
      assertEquals(expectedForgerContractBalance, view.getBalance(contractAddress))
      assertEquals(expectedOwner1Balance, view.getBalance(owner1.address()))

      expectedOwner2Balance = expectedOwner2Balance.add(stakeAmount)
      assertEquals(expectedOwner2Balance, view.getBalance(owner2.address()))

      // Check log event
      listOfLogs = view.getLogs(txHash5)
      assertEquals("Wrong number of logs", 1, listOfLogs.length)
      var expectedWithdrawEvent = WithdrawForgerStake(owner2.address(), withdrawInput.forgerPublicKeys.blockSignPublicKey,
        withdrawInput.forgerPublicKeys.vrfPublicKey, stakeAmount)
      checkWithdrawForgerStakeEvent(expectedWithdrawEvent, listOfLogs(0))

      // remove all the stakes for ownerAddressProposition in 2 different epochs
      stakeAmount = initialStake

      withdrawInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey), stakeAmount
      )
      val txHash6 = Keccak256.hash("tx6")
      view.setupTxContext(txHash6, 10)

      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, ownerAddressProposition.address())
      assertGas(37687, msg, view, forgerStakeV2MessageProcessor, blockContext)

      listOfStakes = StakeStorage.getAllForgerStakes(view)
      assertEquals(2, listOfStakes.size)
      assertEquals(delegateCmdInput.forgerPublicKeys, listOfStakes.head.forgerPublicKeys)
      assertEquals(ownerAddressProposition, listOfStakes.head.ownerPublicKey)
      expectedOwnerStakeAmount = expectedOwnerStakeAmount.subtract(stakeAmount)
      assertEquals(expectedOwnerStakeAmount, listOfStakes.head.stakedAmount)

      assertEquals(owner1, listOfStakes(1).ownerPublicKey)
      assertEquals(expectedOwner1StakeAmount, listOfStakes(1).stakedAmount)

      expectedOwnerBalance = expectedOwnerBalance.add(stakeAmount)
      assertEquals(expectedOwnerBalance, view.getBalance(ownerAddressProposition.address()))
      expectedForgerContractBalance = expectedForgerContractBalance.subtract(stakeAmount)
      assertEquals(expectedForgerContractBalance, view.getBalance(contractAddress))
      assertEquals(expectedOwner1Balance, view.getBalance(owner1.address()))


      // Check log event
      listOfLogs = view.getLogs(txHash6)
      assertEquals("Wrong number of logs", 1, listOfLogs.length)
      expectedWithdrawEvent = WithdrawForgerStake(ownerAddressProposition.address(), withdrawInput.forgerPublicKeys.blockSignPublicKey,
        withdrawInput.forgerPublicKeys.vrfPublicKey, stakeAmount)
      checkWithdrawForgerStakeEvent(expectedWithdrawEvent, listOfLogs(0))

      blockContext = getBlockContextForEpoch(blockContext.consensusEpochNumber + 1)

      stakeAmount = expectedOwnerStakeAmount
      withdrawInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey), stakeAmount
      )
      val txHash7 = Keccak256.hash("tx7")
      view.setupTxContext(txHash7, 10)

      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, ownerAddressProposition.address())
      assertGas(57687, msg, view, forgerStakeV2MessageProcessor, blockContext)

      listOfStakes = StakeStorage.getAllForgerStakes(view)
      assertEquals(1, listOfStakes.size)
      assertEquals(delegateCmdInput.forgerPublicKeys, listOfStakes.head.forgerPublicKeys)

      assertEquals(owner1, listOfStakes.head.ownerPublicKey)
      assertEquals(expectedOwner1StakeAmount, listOfStakes.head.stakedAmount)

      expectedOwnerBalance = expectedOwnerBalance.add(stakeAmount)
      assertEquals(expectedOwnerBalance, view.getBalance(ownerAddressProposition.address()))
      expectedForgerContractBalance = expectedForgerContractBalance.subtract(stakeAmount)
      assertEquals(expectedForgerContractBalance, view.getBalance(contractAddress))
      assertEquals(expectedOwner1Balance, view.getBalance(owner1.address()))


      // Check log event
      listOfLogs = view.getLogs(txHash7)
      assertEquals("Wrong number of logs", 1, listOfLogs.length)
      expectedWithdrawEvent = WithdrawForgerStake(ownerAddressProposition.address(), withdrawInput.forgerPublicKeys.blockSignPublicKey,
        withdrawInput.forgerPublicKeys.vrfPublicKey, stakeAmount)
      checkWithdrawForgerStakeEvent(expectedWithdrawEvent, listOfLogs(0))


      //////////////////////////////////////////////////////////
      // Negative tests
      //////////////////////////////////////////////////////////

      // Check that it is not payable
      msg = getMessage(contractAddress, stakeAmount, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, owner1.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(0, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Call value must be zero"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Invalid withdrawal amount: 0, invalid wei amount
      withdrawInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey), BigInteger.ZERO
      )
      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, owner1.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(2100, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Withdrawal amount must be greater than zero"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      withdrawInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey), invalidWeiAmount
      )
      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, owner1.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(2100, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Value is not a legal wei amount"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Wrong input: try processing a msg with a trailing byte in the arguments
      withdrawInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey), stakeAmount
      )
      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode() ++ new Array[Byte](1), randomNonce, owner1.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(2100, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Wrong message data field length"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Remove stake without any stake. 3 tests: delegator that hasn't ever delegate something to the forger, delegator
      // who withdrew all its stakes and delegator who tries to withdraw an amount greater than its stakes.
      withdrawInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey), stakeAmount
      )

      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, owner3.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(6300, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "doesn't have stake with the forger"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, ownerAddressProposition.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(8400, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Not enough stake"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      withdrawInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey), expectedOwner1StakeAmount.add(stakeAmount)
      )

      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, owner1.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(8400, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Not enough stake"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Remove stake from a non-existing forger.
      val vrfPublicKey2 = new VrfPublicKey(BytesUtils.fromHexString("22222222222222446236683fdde9d0464bba43cc565fa066b0b3ed1b888b9d1180")) // 33 bytes

      withdrawInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey2), stakeAmount
      )

      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, owner1.address())
      exc = intercept[ExecutionRevertedException] {
        assertGas(4200, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Forger doesn't exist."
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Remove stakes in the past
      val revert = view.snapshot
      withdrawInput = WithdrawCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey), expectedOwner1StakeAmount
      )
      msg = getMessage(contractAddress, BigInteger.ZERO, withdrawCmdBytes ++ withdrawInput.encode(), randomNonce, owner1.address())
      blockContext = getBlockContextForEpoch(blockContext.consensusEpochNumber - 1)

      exc = intercept[ExecutionRevertedException] {
        assertGas(32800, msg, view, forgerStakeV2MessageProcessor, blockContext)
      }
      expectedErr = "Epoch is in the past"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))
      view.revertToSnapshot(revert)

      ///////////////////////////////////////////////////////////////////////////////////
      // Remove all the remaining stakes
      ///////////////////////////////////////////////////////////////////////////////////
      val txHash8 = Keccak256.hash("tx8")
      view.setupTxContext(txHash8, 10)
      blockContext = getBlockContextForEpoch(blockContext.consensusEpochNumber + 10)
      assertGas(57687, msg, view, forgerStakeV2MessageProcessor, blockContext)

      listOfStakes = StakeStorage.getAllForgerStakes(view)
      assertEquals(0, listOfStakes.size)

      assertEquals(BigInteger.ZERO, view.getBalance(contractAddress))
      expectedOwner1Balance = expectedOwner1Balance.add(withdrawInput.value)
      assertEquals(expectedOwner1Balance, view.getBalance(owner1.address()))

      // Check log event
      listOfLogs = view.getLogs(txHash8)
      assertEquals("Wrong number of logs", 1, listOfLogs.length)
      expectedWithdrawEvent = WithdrawForgerStake(owner1.address(), withdrawInput.forgerPublicKeys.blockSignPublicKey,
        withdrawInput.forgerPublicKeys.vrfPublicKey, withdrawInput.value)
      checkWithdrawForgerStakeEvent(expectedWithdrawEvent, listOfLogs(0))

      ///////////////////////////////////////////////////////////////////////////////////
      // Add again some stakes
      ///////////////////////////////////////////////////////////////////////////////////
      val txHash9 = Keccak256.hash("tx9")
      view.setupTxContext(txHash9, 10)
      stakeAmount = validWeiAmount

      msg = getMessage(contractAddress, stakeAmount, delegateData, randomNonce, owner1.address())
      assertGas(17787, msg, view, forgerStakeV2MessageProcessor, blockContext)

      listOfStakes = StakeStorage.getAllForgerStakes(view)
      assertEquals(1, listOfStakes.size)
      assertEquals(delegateCmdInput.forgerPublicKeys, listOfStakes.head.forgerPublicKeys)
      assertEquals(owner1, listOfStakes.head.ownerPublicKey)
      assertEquals(stakeAmount, listOfStakes.head.stakedAmount)

      // Check that the balances of the delegator and of the forger smart contract have changed
      assertEquals(stakeAmount, view.getBalance(contractAddress))

      expectedOwner1Balance = expectedOwner1Balance.subtract(stakeAmount)
      assertEquals(expectedOwner1Balance, view.getBalance(owner1.address()))

      // Check log event
      listOfLogs = view.getLogs(txHash9)
      assertEquals("Wrong number of logs", 1, listOfLogs.length)
      expectedDelegateEvent = DelegateForgerStake(owner1.address(), delegateCmdInput.forgerPublicKeys.blockSignPublicKey,
        delegateCmdInput.forgerPublicKeys.vrfPublicKey, stakeAmount)
      checkDelegateForgerStakeEvent(expectedDelegateEvent, listOfLogs(0))


    }
  }

  @Test
  def testGetStakeTotal(): Unit = {
    val blockSignerProposition1 = new PublicKey25519Proposition(BytesUtils.fromHexString("1122334455667788112233445566778811223344556677881122334455667788")) // 32 bytes
    val vrfPublicKey1 = new VrfPublicKey(BytesUtils.fromHexString("d6b775fd4cefc7446236683fdde9d0464bba43cc565fa066b0b3ed1b888b9d1180")) // 33 bytes
    val blockSignerProposition2 = new PublicKey25519Proposition(BytesUtils.fromHexString("1122334455667788112233445566778811223344556677881122334455667799")) // 32 bytes
    val vrfPublicKey2 = new VrfPublicKey(BytesUtils.fromHexString("d6b775fd4cefc7446236683fdde9d0464bba43cc565fa066b0b3ed1b888b9d1190")) // 33 bytes
    val address1: Address = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest1".getBytes(StandardCharsets.UTF_8)).publicImage().address()
    val address2: Address = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest2".getBytes(StandardCharsets.UTF_8)).publicImage().address()
    val address3: Address = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest3".getBytes(StandardCharsets.UTF_8)).publicImage().address()
    val address4: Address = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest4".getBytes(StandardCharsets.UTF_8)).publicImage().address()

    usingView(forgerStakeV2MessageProcessor) { view =>
      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)
      // Setup
      val initialAmount = BigInteger.valueOf(100).multiply(validWeiAmount)
      createSenderAccount(view, initialAmount)
      val txHash1 = Keccak256.hash("first tx")
      view.setupTxContext(txHash1, 10)

      // assert invocation fails until stake v2 is active
      val msg1 = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(StakeTotalCmd) ++ Array.emptyByteArray, randomNonce)
      val gas = new GasPool(1000000000)
      assertThrows[ExecutionRevertedException](TestContext.process(forgerStakeV2MessageProcessor, msg1, view, blockContextForkV1_4_plus10, gas, view))

      val BI_0 = BigInteger.ZERO
      val BI_20 = BigInteger.valueOf(20 * ZenCoinsUtils.COIN)
      val BI_40 = BigInteger.valueOf(40 * ZenCoinsUtils.COIN)
      val BI_60 = BigInteger.valueOf(60 * ZenCoinsUtils.COIN)
      val BI_80 = BigInteger.valueOf(80 * ZenCoinsUtils.COIN)

      StakeStorage.setActive(view)
      StakeStorage.addForger(view, blockSignerProposition1, vrfPublicKey1, 100, Address.ZERO, V1_4_MOCK_FORK_POINT + 3, address1, BI_20)
      StakeStorage.addForger(view, blockSignerProposition2, vrfPublicKey2, 100, Address.ZERO, V1_4_MOCK_FORK_POINT + 5, address2, BI_20)
      StakeStorage.addStake(view, blockSignerProposition1, vrfPublicKey1, V1_4_MOCK_FORK_POINT + 7, address3, BI_20)
      StakeStorage.addStake(view, blockSignerProposition1, vrfPublicKey1, V1_4_MOCK_FORK_POINT + 9, address4, BI_20)
      /*
          epoch forger1 forger2 delegator1  delegator2  total
          300   0       0       0           0           0
          303   20      0       0           0           20
          305   20      20      0           0           40
          307   40      20      20          0           60
          309   60      20      20          20          80

       */

      // get single delegation for current epoch
      var stakeTotalCmdInput = StakeTotalCmdInput(Some(ForgerPublicKeys(blockSignerProposition1, vrfPublicKey1)), Some(address3), None, None)
      var data: Array[Byte] = stakeTotalCmdInput.encode()
      var msg = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(StakeTotalCmd) ++ data, randomNonce)
      var returnData = assertGas(15000, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4_plus10)
      assertNotNull(returnData)
      var stakeTotalResponse = StakeTotalCmdOutputDecoder.decode(returnData)
      assertEquals(
        Seq(BI_20),
        stakeTotalResponse.listOfStakes
      )

      // get single forger for current epoch
      stakeTotalCmdInput = StakeTotalCmdInput(Some(ForgerPublicKeys(blockSignerProposition1, vrfPublicKey1)), None, None, None)
      data= stakeTotalCmdInput.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(StakeTotalCmd) ++ data, randomNonce)
      returnData = assertGas(19100, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4_plus10)
      assertNotNull(returnData)
      stakeTotalResponse = StakeTotalCmdOutputDecoder.decode(returnData)
      assertEquals(
        Seq(BI_60),
        stakeTotalResponse.listOfStakes
      )

      // get total stake for current epoch
      stakeTotalCmdInput = StakeTotalCmdInput(None, None, None, None)
      data= stakeTotalCmdInput.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(StakeTotalCmd) ++ data, randomNonce)
      returnData = assertGas(21300, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4_plus10)
      assertNotNull(returnData)
      stakeTotalResponse = StakeTotalCmdOutputDecoder.decode(returnData)
      assertEquals(
        Seq(BI_80),
        stakeTotalResponse.listOfStakes
      )

      // get total stake for last 11 epochs
      stakeTotalCmdInput = StakeTotalCmdInput(None, None, Some(V1_4_MOCK_FORK_POINT), Some(11))
      data= stakeTotalCmdInput.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(StakeTotalCmd) ++ data, randomNonce)
      returnData = assertGas(21500, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4_plus10)
      assertNotNull(returnData)
      stakeTotalResponse = StakeTotalCmdOutputDecoder.decode(returnData)
      assertEquals(
        Seq(BI_0, BI_0, BI_0, BI_20, BI_20, BI_40, BI_40, BI_60, BI_60, BI_80, BI_80),
        stakeTotalResponse.listOfStakes
      )

      // negative - illegal input params combination
      stakeTotalCmdInput = StakeTotalCmdInput(None, Some(address4), None, None)
      data= stakeTotalCmdInput.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(StakeTotalCmd) ++ data, randomNonce)
      assertThrows[ExecutionRevertedException](TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4_plus10, gas, view))

      // negative - not existing forger
      stakeTotalCmdInput = StakeTotalCmdInput(Some(ForgerPublicKeys(blockSignerProposition1, vrfPublicKey2)), None, None, None)
      data= stakeTotalCmdInput.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(StakeTotalCmd) ++ data, randomNonce)
      assertThrows[ExecutionRevertedException](TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4_plus10, gas, view))


    }
  }

  @Test
  def testGetForgerRewards(): Unit = {
    val blockSignerProposition1 = new PublicKey25519Proposition(BytesUtils.fromHexString("1122334455667788112233445566778811223344556677881122334455667788")) // 32 bytes
    val vrfPublicKey1 = new VrfPublicKey(BytesUtils.fromHexString("d6b775fd4cefc7446236683fdde9d0464bba43cc565fa066b0b3ed1b888b9d1180")) // 33 bytes
    val forgerPublicKeys1 = ForgerPublicKeys(blockSignerProposition1, vrfPublicKey1)
    val forgerRewards = Seq(BigInteger.valueOf(10), BigInteger.valueOf(20), BigInteger.valueOf(30), BigInteger.valueOf(40), BigInteger.valueOf(50))
    when(metadataStorageView.getForgerRewards(forgerPublicKeys1, 10, 5))
      .thenReturn(forgerRewards)

    usingView(forgerStakeV2MessageProcessor) { view =>
      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)

      // test getRewardsReceived fails until ForgerStakeV2 is active
      val msg1 = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(RewardsReceivedCmd) ++ Array.emptyByteArray, randomNonce)
      val gas = new GasPool(1000000000)
      assertThrows[ExecutionRevertedException](TestContext.process(forgerStakeV2MessageProcessor, msg1, view, blockContextForkV1_4_plus10, gas, view))

      StakeStorage.setActive(view)
      StakeStorage.addForger(view, blockSignerProposition1, vrfPublicKey1, 100, Address.ZERO, V1_4_MOCK_FORK_POINT + 3, Address.ZERO, BigInteger.ZERO)

      // test getRewardsReceived
      val rewardsReceivedCmdInput = RewardsReceivedCmdInput(ForgerPublicKeys(blockSignerProposition1, vrfPublicKey1), 10, 5)
      val msg = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(RewardsReceivedCmd) ++ rewardsReceivedCmdInput.encode(), randomNonce)
      val returnData = assertGas(10600, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4_plus10)
      assertNotNull(returnData)
      val rewardsReceivedOutput = RewardsReceivedCmdOutputDecoder.decode(returnData)
      assertEquals(
        forgerRewards,
        rewardsReceivedOutput.listOfRewards
      )
    }
  }

  @Test
  def testGetForgers(): Unit = {

    val processors = Seq(forgerStakeV2MessageProcessor, forgerStakeMessageProcessor)

    usingView(processors) { view =>
      forgerStakeMessageProcessor.init(view, V1_3_MOCK_FORK_POINT)
      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)

      // create sender account with some fund in it
      val initialAmount = BigInteger.valueOf(100).multiply(validWeiAmount)
      createSenderAccount(view, initialAmount)

      //Setting the context

      val blockSignerProposition = new PublicKey25519Proposition(BytesUtils.fromHexString("1122334455667788112233445566778811223344556677881122334455667788")) // 32 bytes
      val vrfPublicKey = new VrfPublicKey(BytesUtils.fromHexString("d6b775fd4cefc7446236683fdde9d0464bba43cc565fa066b0b3ed1b888b9d1180")) // 33 bytes

      /////////////////////////////////////////////////////////////////////////////////////////////
      //  Before activate tests
      /////////////////////////////////////////////////////////////////////////////////////////////

      // Check that getForger and getPagedForgers cannot be called before activate

      var getForgerCmdInput = SelectByForgerCmdInput(
        ForgerPublicKeys(blockSignerProposition, vrfPublicKey)
      )

      var getForgerData: Array[Byte] = BytesUtils.fromHexString(GetForgerCmd) ++ getForgerCmdInput.encode()
      var msg = getMessage(contractAddress, BigInteger.ZERO,  getForgerData, randomNonce)

      // Try getForger before fork 1.4
      val blockContextBeforeFork = new BlockContext(
        Address.ZERO,
        0,
        0,
        DefaultGasFeeFork.blockGasLimit,
        0,
        V1_4_MOCK_FORK_POINT - 1,
        0,
        1,
        MockedHistoryBlockHashProvider,
        Hash.ZERO
      )

      var exc = intercept[ExecutionRevertedException] {
        assertGas(0, msg, view, forgerStakeV2MessageProcessor, blockContextBeforeFork)
      }
      var expectedErr = "fork not active"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))


      // Try getForger after fork 1.4 but before activate
     exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }

      expectedErr = "Forger stake V2 has not been activated yet"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Try getPagedForgers before fork 1.4
      val getPagedForgersCmdInput = PagedForgersCmdInput(
        0, 100
      )

      val getPagedForgersData: Array[Byte] = BytesUtils.fromHexString(GetPagedForgersCmd) ++ getPagedForgersCmdInput.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO,  getPagedForgersData, randomNonce)

      exc = intercept[ExecutionRevertedException] {
        assertGas(0, msg, view, forgerStakeV2MessageProcessor, blockContextBeforeFork)
      }

      expectedErr = "fork not active"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Try getPagedForgers after fork 1.4 but before activate
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }

      expectedErr = "Forger stake V2 has not been activated yet"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))


      // Call activate

      val initialOwnerBalance = BigInteger.valueOf(200).multiply(validWeiAmount)
      createSenderAccount(view, initialOwnerBalance, ownerAddressProposition.address())

      msg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(ActivateCmd), randomNonce, ownerAddressProposition.address())

      assertGasInterop(0, msg, view, processors, blockContextForkV1_4)

      /////////////////////////////////////////////////////////////////////////////////////////////
      //  Tests
      /////////////////////////////////////////////////////////////////////////////////////////////

      // Try getForger for a non-existing forger. It should fail.
      msg = getMessage(contractAddress, BigInteger.ZERO,  getForgerData, randomNonce)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Forger doesn't exist."
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Try getPagedForgers without any forger. It should return an empty list
      msg = getMessage(contractAddress, BigInteger.ZERO,  getPagedForgersData, randomNonce)
      var res = assertGas(4200, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

      var getForgersOutput = PagedForgersOutputDecoder.decode(res)
      assertEquals(-1, getForgersOutput.nextStartPos)
      assertTrue(getForgersOutput.listOfForgerInfo.isEmpty)

      // Register a forger
      val initialStake = new BigInteger("1000000000000")

      StakeStorage.addForger(view, blockSignerProposition, vrfPublicKey,
        0, Address.ZERO, blockContextForkV1_4.consensusEpochNumber, ownerAddressProposition.address(), initialStake)

      // Try getForger
      msg = getMessage(contractAddress, BigInteger.ZERO,  getForgerData, randomNonce)
      res = assertGas(10600, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

      var getForgerOutput = GetForgerOutputDecoder.decode(res)
      assertEquals(getForgerCmdInput.forgerPublicKeys, getForgerOutput.forgerPublicKeys)
      assertEquals(0, getForgerOutput.rewardShare)
      assertEquals(Address.ZERO, getForgerOutput.rewardAddress.address())

      // Try getPagedForgers
      msg = getMessage(contractAddress, BigInteger.ZERO,  getPagedForgersData, randomNonce)
      res = assertGas(14700, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

      getForgersOutput = PagedForgersOutputDecoder.decode(res)
      assertEquals(-1, getForgersOutput.nextStartPos)
      assertEquals(1, getForgersOutput.listOfForgerInfo.size)
      assertEquals(getForgerCmdInput.forgerPublicKeys, getForgersOutput.listOfForgerInfo.head.forgerPublicKeys)
      assertEquals(0, getForgersOutput.listOfForgerInfo.head.rewardShare)
      assertEquals(Address.ZERO, getForgersOutput.listOfForgerInfo.head.rewardAddress.address())

      // Register more forgers, with a reward address

      // add the initial forger to the expected forgers
      val listOfExpectedForgers = getForgersOutput.listOfForgerInfo ++ (1 to 100).map {idx =>

        val postfix = f"$idx%03d"
        val blockSignerProposition = new PublicKey25519Proposition(BytesUtils.fromHexString(s"1122334455667788112233445566778811223344556677881122334455667$postfix")) // 32 bytes
        val vrfPublicKey = new VrfPublicKey(BytesUtils.fromHexString(s"d6b775fd4cefc7446236683fdde9d0464bba43cc565fa066b0b3ed1b888b9d1$postfix")) // 33 bytes

        val privateKey1: PrivateKeySecp256k1 = PrivateKeySecp256k1Creator.getInstance().generateSecret(s"nativemsgprocessortest$postfix".getBytes(StandardCharsets.UTF_8))
        val rewardAddress: AddressProposition = privateKey1.publicImage()
        val rewardShare = 1000 - idx

        StakeStorage.addForger(view, blockSignerProposition, vrfPublicKey,
          rewardShare, rewardAddress.address(), blockContextForkV1_4.consensusEpochNumber, ownerAddressProposition.address(), initialStake)
        ForgerInfo(ForgerPublicKeys(blockSignerProposition, vrfPublicKey), rewardShare, rewardAddress)
      }

     // Try getForger
      listOfExpectedForgers.foreach { expForgerInfo =>
        getForgerCmdInput = SelectByForgerCmdInput(expForgerInfo.forgerPublicKeys)
        getForgerData = BytesUtils.fromHexString(GetForgerCmd) ++ getForgerCmdInput.encode()
        msg = getMessage(contractAddress, BigInteger.ZERO, getForgerData, randomNonce)
        res = assertGas(10600, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

        getForgerOutput = GetForgerOutputDecoder.decode(res)
        assertEquals(expForgerInfo, getForgerOutput)
      }

      // Try getPagedForgers

      @tailrec
      def checkPagedResult(listOfExpectedForgers: Seq[ForgerInfo], startPos: Int, pageSize: Int): Unit = {
        val (currentPage, remaining) = listOfExpectedForgers.splitAt(pageSize)
        if (listOfExpectedForgers.size < pageSize)
          assertTrue(remaining.isEmpty)
        else
          assertEquals(listOfExpectedForgers.size - pageSize, remaining.size)

        val cmdInput = PagedForgersCmdInput(
          startPos,
          pageSize
        )
        val msg = getMessage(
          contractAddress, 0, BytesUtils.fromHexString(GetPagedForgersCmd) ++ cmdInput.encode(), randomNonce)
        val returnData = withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
        //Check getPagedForgers
        val res = PagedForgersOutputDecoder.decode(returnData)
        assertEquals(currentPage, res.listOfForgerInfo)
        if (remaining.isEmpty)
          assertEquals(-1, res.nextStartPos)
        else {
          checkPagedResult(remaining, res.nextStartPos, pageSize)
        }

      }

      checkPagedResult(listOfExpectedForgers, 0, listOfExpectedForgers.size + 10)
      checkPagedResult(listOfExpectedForgers, 0, listOfExpectedForgers.size)
      checkPagedResult(listOfExpectedForgers, 0, listOfExpectedForgers.size - 1)
      checkPagedResult(listOfExpectedForgers, 0, 13)

      var startPos = 3
      checkPagedResult(listOfExpectedForgers.drop(startPos), startPos, 5)

      startPos = listOfExpectedForgers.size - 1
      checkPagedResult(listOfExpectedForgers.drop(startPos), startPos, 1)


      ////////////////////////////////////////////////////////////
      // Negative tests
      //////////////////////////////////////////////////////////

      // Check that it is not payable
      msg = getMessage(contractAddress, validWeiAmount, getForgerData, randomNonce)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Call value must be zero"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))


      msg = getMessage(contractAddress, validWeiAmount, getPagedForgersData, randomNonce)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Call value must be zero"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))


      // try processing a msg with a trailing byte in the arguments
      var badData = Bytes.concat(getForgerData, new Array[Byte](1))
      var msgBad = getMessage(contractAddress, BigInteger.ZERO, badData, randomNonce)

      // should fail because input has a trailing byte
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msgBad, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Wrong message data field length"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      badData = Bytes.concat(getPagedForgersData, new Array[Byte](1))
      msgBad = getMessage(contractAddress, BigInteger.ZERO, badData, randomNonce)

      // should fail because input has a trailing byte
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msgBad, view, blockContextForkV1_4, _, view))
      }
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Try getPagedForgers with invalid input

      var cmdInput = PagedForgersCmdInput(
        -1,
        10
      )
      msg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(GetPagedForgersCmd) ++ cmdInput.encode(), randomNonce)
      exc = intercept[ExecutionRevertedException] {
        assertGasInterop(2100, msg, view, processors, blockContextForkV1_4)
      }
      expectedErr = "Invalid startPos input"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))


      cmdInput = PagedForgersCmdInput(
        listOfExpectedForgers.size,
        10
      )
      msg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(GetPagedForgersCmd) ++ cmdInput.encode(), randomNonce)
      exc = intercept[ExecutionRevertedException] {
        assertGasInterop(4200, msg, view, processors, blockContextForkV1_4)
      }
      expectedErr = "Invalid start position"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))


      cmdInput = PagedForgersCmdInput(
        0,
        -1
      )
      msg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(GetPagedForgersCmd) ++ cmdInput.encode(), randomNonce)
      exc = intercept[ExecutionRevertedException] {
        assertGasInterop(2100, msg, view, processors, blockContextForkV1_4)
      }
      expectedErr = "Invalid page size"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      cmdInput = PagedForgersCmdInput(
        0,
        0
      )
      msg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(GetPagedForgersCmd) ++ cmdInput.encode(), randomNonce)
      exc = intercept[ExecutionRevertedException] {
        assertGasInterop(2100, msg, view, processors, blockContextForkV1_4)
      }
      expectedErr = "Invalid page size"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

    }

  }


  @Test
  def testGetCurrentConsensusEpoch(): Unit = {

    val processors = Seq(forgerStakeV2MessageProcessor, forgerStakeMessageProcessor)

    usingView(processors) { view =>
      forgerStakeMessageProcessor.init(view, V1_3_MOCK_FORK_POINT)
      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)

      // create sender account with some fund in it
      val initialAmount = BigInteger.valueOf(100).multiply(validWeiAmount)
      createSenderAccount(view, initialAmount)

      /////////////////////////////////////////////////////////////////////////////////////////////
      //  Before activate tests
      /////////////////////////////////////////////////////////////////////////////////////////////

      // Check that getCurrentConsensusEpoch cannot be called before activate

      val getCurrentConsensusEpochData: Array[Byte] = BytesUtils.fromHexString(GetCurrentConsensusEpochCmd)
      var msg = getMessage(contractAddress, BigInteger.ZERO,  getCurrentConsensusEpochData, randomNonce)

      // Try GetCurrentConsensusEpochCmd before fork 1.4
      val blockContextBeforeFork = new BlockContext(
        Address.ZERO,
        0,
        0,
        DefaultGasFeeFork.blockGasLimit,
        0,
        V1_4_MOCK_FORK_POINT - 1,
        0,
        1,
        MockedHistoryBlockHashProvider,
        Hash.ZERO
      )

      var exc = intercept[ExecutionRevertedException] {
        assertGas(0, msg, view, forgerStakeV2MessageProcessor, blockContextBeforeFork)
      }
      var expectedErr = "fork not active"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))


      // Try getForger after fork 1.4 but before activate
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }

      expectedErr = "Forger stake V2 has not been activated yet"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // Call activate

      val initialOwnerBalance = BigInteger.valueOf(200).multiply(validWeiAmount)
      createSenderAccount(view, initialOwnerBalance, ownerAddressProposition.address())

      msg = getMessage(
        contractAddress, 0, BytesUtils.fromHexString(ActivateCmd), randomNonce, ownerAddressProposition.address())

      assertGasInterop(0, msg, view, processors, blockContextForkV1_4)

      /////////////////////////////////////////////////////////////////////////////////////////////
      //  Tests
      /////////////////////////////////////////////////////////////////////////////////////////////

      msg = getMessage(contractAddress, BigInteger.ZERO,  getCurrentConsensusEpochData, randomNonce)
      var res = assertGas(2100, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4)

      var epoch = ConsensusEpochCmdOutputDecoder.decode(res).epoch
      assertEquals(blockContextForkV1_4.consensusEpochNumber, epoch)

      val expectedEpoch = blockContextForkV1_4.consensusEpochNumber + 345
      res = assertGas(2100, msg, view, forgerStakeV2MessageProcessor, getBlockContextForEpoch(expectedEpoch))
      epoch = ConsensusEpochCmdOutputDecoder.decode(res).epoch
      assertEquals(expectedEpoch, epoch)

      ////////////////////////////////////////////////////////////
      // Negative tests
      //////////////////////////////////////////////////////////

      // Check that it is not payable
      msg = getMessage(contractAddress, validWeiAmount, getCurrentConsensusEpochData, randomNonce)
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msg, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "Call value must be zero"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))

      // try processing a msg with a trailing byte in the arguments
      val badData = Bytes.concat(getCurrentConsensusEpochData, new Array[Byte](1))
      val msgBad = getMessage(contractAddress, BigInteger.ZERO, badData, randomNonce)

      // should fail because input has a trailing byte
      exc = intercept[ExecutionRevertedException] {
        withGas(TestContext.process(forgerStakeV2MessageProcessor, msgBad, view, blockContextForkV1_4, _, view))
      }
      expectedErr = "invalid msg data length"
      assertTrue(s"Wrong error message, expected $expectedErr, got: ${exc.getMessage}", exc.getMessage.contains(expectedErr))
    }

  }


  def checkActivateEvents(listOfLogs: Array[EthereumConsensusDataLog]): Unit = {
    assertEquals("Wrong number of logs", 2, listOfLogs.length)

    assertEquals("Wrong address", forgerStakeMessageProcessor.contractAddress, listOfLogs.head.address)
    assertArrayEquals("Wrong event signature", getEventSignature("DisableStakeV1()"), listOfLogs.head.topics(0).toBytes)

    assertEquals("Wrong address", contractAddress, listOfLogs(1).address)
    assertEquals("Wrong number of topics", 1, listOfLogs(1).topics.length) //The first topic is the hash of the signature of the event
    assertArrayEquals("Wrong event signature", ActivateStakeV2EventSig, listOfLogs(1).topics(0).toBytes)

  }


  def checkRegisterForgerEvent(expectedEvent: RegisterForger, actualEvent: EthereumConsensusDataLog): Unit = {
    assertEquals("Wrong address", contractAddress, actualEvent.address)
    assertEquals("Wrong number of topics", NumOfIndexedRegisterForgerEvtParams + 1, actualEvent.topics.length) //The first topic is the hash of the signature of the event
    assertArrayEquals("Wrong event signature", RegisterForgerEventSig, actualEvent.topics(0).toBytes)
    assertEquals("Wrong sender address in topic", expectedEvent.sender, decodeEventTopic(actualEvent.topics(1), TypeReference.makeTypeReference(expectedEvent.sender.getTypeAsString)))
    assertEquals("Wrong vrfKey1 in topic", expectedEvent.vrf1, decodeEventTopic(actualEvent.topics(2), TypeReference.makeTypeReference(expectedEvent.vrf1.getTypeAsString)))
    assertEquals("Wrong vrfKey2 in topic", expectedEvent.vrf2, decodeEventTopic(actualEvent.topics(3), TypeReference.makeTypeReference(expectedEvent.vrf2.getTypeAsString)))

    val listOfRefs = util.Arrays.asList(
      TypeReference.makeTypeReference(expectedEvent.signPubKey.getTypeAsString),
      TypeReference.makeTypeReference(expectedEvent.value.getTypeAsString),
      TypeReference.makeTypeReference(expectedEvent.rewardShare.getTypeAsString),
      TypeReference.makeTypeReference(expectedEvent.rewardAddress.getTypeAsString))
      .asInstanceOf[util.List[TypeReference[Type[_]]]]
    val listOfDecodedData = FunctionReturnDecoder.decode(BytesUtils.toHexString(actualEvent.data), listOfRefs)
    assertEquals("Wrong signer key in data", expectedEvent.signPubKey, listOfDecodedData.get(0))
    assertEquals("Wrong amount in data", expectedEvent.value.getValue, listOfDecodedData.get(1).getValue)
    assertEquals("Wrong reward share in data", expectedEvent.rewardShare.getValue, listOfDecodedData.get(2).getValue)
    assertEquals("Wrong reward address in data", expectedEvent.rewardAddress, listOfDecodedData.get(3))
  }


  def checkUpdateForgerEvent(expectedEvent: UpdateForger, actualEvent: EthereumConsensusDataLog): Unit = {
    assertEquals("Wrong address", contractAddress, actualEvent.address)
    assertEquals("Wrong number of topics", NumOfIndexedRegisterForgerEvtParams + 1, actualEvent.topics.length) //The first topic is the hash of the signature of the event
    assertArrayEquals("Wrong event signature", UpdateForgerEventSig, actualEvent.topics(0).toBytes)
    assertEquals("Wrong signer key address in topic", expectedEvent.sender, decodeEventTopic(actualEvent.topics(1), TypeReference.makeTypeReference(expectedEvent.sender.getTypeAsString)))
    assertEquals("Wrong vrfKey1 in topic", expectedEvent.vrf1, decodeEventTopic(actualEvent.topics(2), TypeReference.makeTypeReference(expectedEvent.vrf1.getTypeAsString)))
    assertEquals("Wrong vrfKey2 in topic", expectedEvent.vrf2, decodeEventTopic(actualEvent.topics(3), TypeReference.makeTypeReference(expectedEvent.vrf2.getTypeAsString)))

    val listOfRefs = util.Arrays.asList(
      TypeReference.makeTypeReference(expectedEvent.signPubKey.getTypeAsString),
      TypeReference.makeTypeReference(expectedEvent.rewardShare.getTypeAsString),
      TypeReference.makeTypeReference(expectedEvent.rewardAddress.getTypeAsString))
      .asInstanceOf[util.List[TypeReference[Type[_]]]]
    val listOfDecodedData = FunctionReturnDecoder.decode(BytesUtils.toHexString(actualEvent.data), listOfRefs)
    assertEquals("Wrong signature in data", expectedEvent.signPubKey, listOfDecodedData.get(0))
    assertEquals("Wrong reward share in data", expectedEvent.rewardShare.getValue, listOfDecodedData.get(1).getValue)
    assertEquals("Wrong reward address in data", expectedEvent.rewardAddress, listOfDecodedData.get(2))
  }

  def checkDelegateForgerStakeEvent(expectedEvent: DelegateForgerStake, actualEvent: EthereumConsensusDataLog): Unit = {
    assertEquals("Wrong address", contractAddress, actualEvent.address)
    assertEquals("Wrong number of topics", NumOfIndexedDelegateStakeEvtParams + 1, actualEvent.topics.length) //The first topic is the hash of the signature of the event
    assertArrayEquals("Wrong event signature", DelegateForgerStakeEventSig, actualEvent.topics(0).toBytes)
    assertEquals("Wrong sender address in topic", expectedEvent.sender, decodeEventTopic(actualEvent.topics(1), TypeReference.makeTypeReference(expectedEvent.sender.getTypeAsString)))
    assertEquals("Wrong vrfKey1 in topic", expectedEvent.vrf1, decodeEventTopic(actualEvent.topics(2), TypeReference.makeTypeReference(expectedEvent.vrf1.getTypeAsString)))
    assertEquals("Wrong vrfKey2 in topic", expectedEvent.vrf2, decodeEventTopic(actualEvent.topics(3), TypeReference.makeTypeReference(expectedEvent.vrf2.getTypeAsString)))

    val listOfRefs = util.Arrays.asList(
        TypeReference.makeTypeReference(expectedEvent.signPubKey.getTypeAsString),
        TypeReference.makeTypeReference(expectedEvent.value.getTypeAsString))
      .asInstanceOf[util.List[TypeReference[Type[_]]]]
    val listOfDecodedData = FunctionReturnDecoder.decode(BytesUtils.toHexString(actualEvent.data), listOfRefs)
    assertEquals("Wrong signPubKey in data", expectedEvent.signPubKey, listOfDecodedData.get(0))
    assertEquals("Wrong amount in data", expectedEvent.value.getValue, listOfDecodedData.get(1).getValue)
  }

  def checkWithdrawForgerStakeEvent(expectedEvent: WithdrawForgerStake, actualEvent: EthereumConsensusDataLog): Unit = {
    assertEquals("Wrong address", contractAddress, actualEvent.address)
    assertEquals("Wrong number of topics", NumOfIndexedDelegateStakeEvtParams + 1, actualEvent.topics.length) //The first topic is the hash of the signature of the event
    assertArrayEquals("Wrong event signature", WithdrawForgerStakeEventSig, actualEvent.topics(0).toBytes)
    assertEquals("Wrong sender address in topic", expectedEvent.sender, decodeEventTopic(actualEvent.topics(1), TypeReference.makeTypeReference(expectedEvent.sender.getTypeAsString)))
    assertEquals("Wrong vrfKey1 in topic", expectedEvent.vrf1, decodeEventTopic(actualEvent.topics(2), TypeReference.makeTypeReference(expectedEvent.vrf1.getTypeAsString)))
    assertEquals("Wrong vrfKey2 in topic", expectedEvent.vrf2, decodeEventTopic(actualEvent.topics(3), TypeReference.makeTypeReference(expectedEvent.vrf2.getTypeAsString)))

    val listOfRefs = util.Arrays.asList(
        TypeReference.makeTypeReference(expectedEvent.signPubKey.getTypeAsString),
        TypeReference.makeTypeReference(expectedEvent.value.getTypeAsString))
      .asInstanceOf[util.List[TypeReference[Type[_]]]]
    val listOfDecodedData = FunctionReturnDecoder.decode(BytesUtils.toHexString(actualEvent.data), listOfRefs)
    assertEquals("Wrong signPubKey in data", expectedEvent.signPubKey, listOfDecodedData.get(0))
    assertEquals("Wrong amount in data", expectedEvent.value.getValue, listOfDecodedData.get(1).getValue)
  }

  @Test
  def getStakeStartTest() = {
    val blockSignerProposition = new PublicKey25519Proposition(BytesUtils.fromHexString("1122334455667788112233445566778811223344556677881122334455667788")) // 32 bytes
    val vrfPublicKey = new VrfPublicKey(BytesUtils.fromHexString("d6b775fd4cefc7446236683fdde9d0464bba43cc565fa066b0b3ed1b888b9d1180")) // 33 bytes
    val address1: Address = PrivateKeySecp256k1Creator.getInstance().generateSecret("nativemsgprocessortest1".getBytes(StandardCharsets.UTF_8)).publicImage().address()

    usingView(forgerStakeV2MessageProcessor) { view =>
      forgerStakeV2MessageProcessor.init(view, view.getConsensusEpochNumberAsInt)
      // Setup
      val initialAmount = BigInteger.valueOf(100).multiply(validWeiAmount)
      createSenderAccount(view, initialAmount)
      val txHash1 = Keccak256.hash("first tx")
      view.setupTxContext(txHash1, 10)

      // assert invocation fails until stake v2 is active
      val msg1 = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(StakeStartCmd) ++ Array.emptyByteArray, randomNonce)
      val gas1 = new GasPool(1000000000)
      assertThrows[ExecutionRevertedException](TestContext.process(forgerStakeV2MessageProcessor, msg1, view, blockContextForkV1_4_plus10, gas1, view))
      StakeStorage.setActive(view)

      // assert default value -1 is returned if delegation does not exist
      var stakeStartCmdInput = StakeStartCmdInput(ForgerPublicKeys(blockSignerProposition, vrfPublicKey), address1)
      var data: Array[Byte] = stakeStartCmdInput.encode()
      var msg = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(StakeStartCmd) ++ data, randomNonce)
      var returnData = assertGas(4200, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4_plus10)
      assertNotNull(returnData)
      var stakeStartResponse = StakeStartCmdOutputDecoder.decode(returnData)
      assertEquals(
        -1,
        stakeStartResponse.epoch
      )

      val stakeStartEpoch = V1_4_MOCK_FORK_POINT + 3
      StakeStorage.addForger(view, blockSignerProposition, vrfPublicKey, 100, Address.ZERO, stakeStartEpoch, address1, BigInteger.TEN)
      StakeStorage.addStake(view, blockSignerProposition, vrfPublicKey, stakeStartEpoch + 5, address1, BigInteger.TEN)

      // get valid stateStart
      stakeStartCmdInput = StakeStartCmdInput(ForgerPublicKeys(blockSignerProposition, vrfPublicKey), address1)
      data = stakeStartCmdInput.encode()
      msg = getMessage(contractAddress, BigInteger.ZERO, BytesUtils.fromHexString(StakeStartCmd) ++ data, randomNonce)
      returnData = assertGas(6300, msg, view, forgerStakeV2MessageProcessor, blockContextForkV1_4_plus10)
      assertNotNull(returnData)
      stakeStartResponse = StakeStartCmdOutputDecoder.decode(returnData)
      assertEquals(
        stakeStartEpoch,
        stakeStartResponse.epoch
      )
    }
  }

  private def addStakesV2(view: AccountStateView,
                        blockSignerProposition: PublicKey25519Proposition,
                        vrfPublicKey: VrfPublicKey,
                        ownerAddressProposition1: AddressProposition,
                        numOfStakes: Int,
                        blockContext: BlockContext): BigInteger = {
    val cmdInput1 = AddNewStakeCmdInput(
      ForgerPublicKeys(blockSignerProposition, vrfPublicKey),
      ownerAddressProposition1.address()
    )
    val data: Array[Byte] = cmdInput1.encode()

    var listOfForgerStakes = Seq[AccountForgingStakeInfo]()

    var totalAmount = BigInteger.ZERO
    for (i <- 1 to numOfStakes) {
      val stakeAmount = validWeiAmount.multiply(BigInteger.valueOf(i))
      val nonce = randomNonce
      val msg = getMessage(contractAddress, stakeAmount,
        BytesUtils.fromHexString(AddNewStakeCmdV1) ++ data, nonce)
      val expStakeId = forgerStakeMessageProcessor.getStakeId(msg)
      listOfForgerStakes = listOfForgerStakes :+ AccountForgingStakeInfo(expStakeId,
        ForgerStakeData(ForgerPublicKeys(blockSignerProposition, vrfPublicKey),
          ownerAddressProposition1, stakeAmount))
      val returnData = withGas(TestContext.process(forgerStakeMessageProcessor, msg, view, blockContext, _, view))
      assertNotNull(returnData)
      totalAmount = totalAmount.add(stakeAmount)
    }
    totalAmount
  }

}


