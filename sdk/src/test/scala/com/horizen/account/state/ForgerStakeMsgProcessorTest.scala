package com.horizen.account.state

import com.google.common.primitives.Bytes
import com.horizen.account.abi
import com.horizen.account.proof.SignatureSecp256k1
import com.horizen.account.proposition.AddressProposition
import com.horizen.account.state.ForgerStakeMsgProcessor.{AddNewStakeCmd, GetListOfForgersCmd, RemoveStakeCmd}
import com.horizen.params.NetworkParams
import com.horizen.proposition.{PublicKey25519Proposition, VrfPublicKey}
import com.horizen.utils.{ByteArrayWrapper, BytesUtils}
import org.junit.Assert._
import org.junit._
import org.mockito._
import org.scalatestplus.junit.JUnitSuite
import org.scalatestplus.mockito._
import org.web3j.abi.datatypes.DynamicArray
import org.web3j.abi.{DefaultFunctionReturnDecoder, TypeReference}
import org.web3j.crypto.{ECKeyPair, Keys, Sign}
import scorex.crypto.hash.Keccak256

import java.math.BigInteger
import java.util
import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import scala.collection.immutable.Seq



class ForgerStakeMsgProcessorTest
  extends JUnitSuite
    with MockitoSugar
    with MessageProcessorFixture {

  val dummyBigInteger: java.math.BigInteger = java.math.BigInteger.ONE
  val negativeAmount: java.math.BigInteger = BigInteger.valueOf(-1)

  val invalidWeiAmount: java.math.BigInteger = new java.math.BigInteger("10000000001")
  val validWeiAmount: java.math.BigInteger = new java.math.BigInteger("10000000000")

  val senderProposition: AddressProposition = new AddressProposition(BytesUtils.fromHexString("00aabbcc9900aabbcc9900aabbcc9900aabbcc99"))

  val mockNetworkParams : NetworkParams = mock[NetworkParams]
  val forgerStakeMessageProcessor : ForgerStakeMsgProcessor = ForgerStakeMsgProcessor(mockNetworkParams)

  // create private/public key pair
  val pair : ECKeyPair = Keys.createEcKeyPair
  val ownerAddressProposition = new AddressProposition(BytesUtils.fromHexString(Keys.getAddress(pair)))

  @Before
  def setUp(): Unit = {
  }

  def getDefaultMessage(opCode: Array[Byte], arguments: Array[Byte], nonce: BigInteger, value: BigInteger = negativeAmount) : Message = {
    val data = Bytes.concat(opCode, arguments)
    new Message(
      senderProposition,
      forgerStakeMessageProcessor.fakeSmartContractAddress, // to
      dummyBigInteger, // gasPrice
      dummyBigInteger, // gasFeeCap
      dummyBigInteger, // gasTipCap
      dummyBigInteger, // gasLimit
      value,
      nonce,
      data)
  }

  def getRandomNonce: BigInteger = {
    val nonce = new Array[Byte](32)
    scala.util.Random.nextBytes(nonce)
    new java.math.BigInteger(nonce)
  }

  def removeForgerStake(stateView: AccountStateView, stakeId: Array[Byte]): Unit = {
    val nonce = getRandomNonce
    val msgToSign = ForgerStakeMsgProcessor.getMessageToSign(stakeId, senderProposition.address(), nonce.toByteArray)
    val msgSignatureData = Sign.signMessage(msgToSign, pair, true)
    val msgSignature = new SignatureSecp256k1(msgSignatureData)

    // create command arguments
    val removeCmdInput = RemoveStakeCmdInput(stakeId, msgSignature)

    val data: Array[Byte] = removeCmdInput.encode()

    val msg = getDefaultMessage(
      BytesUtils.fromHexString(RemoveStakeCmd),
      data, nonce)

    // try processing the removal of stake, should succeed
    forgerStakeMessageProcessor.process(msg, stateView) match {
      case res: InvalidMessage => Assert.fail(s"Wrong result: $res")
      case res: ExecutionFailed => Assert.fail(s"Wrong result: $res")
      case res: ExecutionSucceeded =>
        assertTrue(res.hasReturnData)
    }
  }

  def getForgerStakeList(stateView: AccountStateView) : java.util.List[abi.util.AccountForgingStakeInfo] = {

    val data: Array[Byte] = new Array[Byte](0)
    val msg = getDefaultMessage(BytesUtils.fromHexString(GetListOfForgersCmd),
      data, getRandomNonce)

    forgerStakeMessageProcessor.process(msg, stateView) match {
      case _: InvalidMessage =>
        throw new IllegalArgumentException("")
      case _: ExecutionFailed =>
        throw new IllegalArgumentException("")

      case res: ExecutionSucceeded =>
        assertTrue(res.hasReturnData)
        assertTrue(res.gasUsed() == forgerStakeMessageProcessor.RemoveStakeGasPaidValue)

        decodeListOfForgerStake(res.returnData())
    }
  }

  def decodeListOfForgerStake(data: Array[Byte]): java.util.List[abi.util.AccountForgingStakeInfo] ={
    val decoder = new DefaultFunctionReturnDecoder()
    val typeRef1 = org.web3j.abi.Utils.convert(util.Arrays.asList(new TypeReference[DynamicArray[abi.util.AccountForgingStakeInfo]]() {}))
    val listOfWR = decoder.decodeFunctionResult(org.web3j.utils.Numeric.toHexString(data), typeRef1)
    listOfWR.get(0).asInstanceOf[DynamicArray[abi.util.AccountForgingStakeInfo]].getValue

  }

  def createSenderAccount(view: AccountStateView, amount: BigInteger = BigInteger.ZERO) : Unit = {
    if (!view.accountExists(senderProposition.address())) {
      val codeHash = new Array[Byte](32)
      scala.util.Random.nextBytes(codeHash)
      view.addAccount(senderProposition.address(), codeHash)

      if (amount.compareTo(BigInteger.ZERO) >= 0)
      {
         view.addBalance(senderProposition.address(), amount)
      }
    }
  }

  @Test
  def testMethodIds(): Unit = {
    //The expected methodIds were calcolated using this site: https://emn178.github.io/online-tools/keccak_256.html
    assertEquals("Wrong MethodId for GetListOfForgersCmd", "a64717f5", ForgerStakeMsgProcessor.GetListOfForgersCmd)
    assertEquals("Wrong MethodId for AddNewStakeCmd", "5ca748ff", ForgerStakeMsgProcessor.AddNewStakeCmd)
    assertEquals("Wrong MethodId for RemoveStakeCmd", "f7419d79", ForgerStakeMsgProcessor.RemoveStakeCmd)
  }



  @Test
  def testNullRecords(): Unit = {
    val stateView = getView
    forgerStakeMessageProcessor.init(stateView)

    // getting a not existing key from state DB using RAW strategy gives an array of 32 bytes filled with 0, while
    // using CHUNK strategy gives an empty array instead.
    // If this behaviour changes, the codebase must change as well

    val notExistingKey1 = Keccak256.hash("NONE1")
    stateView.removeAccountStorage(forgerStakeMessageProcessor.fakeSmartContractAddress.address(), notExistingKey1)
    val ret1 = stateView.getAccountStorage(forgerStakeMessageProcessor.fakeSmartContractAddress.address(), notExistingKey1).get
    require(new ByteArrayWrapper(ret1) == new ByteArrayWrapper(new Array[Byte](32)))

    val notExistingKey2 = Keccak256.hash("NONE2")
    stateView.removeAccountStorageBytes(forgerStakeMessageProcessor.fakeSmartContractAddress.address(), notExistingKey2)
    val ret2 = stateView.getAccountStorageBytes(forgerStakeMessageProcessor.fakeSmartContractAddress.address(), notExistingKey2).get
    require(new ByteArrayWrapper(ret2) == new ByteArrayWrapper(new Array[Byte](0)))

    stateView.stateDb.commit()
    stateView.stateDb.close()
  }


  @Test
  def testInit(): Unit = {
    val stateView = getView

    // we have to call init beforehand
    assertFalse(stateView.accountExists(forgerStakeMessageProcessor.fakeSmartContractAddress.address()))
    
    forgerStakeMessageProcessor.init(stateView)

    assertTrue(stateView.accountExists(forgerStakeMessageProcessor.fakeSmartContractAddress.address()))

    stateView.stateDb.commit()
    stateView.close()

  }

  @Test
  def testCanProcess(): Unit = {
    val stateView = getView

    forgerStakeMessageProcessor.init(stateView)

    val msg = new Message(senderProposition, forgerStakeMessageProcessor.fakeSmartContractAddress,
      dummyBigInteger, dummyBigInteger, dummyBigInteger, dummyBigInteger, dummyBigInteger, dummyBigInteger, new Array[Byte](0))

    assertTrue(forgerStakeMessageProcessor.canProcess(msg, stateView))

    val invalidForgerStakeProposition = new AddressProposition(BytesUtils.fromHexString("35fdd51e73221f467b40946c97791a3e19799bea"))
    val msgNotProcessable = new Message(senderProposition, invalidForgerStakeProposition,
      dummyBigInteger, dummyBigInteger, dummyBigInteger, dummyBigInteger, dummyBigInteger, dummyBigInteger, new Array[Byte](0))
    assertFalse(forgerStakeMessageProcessor.canProcess(msgNotProcessable, stateView))

    val nullForgerStakeProposition = null
    val msgNotProcessable2 = new Message(senderProposition, nullForgerStakeProposition,
      dummyBigInteger, dummyBigInteger, dummyBigInteger, dummyBigInteger, dummyBigInteger, dummyBigInteger, new Array[Byte](0))
    assertFalse(forgerStakeMessageProcessor.canProcess(msgNotProcessable, stateView))

    stateView.stateDb.commit()
    stateView.close()
  }


  @Test
  def testAddAndRemoveStake(): Unit = {

    val blockSignerProposition = new PublicKey25519Proposition(BytesUtils.fromHexString("1122334455667788112233445566778811223344556677881122334455667788")) // 32 bytes
    val vrfPublicKey = new VrfPublicKey(BytesUtils.fromHexString("aabbccddeeff0099aabbccddeeff0099aabbccddeeff0099aabbccddeeff001234")) // 33 bytes

    val stateView = getView

    forgerStakeMessageProcessor.init(stateView)

    // create sender account with some fund in it
    val initialAmount = BigInteger.valueOf(10).multiply(validWeiAmount)
    createSenderAccount(stateView, initialAmount)

    Mockito.when(mockNetworkParams.restrictForgers).thenReturn(true)
    Mockito.when(mockNetworkParams.allowedForgersList).thenReturn(Seq((blockSignerProposition,vrfPublicKey)))

    val cmdInput = AddNewStakeCmdInput(
      ForgerPublicKeys(blockSignerProposition, vrfPublicKey),
      ownerAddressProposition
    )

    val data: Array[Byte] = cmdInput.encode()
    val msg = getDefaultMessage(
      BytesUtils.fromHexString(AddNewStakeCmd),
      data, getRandomNonce, validWeiAmount)

    // positive case, verify we can add the stake to view
    forgerStakeMessageProcessor.process(msg, stateView) match {
      case res: ExecutionSucceeded =>
        assertTrue(res.hasReturnData)
        assertTrue(res.gasUsed() == forgerStakeMessageProcessor.AddNewStakeGasPaidValue)
        println("This is the returned value: " + BytesUtils.toHexString(res.returnData()))

      case result => Assert.fail(s"Wrong result: $result")
    }

    // verify we added the amount to smart contract and we charge the sender
    assertTrue(stateView.getBalance(forgerStakeMessageProcessor.fakeSmartContractAddress.address()) == validWeiAmount)
    assertTrue(stateView.getBalance(senderProposition.address()) == initialAmount.subtract(validWeiAmount))

    // try processing a msg with the same stake (same msg), should fail
    forgerStakeMessageProcessor.process(msg, stateView) match {
      case res: ExecutionFailed =>
        println("This is the reason: " + res.getReason.getMessage)
      case result => Assert.fail(s"Wrong result: $result")
    }

    // try processing a msg with different stake id (different nonce), should succeed
    val msg2 = getDefaultMessage(
      BytesUtils.fromHexString(AddNewStakeCmd),
      data, getRandomNonce, validWeiAmount)

    forgerStakeMessageProcessor.process(msg2, stateView) match {
      case res: InvalidMessage => Assert.fail(s"Wrong result: $res")
      case res: ExecutionFailed => Assert.fail(s"Wrong result: $res")
      case res: ExecutionSucceeded =>
        assertTrue(res.hasReturnData)
        assertTrue(res.gasUsed() == forgerStakeMessageProcessor.AddNewStakeGasPaidValue)
        println("This is the returned value: " + BytesUtils.toHexString(res.returnData()))
    }

    // verify we added the amount to smart contract and we charge the sender
    assertTrue(stateView.getBalance(forgerStakeMessageProcessor.fakeSmartContractAddress.address()) == validWeiAmount.multiply(BigInteger.TWO))
    assertTrue(stateView.getBalance(senderProposition.address()) == initialAmount.subtract(validWeiAmount.multiply(BigInteger.TWO)))

    // remove first stake id

    val stakeId = forgerStakeMessageProcessor.getStakeId(msg)
    val nonce3 = getRandomNonce
    val msgToSign = ForgerStakeMsgProcessor.getMessageToSign(stakeId, senderProposition.address(), nonce3.toByteArray)

    val msgSignatureData = Sign.signMessage(msgToSign, pair, true)
    val msgSignature = new SignatureSecp256k1(msgSignatureData)

    // create command arguments
    val removeCmdInput = RemoveStakeCmdInput(stakeId, msgSignature)

    val data3: Array[Byte] = removeCmdInput.encode()

    val msg3 = getDefaultMessage(
      BytesUtils.fromHexString(RemoveStakeCmd),
      data3, nonce3)

    // try processing the removal of stake, should succeed
    forgerStakeMessageProcessor.process(msg3, stateView) match {
      case res: InvalidMessage => Assert.fail(s"Wrong result: $res")
      case res: ExecutionFailed => Assert.fail(s"Wrong result: $res")
      case res: ExecutionSucceeded =>
        assertTrue(res.hasReturnData)
        assertTrue(res.gasUsed() == forgerStakeMessageProcessor.RemoveStakeGasPaidValue)
        println("This is the returned value: " + BytesUtils.toHexString(res.returnData()))
    }

    // verify we removed the amount from smart contract and we added it to owner (sender is not concerned)
    assertTrue(stateView.getBalance(forgerStakeMessageProcessor.fakeSmartContractAddress.address()) == validWeiAmount)
    assertTrue(stateView.getBalance(senderProposition.address()) == initialAmount.subtract(validWeiAmount.multiply(BigInteger.TWO)))
    assertTrue(stateView.getBalance(ownerAddressProposition.address()) == validWeiAmount)

    // try getting the list of stakes, no command arguments here, just op code
    val data4: Array[Byte] = new Array[Byte](0)
    val msg4 = getDefaultMessage(
      BytesUtils.fromHexString(GetListOfForgersCmd),
      data4, getRandomNonce)

    forgerStakeMessageProcessor.process(msg4, stateView) match {
      case res: InvalidMessage => Assert.fail(s"Wrong result: $res")
      case res: ExecutionFailed => Assert.fail(s"Wrong result: $res")
      case res: ExecutionSucceeded =>
        assertTrue(res.hasReturnData)
        assertTrue(res.gasUsed() == forgerStakeMessageProcessor.RemoveStakeGasPaidValue)

       val returnedList = decodeListOfForgerStake(res.returnData())
        // we should have the second stake id only
        assertTrue(returnedList.size() == 1)
        val item = returnedList.get(0)
        println("This is the returned value: " + item)

        assertTrue(BytesUtils.toHexString(item.stakeId.getValue) == BytesUtils.toHexString(forgerStakeMessageProcessor.getStakeId(msg2)))
        assertTrue(item.amount.getValue.equals(validWeiAmount))
    }

    stateView.stateDb.commit()
    stateView.close()
  }

  @Test
  def testAddStakeNotInAllowedList(): Unit = {

    val stateView = getView

    val blockSignerProposition1 = new PublicKey25519Proposition(BytesUtils.fromHexString("1100000000000000000000000000000000000000000000000000000000000011")) // 32 bytes
    val vrfPublicKey1 = new VrfPublicKey(BytesUtils.fromHexString("110000000000000000000000000000000000000000000000000000000000000011")) // 33 bytes

    val blockSignerProposition2 = new PublicKey25519Proposition(BytesUtils.fromHexString("2200000000000000000000000000000000000000000000000000000000000022")) // 32 bytes
    val vrfPublicKey2 = new VrfPublicKey(BytesUtils.fromHexString("220000000000000000000000000000000000000000000000000000000000000022")) // 33 bytes

    forgerStakeMessageProcessor.init(stateView)
    createSenderAccount(stateView)

    Mockito.when(mockNetworkParams.restrictForgers).thenReturn(true)
    Mockito.when(mockNetworkParams.allowedForgersList).thenReturn(Seq(
      (blockSignerProposition1,vrfPublicKey1),
      (blockSignerProposition2,vrfPublicKey2)
    ))


    val notAllowedBlockSignerProposition = new PublicKey25519Proposition(BytesUtils.fromHexString("ff22334455667788112233445566778811223344556677881122334455667788")) // 32 bytes
    val notAllowedVrfPublicKey = new VrfPublicKey(BytesUtils.fromHexString("ffbbccddeeff0099aabbccddeeff0099aabbccddeeff0099aabbccddeeff001234")) // 33 bytes

    val cmdInput = AddNewStakeCmdInput(
      ForgerPublicKeys(notAllowedBlockSignerProposition, notAllowedVrfPublicKey),
      ownerAddressProposition
    )

    val data: Array[Byte] = cmdInput.encode()
    val msg = getDefaultMessage(
      BytesUtils.fromHexString(AddNewStakeCmd),
      data, getRandomNonce, validWeiAmount)

    // should fail because forger is not in the allowed list
    forgerStakeMessageProcessor.process(msg, stateView) match {
      case res: ExecutionFailed =>
        assertTrue(res.gasUsed() == forgerStakeMessageProcessor.AddNewStakeGasPaidValue)
        assertTrue(res.getReason.getMessage.contains("Forger is not in the allowed list"))

      case result => Assert.fail(s"Wrong result: $result")
    }

    stateView.stateDb.commit()
    stateView.close()
  }

  @Test
  def testProcessInvalidOpCode(): Unit = {

    val stateView = getView

    forgerStakeMessageProcessor.init(stateView)

    val data: Array[Byte] = BytesUtils.fromHexString("1234567890")

    val msg = getDefaultMessage(
      BytesUtils.fromHexString("03"),
      data, getRandomNonce)


    // should fail because op code is invalid
    forgerStakeMessageProcessor.process(msg, stateView) match {
      case res: ExecutionFailed =>
        println("This is the returned value: " + res.getReason)

      case result => Assert.fail(s"Wrong result: $result")
    }

    stateView.stateDb.commit()
    stateView.close()
  }

  @Test
  def testProcessInvalidFakeSmartContractAddress(): Unit = {

    val stateView = getView

    forgerStakeMessageProcessor.init(stateView)

    val data = Bytes.concat(
      BytesUtils.fromHexString(GetListOfForgersCmd),
      new Array[Byte](0))

    val msg = new Message(
      senderProposition,
      WithdrawalMsgProcessor.fakeSmartContractAddress, // wrong address
      dummyBigInteger, dummyBigInteger, dummyBigInteger, dummyBigInteger,
      validWeiAmount, getRandomNonce, data)

    // should fail because op code is invalid
    forgerStakeMessageProcessor.process(msg, stateView) match {
      case res: InvalidMessage =>
        println("This is the returned value: " + res.getReason)

      case result => Assert.fail(s"Wrong result: $result")
    }

    stateView.stateDb.commit()
    stateView.close()
  }


  @Test
  def testAddStakeAmountNotValid(): Unit = {
    // this test will not be meaningful anymore when all sanity checks will be performed before calling any MessageProcessor

    val stateView = getView

    // create private/public key pair
    val pair = Keys.createEcKeyPair

    val blockSignerProposition1 = new PublicKey25519Proposition(BytesUtils.fromHexString("1100000000000000000000000000000000000000000000000000000000000011")) // 32 bytes
    val vrfPublicKey1 = new VrfPublicKey(BytesUtils.fromHexString("110000000000000000000000000000000000000000000000000000000000000011")) // 33 bytes

    val blockSignerProposition2 = new PublicKey25519Proposition(BytesUtils.fromHexString("2200000000000000000000000000000000000000000000000000000000000022")) // 32 bytes
    val vrfPublicKey2 = new VrfPublicKey(BytesUtils.fromHexString("220000000000000000000000000000000000000000000000000000000000000022")) // 33 bytes

    val ownerAddressProposition = new AddressProposition(BytesUtils.fromHexString(Keys.getAddress(pair)))

    forgerStakeMessageProcessor.init(stateView)

    Mockito.when(mockNetworkParams.restrictForgers).thenReturn(true)
    Mockito.when(mockNetworkParams.allowedForgersList).thenReturn(Seq(
      (blockSignerProposition1,vrfPublicKey1),
      (blockSignerProposition2,vrfPublicKey2)
    ))

    val cmdInput = AddNewStakeCmdInput(
      ForgerPublicKeys(blockSignerProposition1, vrfPublicKey1),
      ownerAddressProposition
    )
    val data: Array[Byte] = cmdInput.encode()


    val msg = getDefaultMessage(
      BytesUtils.fromHexString(AddNewStakeCmd),
      data, getRandomNonce,invalidWeiAmount)// gasLimit

    // should fail because staked amount is not a zat amount
    forgerStakeMessageProcessor.process(msg, stateView) match {
      case res: ExecutionFailed =>
        assertTrue(res.gasUsed() == forgerStakeMessageProcessor.AddNewStakeGasPaidValue)
        println("This is the returned value: " + res.getReason)

      case result => Assert.fail(s"Wrong result: $result")
    }

    stateView.stateDb.commit()
    stateView.close()
  }

  @Test
  def testAddStakeFromEmptyBalanceAccount(): Unit = {

    // this test will not be meaningful anymore when all sanity checks will be performed before calling any MessageProcessor
    val stateView = getView

    // create private/public key pair
    val pair = Keys.createEcKeyPair

    val blockSignerProposition1 = new PublicKey25519Proposition(BytesUtils.fromHexString("1100000000000000000000000000000000000000000000000000000000000011")) // 32 bytes
    val vrfPublicKey1 = new VrfPublicKey(BytesUtils.fromHexString("110000000000000000000000000000000000000000000000000000000000000011")) // 33 bytes

    val blockSignerProposition2 = new PublicKey25519Proposition(BytesUtils.fromHexString("2200000000000000000000000000000000000000000000000000000000000022")) // 32 bytes
    val vrfPublicKey2 = new VrfPublicKey(BytesUtils.fromHexString("220000000000000000000000000000000000000000000000000000000000000022")) // 33 bytes

    val ownerAddressProposition = new AddressProposition(BytesUtils.fromHexString(Keys.getAddress(pair)))

    forgerStakeMessageProcessor.init(stateView)

    Mockito.when(mockNetworkParams.restrictForgers).thenReturn(true)
    Mockito.when(mockNetworkParams.allowedForgersList).thenReturn(Seq(
      (blockSignerProposition1,vrfPublicKey1),
      (blockSignerProposition2,vrfPublicKey2)
    ))

    createSenderAccount(stateView, BigInteger.ZERO)

    val cmdInput = AddNewStakeCmdInput(
      ForgerPublicKeys(blockSignerProposition1, vrfPublicKey1),
      ownerAddressProposition
    )
    val data: Array[Byte] = cmdInput.encode()

    val msg = getDefaultMessage(
      BytesUtils.fromHexString(AddNewStakeCmd),
      data, getRandomNonce, validWeiAmount)

    // should fail because staked amount is not a zat amount
    forgerStakeMessageProcessor.process(msg, stateView) match {
      case res: ExecutionFailed =>
        assertTrue(res.gasUsed() == forgerStakeMessageProcessor.AddNewStakeGasPaidValue)
        println("This is the returned value: " + res.getReason)

      case result =>
        Assert.fail(s"Wrong result: $result")
    }

    stateView.stateDb.commit()
    stateView.close()
  }

  @Test
  def testExtraBytesInGetListCmd(): Unit = {

    val blockSignerProposition = new PublicKey25519Proposition(BytesUtils.fromHexString("1122334455667788112233445566778811223344556677881122334455667788")) // 32 bytes
    val vrfPublicKey = new VrfPublicKey(BytesUtils.fromHexString("aabbccddeeff0099aabbccddeeff0099aabbccddeeff0099aabbccddeeff001234")) // 33 bytes

    Mockito.when(mockNetworkParams.restrictForgers).thenReturn(true)
    Mockito.when(mockNetworkParams.allowedForgersList).thenReturn(Seq(
      (blockSignerProposition, vrfPublicKey)
    ))

    val stateView = getView

    forgerStakeMessageProcessor.init(stateView)

    // create sender account with some fund in it
    val initialAmount = BigInteger.valueOf(10).multiply(validWeiAmount)
    createSenderAccount(stateView, initialAmount)

    // try getting the list of stakes with some extra byte after op code (should fail)
    val data: Array[Byte] = new Array[Byte](1)
    val msg = getDefaultMessage(
      BytesUtils.fromHexString(GetListOfForgersCmd),
      data, getRandomNonce)

    forgerStakeMessageProcessor.process(msg, stateView) match {
      case res: ExecutionFailed =>
        println(res.getReason.getMessage)
      case result =>
        Assert.fail(s"Wrong result: $result")
    }

    stateView.stateDb.commit()
    stateView.close()
  }

  @Test
  def testForgerStakeLinkedList(): Unit = {

    val expectedBlockSignerProposition = "1122334455667788112233445566778811223344556677881122334455667788" // 32 bytes
    val blockSignerProposition = new PublicKey25519Proposition(BytesUtils.fromHexString("1122334455667788112233445566778811223344556677881122334455667788")) // 32 bytes
    val expectedVrfKey = "aabbccddeeff0099aabbccddeeff0099aabbccddeeff0099aabbccddeeff001234"
    val vrfPublicKey = new VrfPublicKey(BytesUtils.fromHexString(expectedVrfKey)) // 33 bytes

    Mockito.when(mockNetworkParams.restrictForgers).thenReturn(true)
    Mockito.when(mockNetworkParams.allowedForgersList).thenReturn(Seq(
      (blockSignerProposition, vrfPublicKey)
    ))

    val stateView = getView

    forgerStakeMessageProcessor.init(stateView)

    // create sender account with some fund in it
    val initialAmount = BigInteger.valueOf(10).multiply(validWeiAmount)
    createSenderAccount(stateView, initialAmount)

    val cmdInput = AddNewStakeCmdInput(
      ForgerPublicKeys(blockSignerProposition, vrfPublicKey),
      ownerAddressProposition
    )
    val data: Array[Byte] = cmdInput.encode()

    var totalForgersAmount = BigInteger.ZERO

    // add 4 forger stakes with increasing amount
    for (i <- 1 to 4) {
      val stakeAmount = validWeiAmount.multiply(BigInteger.valueOf(i))
      totalForgersAmount = totalForgersAmount.add(stakeAmount)
        val msg = getDefaultMessage(
          BytesUtils.fromHexString(AddNewStakeCmd),
          data, getRandomNonce, stakeAmount)
      forgerStakeMessageProcessor.process(msg, stateView) match {
          case res: ExecutionSucceeded =>
            assertTrue(res.hasReturnData)
            assertTrue(res.gasUsed() == forgerStakeMessageProcessor.AddNewStakeGasPaidValue)
          case result => Assert.fail(s"Wrong result: $result")
        }
    }

    var forgerList = getForgerStakeList(stateView)
    assertTrue(forgerList.size == 4)
    val listTotalAmount = forgerList.asScala.foldLeft(BigInteger.ZERO)(
      (amount, forgerStake) => forgerStake.amount.getValue.add(amount) )
    assertTrue(listTotalAmount == totalForgersAmount)

    forgerList.asScala.foreach(forgerStake => {
      val vrfKey = org.web3j.utils.Numeric.toHexStringNoPrefix(forgerStake.vrfFirst32Bytes.getValue ++ forgerStake.vrfLastByte.getValue)
      assertEquals("Wrong vrfKey",expectedVrfKey, vrfKey)
      val blockSignerProposition = org.web3j.utils.Numeric.toHexStringNoPrefix(forgerStake.blockSignPublicKey.getValue)
      assertEquals("Wrong BlockSignerProposition",expectedBlockSignerProposition, blockSignerProposition)

    })

    // remove in the middle of the list
    var pair = checkRemoveItemFromList(stateView, forgerList, 2, totalForgersAmount)
    forgerList = pair._1
    totalForgersAmount = pair._2


    // remove at the beginning of the list (head)
    pair = checkRemoveItemFromList(stateView, forgerList, 0, totalForgersAmount)
    forgerList = pair._1
    totalForgersAmount = pair._2


    // remove at the end of the list
    pair = checkRemoveItemFromList(stateView, forgerList, 1, totalForgersAmount)
    forgerList = pair._1
    totalForgersAmount = pair._2


    // remove the last element we have
    val stakeIdToRemove = forgerList.get(0).stakeId
    removeForgerStake(stateView, stakeIdToRemove.getValue)
    forgerList = getForgerStakeList(stateView)
    assertTrue(forgerList.size() == 0)

    stateView.stateDb.commit()
    stateView.stateDb.close()
  }

  def checkRemoveItemFromList(stateView: AccountStateView, inputList: java.util.List[abi.util.AccountForgingStakeInfo],
                              itemPosition: Int, totAmount: BigInteger) : (java.util.List[abi.util.AccountForgingStakeInfo], BigInteger) =
    {
      // get the info related to the item to remove
      val stakeInfo = inputList.get(itemPosition)
      val stakeIdToRemove = stakeInfo.stakeId
      val stakedAmountToRemove = stakeInfo.amount

      // call msg processor for removing the selected stake
      removeForgerStake(stateView, stakeIdToRemove.getValue)

      // call msg processor for retrieving the resulting list of forgers
      val returnedList = getForgerStakeList(stateView)

      // check the results:
      //  we removed just one element
      assertTrue(returnedList.size() == inputList.size()-1)

      // we have now the expected total forger stake amount
      val listTotalAmount = returnedList.asScala.foldLeft(BigInteger.ZERO)(
        (amount, forgerStake) => forgerStake.amount.getValue.add(amount) )
      assertTrue(listTotalAmount == totAmount.subtract(stakedAmountToRemove.getValue))

      // return the list just retrieved and the current total forgers stake
      (returnedList, listTotalAmount)
    }
}
