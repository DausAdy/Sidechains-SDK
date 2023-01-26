package com.horizen.account.api.http

import akka.http.scaladsl.model.{ContentTypes, HttpMethods, StatusCodes}
import akka.http.scaladsl.server.{MalformedRequestContentRejection, MethodRejection, Route}
import com.horizen.account.api.http.AccountTransactionRestScheme._
import com.horizen.account.utils.FeeUtils
import com.horizen.api.http.TransactionBaseRestScheme.ReqSendTransaction
import com.horizen.proposition.VrfPublicKey
import com.horizen.serialization.SerializationUtil
import com.horizen.utils.BytesUtils
import org.junit.Assert._
import org.mockito.Mockito

import java.math.BigInteger
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.util.Random

class AccountTransactionApiRouteTest extends AccountSidechainApiRouteTest {

  override val basePath = "/transaction/"

  "The Api should" should {

    "reject and reply with http error" in {
      Get(basePath) ~> sidechainTransactionApiRoute ~> check {
        rejection shouldBe MethodRejection(HttpMethods.POST)
      }
      Get(basePath) ~> Route.seal(sidechainTransactionApiRoute) ~> check {
        status.intValue() shouldBe StatusCodes.MethodNotAllowed.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
      }

      Post(basePath + "allTransactions").withEntity("maybe_a_json") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "allTransactions").withEntity("maybe_a_json") ~> Route.seal(sidechainTransactionApiRoute) ~> check {
        status.intValue() shouldBe StatusCodes.BadRequest.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
      }


      Post(basePath + "createLegacyEIP155Transaction").withHeaders(apiTokenHeader) ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "createLegacyEIP155Transaction").withHeaders(apiTokenHeader).withEntity("maybe_a_json") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "createLegacyEIP155Transaction").withHeaders(apiTokenHeader) ~> Route.seal(sidechainTransactionApiRoute) ~> check {
        status.intValue() shouldBe StatusCodes.BadRequest.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
      }
      Post(basePath + "createLegacyEIP155Transaction").withHeaders(badApiTokenHeader).withEntity("maybe_a_json") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }

      Post(basePath + "makeForgerStake") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "makeForgerStake").withEntity("maybe_a_json") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "makeForgerStake").withHeaders(badApiTokenHeader).withEntity("maybe_a_json") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }

      Post(basePath + "spendForgingStake") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "spendForgingStake").withEntity("maybe_a_json") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "spendForgingStake").withHeaders(apiTokenHeader) ~> Route.seal(sidechainTransactionApiRoute) ~> check {
        status.intValue() shouldBe StatusCodes.BadRequest.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
      }

      Post(basePath + "withdrawCoins") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "withdrawCoins").withEntity("maybe_a_json") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "withdrawCoins").withHeaders(apiTokenHeader) ~> Route.seal(sidechainTransactionApiRoute) ~> check {
        status.intValue() shouldBe StatusCodes.BadRequest.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
      }

      Post(basePath + "createSmartContract") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "createSmartContract").withEntity("maybe_a_json") ~> sidechainTransactionApiRoute ~> check {
        rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName.toString)
      }
      Post(basePath + "createSmartContract").withHeaders(apiTokenHeader) ~> Route.seal(sidechainTransactionApiRoute) ~> check {
        status.intValue() shouldBe StatusCodes.BadRequest.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
      }
    }

    "reply at /allTransactions" in {
      // parameter 'format' = true
      Post(basePath + "allTransactions")
        .withEntity(SerializationUtil.serialize(ReqAllTransactions(None))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals(1, result.elements().asScala.length)
        assertTrue(result.get("transactions").isArray)
        assertEquals(memoryPool.size(), result.get("transactions").elements().asScala.length)
        val transactionJsonNode = result.get("transactions").elements().asScala.toList
        for (i <- 0 to transactionJsonNode.size - 1)
          jsonChecker.assertsOnAccountTransactionJson(transactionJsonNode(i), memoryPool.get(i))
      }
      // parameter 'format' = false
      Post(basePath + "allTransactions")
        .withEntity(SerializationUtil.serialize(ReqAllTransactions(Some(false)))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals(1, result.elements().asScala.length)
        assertTrue(result.get("transactionIds").isArray)
        assertEquals(memoryPool.size(), result.get("transactionIds").elements().asScala.length)
        val transactionIdsJsonNode = result.get("transactionIds").elements().asScala.toList
        for (i <- 0 to transactionIdsJsonNode.size - 1)
          assertEquals(memoryPool.get(i).id, transactionIdsJsonNode(i).asText())
      }
    }

    "reply at /createLegacyEIP155Transaction" in {
      Post(basePath + "createLegacyEIP155Transaction")
        .withHeaders(apiTokenHeader)
        .withEntity(SerializationUtil.serialize(ReqLegacyTransaction(Option.apply("1234567890123456789012345678901234567890"),
          Option.apply("2234567890123456789012345678901234567890"), Some(BigInteger.ONE),
          BigInteger.valueOf(FeeUtils.GAS_LIMIT.longValue()), BigInteger.ONE, Option.apply(BigInteger.ONE),
          ""))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
      }
    }

    "reply at /allWithdrawalRequests" in {
      val epochNum = 102
      Post(basePath + "allWithdrawalRequests").withEntity(SerializationUtil.serialize(ReqAllWithdrawalRequests(epochNum))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals(1, result.elements().asScala.length)
        assertTrue(result.get("listOfWR").isArray)
        assertEquals(1, result.get("listOfWR").elements().asScala.length)
        val stakesJsonNode = result.get("listOfWR").elements().asScala.toList
        for (i <- 0 to stakesJsonNode.size - 1)
          jsonChecker.assertsOnWithdrawalRequestJson(stakesJsonNode(i), utilMocks.listOfWithdrawalRequests(i))
      }
    }

    "reply at /withdrawCoins" in {
      val amountInZennies = 32
      val mcAddr = BytesUtils.toHorizenPublicKeyAddress(utilMocks.getMCPublicKeyHashProposition.bytes(),params)
      Post(basePath + "withdrawCoins").withHeaders(apiTokenHeader).withEntity(SerializationUtil.serialize(ReqWithdrawCoins(Some(BigInteger.ONE),
        TransactionWithdrawalRequest(mcAddr, amountInZennies), None))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals(1, result.elements().asScala.length)
        assertTrue(result.get("transactionId").isTextual)
      }
    }


    "reply at /allForgingStakes" in {
      Post(basePath + "allForgingStakes") ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals(1, result.elements().asScala.length)
        assertTrue(result.get("stakes").isArray)
        assertEquals(utilMocks.listOfStakes.size, result.get("stakes").elements().asScala.length)
        val stakesJsonNode = result.get("stakes").elements().asScala.toList
        for (i <- 0 to stakesJsonNode.size - 1)
          jsonChecker.assertsOnAccountStakeInfoJson(stakesJsonNode(i), utilMocks.listOfStakes(i))
      }
    }

    "reply at /makeForgerStake" in {
      val stakeAmountInZennies = 32

      Post(basePath + "makeForgerStake").withHeaders(apiTokenHeader).withEntity(SerializationUtil.serialize(ReqCreateForgerStake(Some(BigInteger.ONE),
        TransactionForgerOutput(utilMocks.ownerPublicKeyString, Some(utilMocks.blockSignerPropositionString), utilMocks.vrfPublicKeyString, stakeAmountInZennies), None))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals(1, result.elements().asScala.length)
        assertTrue(result.get("transactionId").isTextual)
      }
    }

    "reply at /spendForgingStake" in {
      val outAsTransactionObj = ReqSpendForgingStake(Some(BigInteger.ONE),
        utilMocks.stakeId, None)

      Post(basePath + "spendForgingStake").withHeaders(apiTokenHeader).withEntity(SerializationUtil.serialize(outAsTransactionObj)) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals(1, result.elements().asScala.length)
        assertTrue(result.get("transactionId").isTextual)

      }
    }

    // setup a mocked conf with a restricted forger list
    Mockito.when(params.restrictForgers).thenReturn(true)
    val blockSignerProposition = utilMocks.signerSecret.publicImage()
    val vrfPublicKey = new VrfPublicKey(BytesUtils.fromHexString("aabbccddeeff0099aabbccddeeff0099aabbccddeeff0099aabbccddeeff001234")) // 33 bytes
    Mockito.when(params.allowedForgersList).thenReturn(Seq((blockSignerProposition, vrfPublicKey)))

    "reply at /openForgerList" in {
      val outAsTransactionObj = ReqOpenStakeForgerList(Some(BigInteger.ONE), utilMocks.forgerIndex, None)

      Post(basePath + "openForgerList").withHeaders(apiTokenHeader).withEntity(SerializationUtil.serialize(outAsTransactionObj)) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        if (result == null)
          fail(s"Serialization failed for object SidechainApiResponseBody: ${mapper.readTree(entityAs[String])}")

        assertEquals(1, result.elements().asScala.length)
        assertTrue(result.get("transactionId").isTextual)

      }
    }

    "reply at /createSmartContract" in {
      val contractCodeBytes = new Array[Byte](100)
      Random.nextBytes(contractCodeBytes)
      val contractCode = BytesUtils.toHexString(contractCodeBytes)
      Post(basePath + "createSmartContract").withHeaders(apiTokenHeader)
        .withEntity(SerializationUtil.serialize(ReqCreateContract(Some(BigInteger.ONE),
          contractCode, None))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals(1, result.elements().asScala.length)
        assertTrue(result.get("transactionId").isTextual)
      }
    }

    "reply at /createEIP1559Transaction" in {
      Post(basePath + "createEIP1559Transaction").withHeaders(apiTokenHeader)
        .withEntity(SerializationUtil.serialize(ReqEIP1559Transaction(Option.apply("1234567890123456789012345678901234567890"),
          Option.apply("2234567890123456789012345678901234567890"), Option.apply(BigInteger.ONE),
          BigInteger.valueOf(FeeUtils.GAS_LIMIT.longValue()), BigInteger.ONE, BigInteger.ONE,
          Option.apply(BigInteger.ONE), "")))~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("error")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals("0205", result.get("code").asText())
      }
    }

    "reply at /createLegacyTransaction" in {
      Post(basePath + "createLegacyTransaction").withHeaders(apiTokenHeader)
        .withEntity(SerializationUtil.serialize(ReqLegacyTransaction(Option.apply("1234567890123456789012345678901234567890"),
          Option.apply("2234567890123456789012345678901234567890"), Some(BigInteger.ONE),
          BigInteger.valueOf(FeeUtils.GAS_LIMIT.longValue()), BigInteger.ONE, Option.apply(BigInteger.ONE),
          ""))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("error")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals("0205", result.get("code").asText())
      }
    }

    "reply at /sendTransaction" in {
      //sdk/target/test-classes/ethereumtransaction_eoa2eoa_legacy_signed_hex
      Post(basePath + "sendTransaction").withHeaders(apiTokenHeader)
        .withEntity(SerializationUtil.serialize(ReqSendTransaction(
          "01d601f86946824ce38252089470997970c51812dc3a010c7d01b50e0d17dc79c8888ac7230489e80000801ca02a4afbdd7e8d99c3df9dfd9e4ecd0afe018d8dec0b8b5fe1a44d5f30e7d0a5c5a07ca554a8317ff86eb6b23d06fa210d23e551bed58f58f803a87e5950aa47a9e9"
        ))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        if (result == null)
          fail("Serialization failed for objecgit SidechainApiResponseBody")

        assertEquals("6d4f0bc052e919019d392debb1a39724baa85b3d9c54836e9892170180793613", result.get("transactionId").asText())
      }
    }

    "reply at /signTransaction" in {
      //sdk/target/test-classes/ethereumtransaction_eoa2eoa_legacy_unsigned_hex
      // with insufficient balance on given 'from' address
      Post(basePath + "signTransaction").withHeaders(apiTokenHeader)
        .withEntity(SerializationUtil.serialize(ReqSignTransaction(Option.apply("dafea492d9c6733ae3d56b7ed1adb60692c98bc5"),
          "014ee646824ce38252089470997970c51812dc3a010c7d01b50e0d17dc79c8888ac7230489e8000080"))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("error")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals("0205", result.get("code").asText())
      }
    }

    "reply at /createKeyRotationTransaction" in {
      Post(basePath + "createKeyRotationTransaction").withHeaders(apiTokenHeader)
        .withEntity(SerializationUtil.serialize(ReqCreateKeyRotationTransaction(1, 0, "123",
          "123", "123", "123",
          Option.apply(BigInteger.ONE), Option.apply(EIP1559GasInfo(
            BigInteger.valueOf(FeeUtils.GAS_LIMIT.longValue()), BigInteger.ONE, BigInteger.ONE)
          )))) ~> sidechainTransactionApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("error")
        if (result == null)
          fail("Serialization failed for object SidechainApiResponseBody")

        assertEquals("0404", result.get("code").asText())
      }
    }
  }
}
