package com.horizen.account.api.http

import akka.http.scaladsl.model.{ContentTypes, HttpMethods, StatusCodes}
import akka.http.scaladsl.server.{MalformedRequestContentRejection, MethodRejection, Route}
import com.horizen.account.api.http.AccountWalletRestScheme.ReqGetBalance
import com.horizen.serialization.SerializationUtil
import org.junit.Assert.assertTrue
import org.scalatest.prop.TableDrivenPropertyChecks

class AccountWalletApiRouteTest extends AccountSidechainApiRouteTest with TableDrivenPropertyChecks {
  override val basePath = "/wallet/"

  "The Api should" should {

    "reject and reply with http error" in {
      Get(basePath) ~> sidechainWalletApiRoute ~> check {
        rejection shouldBe MethodRejection(HttpMethods.POST)
      }
      Get(basePath) ~> Route.seal(sidechainWalletApiRoute) ~> check {
        status.intValue() shouldBe StatusCodes.MethodNotAllowed.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
      }

      val cases = Table(
        ("Route", "Expected response code"),
        ("createPrivateKey25519", StatusCodes.InternalServerError.intValue),
        ("createVrfSecret", StatusCodes.InternalServerError.intValue),
        ("allPublicKeys", StatusCodes.OK.intValue),
        ("createPrivateKeySecp256k1", StatusCodes.InternalServerError.intValue),
        ("getBalance", StatusCodes.BadRequest.intValue),
        ("getTotalBalance", StatusCodes.OK.intValue)
      )

      forAll(cases) { (route, expectedCode) =>
        val path = basePath + route

        if (expectedCode == StatusCodes.BadRequest.intValue) {
          Post(path).withHeaders(apiTokenHeader) ~> sidechainWalletApiRoute ~> check {
            rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName)
          }
          Post(path).withHeaders(apiTokenHeader).withEntity("maybe_a_json") ~> sidechainWalletApiRoute ~> check {
            rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName)
          }
        }

        Post(path).withHeaders(apiTokenHeader) ~> Route.seal(sidechainWalletApiRoute) ~> check {
          status.intValue() shouldBe expectedCode
          if (expectedCode != StatusCodes.InternalServerError.intValue) {
            responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
          }
        }

        Post(path).withHeaders(badApiTokenHeader).withEntity("maybe_a_json") ~> sidechainWalletApiRoute ~> check {
          rejection.getClass.getCanonicalName.contains(MalformedRequestContentRejection.getClass.getCanonicalName)
        }
      }
    }

    "reply at /getBalance" in {
      val path = basePath + "getBalance"

      Post(path).withHeaders(apiTokenHeader).withEntity(SerializationUtil.serialize(ReqGetBalance("123"))) ~> sidechainWalletApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("error")
        assertTrue(result != null)
      }

      Post(path).withHeaders(apiTokenHeader).withEntity(SerializationUtil.serialize(ReqGetBalance("1234567890123456789012345678901234567890"))) ~> sidechainWalletApiRoute ~> check {
        status.intValue() shouldBe StatusCodes.OK.intValue
        responseEntity.getContentType() shouldEqual ContentTypes.`application/json`
        val result = mapper.readTree(entityAs[String]).get("result")
        assertTrue(result != null)
      }
    }
  }
}
