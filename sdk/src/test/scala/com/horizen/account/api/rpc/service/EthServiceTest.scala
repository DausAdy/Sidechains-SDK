package com.horizen.account.api.rpc.service

import akka.actor.{ActorRef, ActorSystem}
import com.fasterxml.jackson.databind.ObjectMapper
import com.horizen.SidechainSettings
import com.horizen.account.api.rpc.handler.RpcException
import com.horizen.account.api.rpc.request.RpcRequest
import com.horizen.account.api.rpc.types.Quantity
import com.horizen.account.node.AccountNodeView
import com.horizen.fixtures.FieldElementFixture
import com.horizen.params.{NetworkParams, RegTestParams}
import org.junit.Assert.assertEquals
import org.junit.{Before, Test}
import org.scalatestplus.junit.JUnitSuite
import org.scalatestplus.mockito.MockitoSugar
import org.web3j.utils.Numeric
import java.math.BigInteger
import scala.concurrent.duration.{FiniteDuration, SECONDS}

// TODO: we need full coverage of eth rpc service. Every method with both success and fail cases
class EthServiceTest extends JUnitSuite
  with MockitoSugar
{
  var nodeView: AccountNodeView = _
  var ethService: EthService = _
  var params: NetworkParams = _
  var transactionActorRef: ActorRef = _
  var NodeViewHolderRef: ActorRef = _
  implicit val actorSystem: ActorSystem = ActorSystem("sc_nvh_mocked")

  @Before
  def setUp(): Unit = {
    val settings = mock[SidechainSettings]
    params = RegTestParams(initialCumulativeCommTreeHash = FieldElementFixture.generateFieldElement())
    transactionActorRef = mock[ActorRef]
    NodeViewHolderRef = mock[ActorRef]

    ethService = new EthService(NodeViewHolderRef, new FiniteDuration(10, SECONDS), params, settings, transactionActorRef)
  }

  @Test
  def testEthService(): Unit = {
    val mapper = new ObjectMapper

    // Test 1: Parameters are of wrong type
    var json = "{\"id\":\"1\", \"jsonrpc\":\"2.0\",\"method\":\"eth_estimateGas\", \"params\":{\"tx\":\"test\", \"tx2\":\"test2\"}}"
    var request = mapper.readTree(json)
    var rpcRequest = new RpcRequest(request)
    assertThrows[RpcException] {
      ethService.execute(rpcRequest)
    }

    // Test 1: Parameters are of wrong type
    json = "{\"id\":\"1\", \"jsonrpc\":\"2.0\",\"method\":\"eth_estimateGas\", \"params\":{\"tx\":\"test\", \"tx2\":\"test2\"}}"
    request = mapper.readTree(json)
    rpcRequest = new RpcRequest(request)
    assertThrows[RpcException] {
      ethService.execute(rpcRequest)
    }

    // Test 2: Wrong number of parameters
    json = "{\"id\":\"1\", \"jsonrpc\":\"2.0\",\"method\":\"eth_estimateGas\", \"params\":[5, 10, 20]}}"
    request = mapper.readTree(json)
    rpcRequest = new RpcRequest(request)
    assertThrows[RpcException] {
      ethService.execute(rpcRequest)
    }

    // Test 3: Request execution calls correct function and returns value correctly
    json = "{\"id\":\"1\", \"jsonrpc\":\"2.0\",\"method\":\"eth_chainId\", \"params\":[]}}"
    request = mapper.readTree(json)
    rpcRequest = new RpcRequest(request)
    assertEquals(Numeric.toHexStringWithPrefix(BigInteger.valueOf(params.chainId)), ethService.execute(rpcRequest).asInstanceOf[Quantity].value)

    // Test 4: Trigger IllegalArgumentException rpc call
    json = "{\"id\":\"1\", \"jsonrpc\":\"2.0\",\"method\":\"eth_estimateGas\", \"params\":[-1]}}"
    request = mapper.readTree(json)
    rpcRequest = new RpcRequest(request)
    assertThrows[RpcException] {
      ethService.execute(rpcRequest)
    }
  }

  @Test
  def invalidJsonRpcData(): Unit = {
    val mapper = new ObjectMapper

    // Try to read json request with missing data
    val json = "{\"id\":\"1\", \"jsonrpc\":\"2.0\", \"params\":[]}}"
    val request = mapper.readTree(json)
    assertThrows[RpcException] {
      val rpcRequest = new RpcRequest(request)
    }
  }
}
