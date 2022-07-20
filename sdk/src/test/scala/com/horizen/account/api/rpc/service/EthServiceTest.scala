package com.horizen.account.api.rpc.service

import akka.actor.TypedActor.dispatcher
import akka.actor.{ActorRef, ActorSystem}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.horizen.{SidechainHistory, SidechainSettings}
import com.horizen.account.api.rpc.request.RpcRequest
import com.horizen.account.api.rpc.response.RpcResponseError
import com.horizen.account.api.rpc.utils.Quantity
import com.horizen.account.history.AccountHistory
import com.horizen.account.mempool.AccountMemoryPool
import com.horizen.account.node.AccountNodeView
import com.horizen.account.state.{AccountState, AccountStateView}
import com.horizen.account.wallet.AccountWallet
import com.horizen.api.http.{SidechainApiMockConfiguration, SidechainTransactionActorRef}
import com.horizen.fixtures.{FieldElementFixture, MockedSidechainNodeViewHolderFixture}
import com.horizen.params.{NetworkParams, RegTestParams}
import org.junit.Assert.{assertEquals, assertFalse}
import org.junit.{Before, Test}
import org.scalatestplus.junit.JUnitSuite
import org.scalatestplus.mockito.MockitoSugar
import org.web3j.utils.Numeric
import scorex.core.NodeViewHolder.CurrentView

import java.math.BigInteger
// TODO: we need full coverage of eth rpc service. Every method with both success and fail cases
class EthServiceTest extends JUnitSuite
  with MockitoSugar
  with MockedSidechainNodeViewHolderFixture {
  var nodeView: AccountNodeView = _
  var ethService: EthService = _
  var params: NetworkParams = _
  var transactionActorRef: ActorRef = _

  implicit val actorSystem: ActorSystem = ActorSystem("sc_nvh_mocked")

  @Before
  def setUp(): Unit = {
    val settings = mock[SidechainSettings]
    params = RegTestParams(initialCumulativeCommTreeHash = FieldElementFixture.generateFieldElement())
    transactionActorRef = mock[ActorRef]
    val nodeView = mock[CurrentView[AccountHistory, AccountState, AccountWallet, AccountMemoryPool]]
    val stateView = mock[AccountStateView]
    ethService = new EthService(stateView, nodeView, params, settings, transactionActorRef)
  }

  @Test
  def testEthService(): Unit = {
    var mapper = new ObjectMapper

    // Test 1: Parameters are of wrong type
    var json = "{\"id\":\"1\", \"jsonrpc\":\"2.0\",\"method\":\"eth_estimateGas\", \"params\":{\"tx\":\"test\", \"tx2\":\"test2\"}}"
    var request = mapper.readTree(json)
    var rpcRequest = new RpcRequest(request)
    assertEquals("Invalid params", ethService.execute(rpcRequest).asInstanceOf[RpcResponseError].getError.getMessage)

    // Test 1: Parameters are of wrong type
    json = "{\"id\":\"1\", \"jsonrpc\":\"2.0\",\"method\":\"eth_estimateGas\", \"params\":{\"tx\":\"test\", \"tx2\":\"test2\"}}"
    request = mapper.readTree(json)
    rpcRequest = new RpcRequest(request)
    assertEquals("Invalid params", ethService.execute(rpcRequest).asInstanceOf[RpcResponseError].getError.getMessage)

    // Test 2: Wrong number of parameters
    json = "{\"id\":\"1\", \"jsonrpc\":\"2.0\",\"method\":\"eth_estimateGas\", \"params\":[5, 10, 20]}}"
    request = mapper.readTree(json)
    rpcRequest = new RpcRequest(request)
    assertEquals("Invalid params", ethService.execute(rpcRequest).asInstanceOf[RpcResponseError].getError.getMessage)

    // Test 3: Request execution calls correct function and returns value correctly
    json = "{\"id\":\"1\", \"jsonrpc\":\"2.0\",\"method\":\"eth_chainId\", \"params\":[]}}"
    request = mapper.readTree(json)
    rpcRequest = new RpcRequest(request)
    assertEquals(Numeric.toHexStringWithPrefix(BigInteger.valueOf(params.chainId)), ethService.execute(rpcRequest).asInstanceOf[Quantity].getValue)
  }
}
