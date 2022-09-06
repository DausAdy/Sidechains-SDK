package com.horizen.account.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import com.fasterxml.jackson.annotation.JsonView
import com.horizen.SidechainTypes
import com.horizen.api.http.JacksonSupport._
import com.horizen.account.block.{AccountBlock, AccountBlockHeader}
import com.horizen.account.node.{AccountNodeView, NodeAccountHistory, NodeAccountMemoryPool, NodeAccountState}
import com.horizen.api.http.{ApiResponseUtil, BlockBaseApiRoute, SuccessResponse}
import com.horizen.account.api.http.AccountBlockRestSchema._
import com.horizen.account.chain.AccountFeePaymentsInfo
import com.horizen.account.utils.AccountBlockFeeInfo
import com.horizen.node.NodeWalletBase
import com.horizen.serialization.Views
import scorex.core.settings.RESTApiSettings

import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters._
import scala.concurrent.ExecutionContext


case class AccountBlockApiRoute(override val settings: RESTApiSettings,
                                  sidechainNodeViewHolderRef: ActorRef,
                                  sidechainBlockActorRef: ActorRef, forgerRef: ActorRef)(implicit override val context: ActorRefFactory, override val ec: ExecutionContext)
  extends BlockBaseApiRoute[
    SidechainTypes#SCAT,
    AccountBlockHeader,
    AccountBlock,
    AccountFeePaymentsInfo,
    NodeAccountHistory,
    NodeAccountState,
    NodeWalletBase,
    NodeAccountMemoryPool,
    AccountNodeView] (settings, sidechainBlockActorRef: ActorRef, forgerRef: ActorRef) {

  override val route: Route = pathPrefix("block") {
    findById ~ findLastIds ~ findIdByHeight ~ getBestBlockInfo ~ getFeePayments ~ startForging ~ stopForging ~ generateBlockForEpochNumberAndSlot ~ getForgingInfo
  }


  /**
   * Return the list of forgers fee payments paid after the given block was applied.
   * Return empty list in case no fee payments were paid.
   */
  def getFeePayments: Route = (post & path("getFeePayments")) {
    entity(as[ReqFeePayments]) { body =>
      applyOnNodeView { sidechainNodeView =>
        val sidechainHistory = sidechainNodeView.getNodeHistory
        //val qqq = sidechainHistory.getFeePaymentsInfo(body.blockId).asScala.map(_.transaction.newBoxes().asScala).getOrElse(Seq())
        val accountState = sidechainNodeView.getNodeState
        val withdrawalEpochNumber: Int = accountState.getWithdrawalEpochInfo.epoch

        val feePayments = accountState.getFeePayments(withdrawalEpochNumber)
        ApiResponseUtil.toResponse(RespFeePayments(feePayments.toList))
      }
    }
  }
}


object AccountBlockRestSchema {

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class ReqFeePayments(blockId: String) {
    require(blockId.length == 64, s"Invalid id $blockId. Id length must be 64")
  }

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class RespFeePayments(feePayments: Seq[AccountBlockFeeInfo]) extends SuccessResponse
}

object SidechainBlockErrorResponse {

}