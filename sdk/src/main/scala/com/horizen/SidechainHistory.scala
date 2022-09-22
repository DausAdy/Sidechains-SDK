package com.horizen

import java.util.{Optional => JOptional}
import com.horizen.block.{SidechainBlock, SidechainBlockHeader}
import com.horizen.consensus._
import com.horizen.node.NodeHistory
import com.horizen.params.{NetworkParams, NetworkParamsUtils}
import com.horizen.storage.SidechainHistoryStorage
import com.horizen.validation.{HistoryBlockValidator, SemanticBlockValidator}
import scorex.util.{ModifierId, ScorexLogging}
import scala.util.Try


class SidechainHistory private (storage: SidechainHistoryStorage,
                                consensusDataStorage: ConsensusDataStorage,
                                params: NetworkParams,
                                semanticBlockValidators: Seq[SemanticBlockValidator[SidechainBlock]],
                                historyBlockValidators: Seq[HistoryBlockValidator[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistoryStorage, SidechainHistory]])
  extends AbstractHistory[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistoryStorage, SidechainHistory](
    storage, consensusDataStorage, params, semanticBlockValidators, historyBlockValidators)

  with NetworkParamsUtils
  with ConsensusDataProvider
  with sparkz.core.utils.SparkzEncoding
  with NodeHistory
  with ScorexLogging
{

  override type NVCT = SidechainHistory

  override def makeNewHistory(storage: SidechainHistoryStorage, consensusDataStorage: ConsensusDataStorage): SidechainHistory =
      new SidechainHistory(storage, consensusDataStorage, params, semanticBlockValidators, historyBlockValidators)

  override def searchTransactionInsideSidechainBlock(transactionId: String, blockId: String): JOptional[SidechainTypes#SCBT] = {
    storage.blockById(ModifierId(blockId)) match {
      case Some(scBlock) => findTransactionInsideBlock(transactionId, scBlock)
      case None => JOptional.empty()
    }
  }

  private def findTransactionInsideBlock(transactionId : String, block : SidechainBlock) : JOptional[SidechainTypes#SCBT] = {
    block.transactions.find(box => box.id.equals(ModifierId(transactionId))) match {
      case Some(tx) => JOptional.ofNullable(tx)
      case None => JOptional.empty()
    }
  }

  override def searchTransactionInsideBlockchain(transactionId: String): JOptional[SidechainTypes#SCBT] = {
    var startingBlock = JOptional.ofNullable(getBestBlock)
    var transaction : JOptional[SidechainTypes#SCBT] = JOptional.empty()
    var found = false
    while(!found && startingBlock.isPresent){
      val tx = findTransactionInsideBlock(transactionId, startingBlock.get())
      if(tx.isPresent){
        found = true
        transaction = JOptional.ofNullable(tx.get())
      }else{
        startingBlock = storage.parentBlockId(startingBlock.get().id) match {
          case Some(id) => storage.blockById(id) match {
            case Some(block) => JOptional.ofNullable(block)
            case None => JOptional.empty()
          }
          case None => JOptional.empty()
        }
      }
    }

    transaction
  }

}

object SidechainHistory
{
  private[horizen] def restoreHistory(historyStorage: SidechainHistoryStorage,
                                      consensusDataStorage: ConsensusDataStorage,
                                      params: NetworkParams,
                                      semanticBlockValidators: Seq[SemanticBlockValidator[SidechainBlock]],
                                      historyBlockValidators: Seq[HistoryBlockValidator[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistoryStorage, SidechainHistory]]): Option[SidechainHistory] = {

    if (!historyStorage.isEmpty)
      Some(new SidechainHistory(historyStorage, consensusDataStorage, params, semanticBlockValidators, historyBlockValidators))
    else
      None
  }



  private[horizen] def createGenesisHistory(historyStorage: SidechainHistoryStorage,
                                      consensusDataStorage: ConsensusDataStorage,
                                      params: NetworkParams,
                                      genesisBlock: SidechainBlock,
                                      semanticBlockValidators: Seq[SemanticBlockValidator[SidechainBlock]],
                                      historyBlockValidators: Seq[HistoryBlockValidator[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistoryStorage, SidechainHistory]],
                                      stakeEpochInfo: StakeConsensusEpochInfo) : Try[SidechainHistory] = {
    if (historyStorage.isEmpty) {
      val nonceEpochInfo = ConsensusDataProvider.calculateNonceForGenesisBlock(params)
      new SidechainHistory(historyStorage, consensusDataStorage, params, semanticBlockValidators, historyBlockValidators)
        .append(genesisBlock).map(_._1).get.reportModifierIsValid(genesisBlock)
        .map(_.applyFullConsensusInfo(genesisBlock.id, FullConsensusEpochInfo(stakeEpochInfo, nonceEpochInfo)))
    }
    else
      throw new RuntimeException("History storage is not empty!")
  }
}
