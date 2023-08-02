package io.horizen.utxo

import com.google.inject.Provides
import com.google.inject.name.Named
import io.horizen.fork.{ConsensusParamsFork, ForkConfigurator, OptionalSidechainFork, SidechainForkConsensusEpoch}
import io.horizen.helper.{SecretSubmitHelper, SecretSubmitHelperImpl}
import io.horizen.secret.SecretSerializer
import io.horizen.storage.Storage
import io.horizen.transaction.TransactionSerializer
import io.horizen.utils.Pair
import io.horizen.utxo.api.http.SidechainApplicationApiGroup
import io.horizen.utxo.box.BoxSerializer
import io.horizen.utxo.helper.{NodeViewHelper, NodeViewHelperImpl, TransactionSubmitHelper, TransactionSubmitHelperImpl}
import io.horizen.utxo.state.ApplicationState
import io.horizen.utxo.wallet.ApplicationWallet
import io.horizen.{AbstractSidechainApp, SidechainAppStopper, SidechainSettings, SidechainTypes}

import java.lang.{Byte => JByte}
import java.util.{HashMap => JHashMap, List => JList}
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

abstract class SidechainAppModule extends com.google.inject.AbstractModule {

  var app: SidechainApp = null

  def calculateMaxSlotsInEpoch(forks: java.util.List[Pair[SidechainForkConsensusEpoch, OptionalSidechainFork]]) : Int = {

    //Get the max number of consensus slots per epoch from the App Fork configurator and use it to set the Storage versions to mantain
    var maxConsensusSlotsInEpoch = ConsensusParamsFork.DefaultConsensusParamsFork.consensusSlotsInEpoch
    forks.foreach(fork => {
      if (fork.getValue.isInstanceOf[ConsensusParamsFork] && fork.getValue.asInstanceOf[ConsensusParamsFork].consensusSlotsInEpoch > maxConsensusSlotsInEpoch) {
        maxConsensusSlotsInEpoch = fork.getValue.asInstanceOf[ConsensusParamsFork].consensusSlotsInEpoch
      }
    })
    maxConsensusSlotsInEpoch
  }

  override def configure(): Unit = {

    bind(classOf[NodeViewHelper])
      .to(classOf[NodeViewHelperImpl])

    bind(classOf[TransactionSubmitHelper])
      .to(classOf[TransactionSubmitHelperImpl])

    bind(classOf[AbstractSidechainApp])
      .to(classOf[SidechainApp])

    bind(classOf[SecretSubmitHelper])
      .to(classOf[SecretSubmitHelperImpl])

    configureApp()
  }

  def configureApp(): Unit

  @Provides
  def get(
           @Named("SidechainSettings") sidechainSettings: SidechainSettings,
           @Named("CustomBoxSerializers") customBoxSerializers: JHashMap[JByte, BoxSerializer[SidechainTypes#SCB]],
           @Named("CustomSecretSerializers")  customSecretSerializers: JHashMap[JByte, SecretSerializer[SidechainTypes#SCS]],
           @Named("CustomTransactionSerializers")  customTransactionSerializers: JHashMap[JByte, TransactionSerializer[SidechainTypes#SCBT]],
           @Named("ApplicationWallet")  applicationWallet: ApplicationWallet,
           @Named("ApplicationState")  applicationState: ApplicationState,
           @Named("SecretStorage")  secretStorage: Storage,
           @Named("WalletBoxStorage")  walletBoxStorage: Storage,
           @Named("WalletTransactionStorage")  walletTransactionStorage: Storage,
           @Named("StateStorage")  stateStorage: Storage,
           @Named("StateForgerBoxStorage") forgerBoxStorage: Storage,
           @Named("StateUtxoMerkleTreeStorage") utxoMerkleTreeStorage: Storage,
           @Named("HistoryStorage")  historyStorage: Storage,
           @Named("WalletForgingBoxesInfoStorage")  walletForgingBoxesInfoStorage: Storage,
           @Named("WalletCswDataStorage") walletCswDataStorage: Storage,
           @Named("ConsensusStorage")  consensusStorage: Storage,
           @Named("BackupStorage")  backUpStorage: Storage,
           @Named("CustomApiGroups")  customApiGroups: JList[SidechainApplicationApiGroup],
           @Named("RejectedApiPaths")  rejectedApiPaths : JList[Pair[String, String]],
           @Named("ApplicationStopper") applicationStopper : SidechainAppStopper,
           @Named("ForkConfiguration") forkConfigurator : ForkConfigurator,
           @Named("AppVersion") appVersion: String
  ): SidechainApp = {
    synchronized {
      if (app == null) {
        app = new SidechainApp(
          sidechainSettings,
          customBoxSerializers,
          customSecretSerializers,
          customTransactionSerializers,
          applicationWallet,
          applicationState,
          secretStorage,
          walletBoxStorage,
          walletTransactionStorage,
          stateStorage,
          forgerBoxStorage,
          utxoMerkleTreeStorage,
          historyStorage,
          walletForgingBoxesInfoStorage,
          walletCswDataStorage,
          consensusStorage,
          backUpStorage,
          customApiGroups,
          rejectedApiPaths,
          applicationStopper,
          forkConfigurator,
          appVersion
        )
      }
    }
    app
  }

}
