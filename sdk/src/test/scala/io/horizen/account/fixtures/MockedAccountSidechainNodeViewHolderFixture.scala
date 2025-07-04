package io.horizen.account.fixtures

import akka.actor.{ActorRef, ActorSystem, Props}
import io.horizen._
import io.horizen.account.AccountSidechainNodeViewHolder
import io.horizen.account.history.AccountHistory
import io.horizen.account.mempool.AccountMemoryPool
import io.horizen.account.state.AccountState
import io.horizen.account.wallet.AccountWallet
import org.mockito.Mockito
import org.scalatestplus.mockito.MockitoSugar
import sparkz.core.block.Block.Timestamp
import sparkz.core.settings.{NetworkSettings, SparkzSettings}

class MockedAccountSidechainNodeViewHolder(sidechainSettings: SidechainSettings,
                                    history: AccountHistory,
                                    state: AccountState,
                                    wallet: AccountWallet,
                                    mempool: AccountMemoryPool)
  extends AccountSidechainNodeViewHolder(
    sidechainSettings,
    null,
    null,
    null,
    null,
    null,
    null,
    null,
    null,
    null ) {

  override def dumpStorages(): Unit = {}

  override def getConsensusEpochNumber(timestamp: Timestamp) : Int = 100

  override def restoreState(): Option[(HIS, MS, VL, MP)] = {
    Some(history, state, wallet, mempool)
  }
}


trait MockedAccountSidechainNodeViewHolderFixture extends MockitoSugar {
  val maxModifiersCacheSize = 100

  def getMockedAccountSidechainNodeViewHolderRef(
                                           history: AccountHistory,
                                           state: AccountState,
                                           wallet: AccountWallet,
                                           mempool: AccountMemoryPool,
                                           sidechainSettings: SidechainSettings = mock[SidechainSettings])
                                                (implicit actorSystem: ActorSystem): ActorRef = {
    val sparkzSettings = mock[SparkzSettings]
    val networkSettings = mock[NetworkSettings]
    val walletSettings = mock[WalletSettings]
    Mockito.when(sidechainSettings.sparkzSettings)
      .thenAnswer(_ => {
        sparkzSettings
      })
    Mockito.when(sparkzSettings.network)
      .thenAnswer(_ => {
      networkSettings
    })
    Mockito.when(networkSettings.maxModifiersCacheSize)
      .thenAnswer(_ => {
      maxModifiersCacheSize
    })
    Mockito.when(sidechainSettings.wallet)
      .thenAnswer(_ => {
        walletSettings
      })
    Mockito.when(sidechainSettings.evmStateDump)
      .thenAnswer(_ => {
        EvmStateDump(false)
      })

    actorSystem.actorOf(Props(new MockedAccountSidechainNodeViewHolder(sidechainSettings, history, state, wallet, mempool)))
  }
}
