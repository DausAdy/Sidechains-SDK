package io.horizen.fork

class SimpleForkConfigurator extends ForkConfigurator {
  override val forkActivation: SidechainForkConsensusEpoch = SidechainForkConsensusEpoch(10, 20, 0)
}
