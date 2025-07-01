package io.horizen.account.fork

import io.horizen.fork.{ForkManager, OptionalSidechainFork}

case class Version1_5_0Fork(active: Boolean = false) extends OptionalSidechainFork

/**
 * <p>This fork introduces the following major changes:</p>
 * <ul>
 *  <li>1. It disables the backward transfers submissions as part of the strategy which stops any cross-chain transfer operation between Mainchain and Sidechain with a view to EON2 migration.</li>
 * </ul>
 */
object Version1_5_0Fork {
  def get(epochNumber: Int): Version1_5_0Fork = {
    ForkManager.getOptionalSidechainFork[Version1_5_0Fork](epochNumber).getOrElse(DefaultFork)
  }

  def getActivationEpoch(): Int = {
    ForkManager.getFirstActivationEpoch[Version1_5_0Fork]()
  }

  private val DefaultFork: Version1_5_0Fork = Version1_5_0Fork()
}
