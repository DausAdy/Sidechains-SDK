package io.horizen.account.fork

import io.horizen.fork.{ForkManager, OptionalSidechainFork}

case class Version1_4_0Fork(active: Boolean = false) extends OptionalSidechainFork

/**
 * <p>This fork introduces the following major changes:</p>
 * <ul>
 *  <li>1. It enables new stake delegation management</li>
 *  <li>2. It enables max cap for mainchain forger reward distribution based on mainchain coinbase. </li>
 *  <li>2. It enables the minimum stake for forgers for participating in the Lottery. </li>
 * </ul>
 */
object Version1_4_0Fork {
  def get(epochNumber: Int): Version1_4_0Fork = {
    ForkManager.getOptionalSidechainFork[Version1_4_0Fork](epochNumber).getOrElse(DefaultFork)
  }

  def getActivationEpoch(): Int = {
    ForkManager.getFirstActivationEpoch[Version1_4_0Fork]()
  }

  private val DefaultFork: Version1_4_0Fork = Version1_4_0Fork()
}
