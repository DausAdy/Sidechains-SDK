package com.horizen.account.state

import com.horizen.evm.interop.EvmLog
import com.horizen.state.BaseStateReader

import java.math.BigInteger
import scala.util.Try

trait AccountStateReader extends BaseStateReader {
  def getCodeHash(address: Array[Byte]): Array[Byte]
  def getBalance(address: Array[Byte]): BigInteger
  def getNonce(address: Array[Byte]): BigInteger
  def getAccountStateRoot: Array[Byte] // 32 bytes, kessack hash
  def getLogs(txHash: Array[Byte]): Array[EvmLog]
  def getHeight : Int
}
