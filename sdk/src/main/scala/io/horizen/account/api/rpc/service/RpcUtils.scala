package io.horizen.account.api.rpc.service

import scala.util.Try

object RpcUtils {

  def getClientVersion(appVersion: String): String = {
    val default = "dev"
    val architecture = Try(System.getProperty("os.arch")).getOrElse(default)
    val javaVersion = Try(System.getProperty("java.specification.version")).getOrElse(default)
    val sdkPackage = this.getClass.getPackage
    val sdkVersion = sdkPackage.getImplementationVersion
    s"$appVersion/$sdkVersion/$architecture/jdk$javaVersion"
  }
}
