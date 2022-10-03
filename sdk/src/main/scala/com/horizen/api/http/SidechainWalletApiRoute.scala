package com.horizen.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import com.fasterxml.jackson.annotation.JsonView
import com.horizen.AbstractSidechainNodeViewHolder.ReceivableMessages.LocallyGeneratedSecret
import com.horizen.api.http.JacksonSupport._
import com.horizen.api.http.SidechainWalletRestScheme._
import com.horizen.block.{SidechainBlock, SidechainBlockHeader}
import com.horizen.box.Box
import com.horizen.node._
import com.horizen.proposition.Proposition
import com.horizen.serialization.Views
import com.horizen.utils.BytesUtils
import com.horizen.SidechainTypes
import sparkz.core.settings.RESTApiSettings
import com.horizen.api.http.SidechainWalletErrorResponse.{ErrorFailedToParseSecret, ErrorPropositionNotFound, ErrorPropositionNotMatch, ErrorSecretAlreadyPresent}
import com.horizen.companion.SidechainSecretsCompanion
import com.horizen.secret.Secret
import com.horizen.chain.SidechainFeePaymentsInfo


import java.io.{File, PrintWriter}
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag
import scala.util.Try
import scala.concurrent.Await
import scala.util.{Failure, Success}
import java.util.{Scanner, Optional => JOptional}
import java.util.{ArrayList => JArrayList}

case class SidechainWalletApiRoute(override val settings: RESTApiSettings,
                                   sidechainNodeViewHolderRef: ActorRef,
                                   sidechainSecretsCompanion: SidechainSecretsCompanion)(implicit override val context: ActorRefFactory, override val ec: ExecutionContext)
  extends WalletBaseApiRoute[
    SidechainTypes#SCBT,
    SidechainBlockHeader,
    SidechainBlock,
    SidechainFeePaymentsInfo,
    NodeHistory,
    NodeState,
    NodeWallet,
    NodeMemoryPool,
    SidechainNodeView] (settings, sidechainNodeViewHolderRef) {


  //Please don't forget to add Auth for every new method of the wallet.
  override val route: Route = pathPrefix("wallet") {
    allBoxes ~ coinsBalance ~ balanceOfType ~ createPrivateKey25519 ~ createVrfSecret ~ allPublicKeys ~ importSecret ~ exportSecret ~ dumpSecrets ~ importSecrets
  }

  override implicit val tag: ClassTag[SidechainNodeView] = ClassTag[SidechainNodeView](classOf[SidechainNodeView])


  /**
    * Return all boxes, excluding those which ids are included in 'excludeBoxIds' list. Filter boxes of a given type
    */
  def allBoxes: Route = (post & path("allBoxes")) {
    withAuth {
      entity(as[ReqAllBoxes]) { body =>
        withNodeView { sidechainNodeView =>
          val optBoxTypeClass = body.boxTypeClass
          val wallet = sidechainNodeView.getNodeWallet
          val idsOfBoxesToExclude = body.excludeBoxIds.getOrElse(List()).map(idHex => BytesUtils.fromHexString(idHex))
          if (optBoxTypeClass.isEmpty) {
            val closedBoxesJson = wallet.allBoxes(idsOfBoxesToExclude.asJava).asScala.toList
            ApiResponseUtil.toResponse(RespAllBoxes(closedBoxesJson))
          } else {
            getClassByBoxClassName(optBoxTypeClass.get) match {
              case Failure(exception) => SidechainApiError(exception)
              case Success(clazz) =>
                val allClosedBoxesByType = wallet.boxesOfType(clazz, idsOfBoxesToExclude.asJava).asScala.toList
                ApiResponseUtil.toResponse(RespAllBoxes(allClosedBoxesByType))
            }
          }
        }
      }
    }
  }

  /**
    * Returns the balance of all types of coins boxes
    */
  def coinsBalance: Route = (post & path("coinsBalance")) {
    withAuth {
      withNodeView { sidechainNodeView =>
        val wallet = sidechainNodeView.getNodeWallet
        val sumOfBalances: Long = wallet.allCoinsBoxesBalance()
        ApiResponseUtil.toResponse(RespBalance(sumOfBalances))
      }
    }
  }

  /**
    * Returns the balance for given box type
    */
  def balanceOfType: Route = (post & path("balanceOfType")) {
    withAuth {
      entity(as[ReqBalance]) { body =>
        withNodeView { sidechainNodeView =>
          val wallet = sidechainNodeView.getNodeWallet
          getClassByBoxClassName(body.boxType) match {
            case Failure(exception) => SidechainApiError(exception)
            case Success(clazz) =>
              val balance = wallet.boxesBalance(clazz)
              ApiResponseUtil.toResponse(RespBalance(balance))
          }
        }
      }
    }
  }

  /**
   * Import a private key inside the wallet
   */
  def importSecret: Route = (post & path("importSecret")) {
    withAuth {
      entity(as[ReqImportSecret]) { body =>
        val secret = sidechainSecretsCompanion.parseBytes(BytesUtils.fromHexString(body.privKey))
        val future = sidechainNodeViewHolderRef ? LocallyGeneratedSecret(secret)
        Await.result(future, timeout.duration).asInstanceOf[Try[Unit]] match {
          case Success(_) =>
            ApiResponseUtil.toResponse(RespCreatePrivateKey(secret.publicImage()))
          case Failure(e) =>
            ApiResponseUtil.toResponse(ErrorSecretAlreadyPresent("Failed to add the key.", JOptional.of(e)))
        }
      }
    }
  }

  /**
   * Export a private key from the wallet based on its public key
   */
  def exportSecret: Route = (post & path("exportSecret")) {
    withAuth {
      entity(as[ReqExportSecret]) { body =>
        withNodeView { sidechainNodeView =>
          val wallet = sidechainNodeView.getNodeWallet
          val optionalPrivKey: JOptional[Secret] = wallet.secretByPublicKeyBytes(BytesUtils.fromHexString(body.publickey))
          if (optionalPrivKey.isEmpty) {
            ApiResponseUtil.toResponse(ErrorPropositionNotFound("Proposition not found in the wallet!", JOptional.empty()))
          } else {
            ApiResponseUtil.toResponse(RespExportSecret(BytesUtils.toHexString(sidechainSecretsCompanion.toBytes(optionalPrivKey.get()))))
          }
        }
      }
    }
  }

  /**
   * Perform a dump on a file of all the secrets inside the wallet.
   */
  def dumpSecrets: Route = (post & path("dumpSecrets")) {
    withAuth {
      entity(as[ReqDumpWallet]) { body =>
        val writer = new PrintWriter(new File(body.path))
        writer.write(s"# Secrets dump created on ${java.time.Instant.now()} \n")
        withNodeView { sidechainNodeView =>
          val wallet = sidechainNodeView.getNodeWallet
          wallet.allSecrets().forEach(key =>
            writer.write(BytesUtils.toHexString(sidechainSecretsCompanion.toBytes(key))+" "+BytesUtils.toHexString(key.publicImage().bytes())+"\n")
          )
          writer.close()
          ApiResponseUtil.toResponse(RespDumpSecrets(s"Secrets dump completed successfully at: ${body.path}"))
        }
      }
    }
  }

  /**
   * Import all secrets contained in a file.
   * The file format should be equal to the file format generated by the endpoint dumpSecrets. (SECRETS + " " + PUBLICKEY)
   */
  def importSecrets: Route = (post & path("importSecrets")) {
    withAuth {
      entity(as[ReqDumpWallet]) { body =>
        val reader = new Scanner(new File(body.path))

        //First collect every secrets and verify that their public image match with the corresponding public key in the file.
        var lineNumber = 1
        val secrets = new JArrayList[(SidechainTypes#SCS, Int)]()
        var error: JOptional[ErrorResponse] = JOptional.empty()
        while (reader.hasNextLine && error.isEmpty) {
          val line = reader.nextLine()
          if (!line.contains("#")) {
            val keyPair = line.split(" ")
            sidechainSecretsCompanion.parseBytesTry(BytesUtils.fromHexString(keyPair(0))) match {
              case Success(value) =>
                if(!BytesUtils.toHexString(value.publicImage().bytes()).equals(keyPair(1))) {
                  log.error(s"Import Wallet: Public key doesn't match: ${BytesUtils.toHexString(value.publicImage().bytes())}  ${keyPair(1)}")
                  error = JOptional.of(ErrorPropositionNotMatch(s"Public key doesn't match on line $lineNumber", JOptional.empty()))
                } else {
                  secrets.add((value, lineNumber))
                }
              case Failure(e) =>
                log.error(s"Import Wallet: Failed to parse the secret: ${keyPair(0)}")
                error = JOptional.of(ErrorFailedToParseSecret(s"Failed to parse the secret at line $lineNumber", JOptional.of(e)))
            }
          }
          lineNumber += 1
        }

        if(error.isPresent) {
          ApiResponseUtil.toResponse(error.get())
        } else {
          //Try to import the secrets
          var successfullyAdded = 0
          var failedToAdd = 0
          val errorDetail = new JArrayList[ImportSecretsDetail]()
          secrets.forEach(secret => {
            val future = sidechainNodeViewHolderRef ? LocallyGeneratedSecret(secret._1)
            Await.result(future, timeout.duration).asInstanceOf[Try[Unit]] match {
              case Success(_) =>
                log.info("Import Wallet: Successfully added the proposition: "+BytesUtils.toHexString(secret._1.publicImage().bytes()))
                successfullyAdded += 1
              case Failure(e) =>
                log.error("Import Wallet: Failed to add the proposition: "+BytesUtils.toHexString(secret._1.publicImage().bytes()))
                failedToAdd += 1
                errorDetail.add(ImportSecretsDetail(secret._2, e.getMessage))
            }
          })
          ApiResponseUtil.toResponse(RespImportSecrets(successfullyAdded, failedToAdd, errorDetail))
        }
      }
    }
  }

  def getClassByBoxClassName(className: String): Try[java.lang.Class[_ <: SidechainTypes#SCB]] = {
    Try(Class.forName(className).asSubclass(classOf[SidechainTypes#SCB])) orElse
      Try(Class.forName("com.horizen.box." + className).asSubclass(classOf[SidechainTypes#SCB]))
  }
}

object SidechainWalletRestScheme {

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class ReqAllBoxes(boxTypeClass: Option[String], excludeBoxIds: Option[Seq[String]])

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class RespAllBoxes(boxes: List[Box[Proposition]]) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class ReqBalance(boxType: String)

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class RespBalance(balance: Long) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class RespCreatePrivateKey(proposition: Proposition) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class ReqImportSecret(privKey: String) {
    require(privKey.nonEmpty, "Private key cannot be empty!")
  }

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class ReqExportSecret(publickey: String) {
    require(publickey.nonEmpty, "Publickey cannot be empty!")
  }

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class RespExportSecret(privKey: String)  extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class ReqDumpWallet(path: String) {
    require(path.nonEmpty, "Path cannot be empty!")
  }

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class RespDumpSecrets(status: String)  extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[api] case class RespImportSecrets(successfullyAdded: Int, failedToAdd: Int, summary: JArrayList[ImportSecretsDetail])  extends SuccessResponse
}

object SidechainWalletErrorResponse {

  case class ErrorSecretAlreadyPresent(description: String, exception: JOptional[Throwable]) extends ErrorResponse {
    override val code: String = "0302"
  }

  case class ErrorPropositionNotFound(description: String, exception: JOptional[Throwable]) extends ErrorResponse {
    override val code: String = "0303"
  }

  case class ErrorPropositionNotMatch(description: String, exception: JOptional[Throwable]) extends ErrorResponse {
    override val code: String = "0304"
  }

  case class ErrorFailedToParseSecret(description: String, exception: JOptional[Throwable]) extends ErrorResponse {
    override val code: String = "0305"
  }
}

@JsonView(Array(classOf[Views.Default]))
case class ImportSecretsDetail private (lineNumber: Int,
                                        description: String) {
  def getLineNumber: Int = {
    lineNumber
  }

  def getDescription: String = {
    description
  }
}
