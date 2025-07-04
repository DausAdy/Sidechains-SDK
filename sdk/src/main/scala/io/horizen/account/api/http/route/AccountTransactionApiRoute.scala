package io.horizen.account.api.http.route

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import com.fasterxml.jackson.annotation.JsonView
import com.fasterxml.jackson.databind.annotation.JsonDeserialize
import com.google.common.primitives.Bytes
import io.horizen.SidechainTypes
import io.horizen.account.api.http.route.AccountTransactionErrorResponse._
import io.horizen.account.api.http.route.AccountTransactionRestScheme._
import io.horizen.account.block.{AccountBlock, AccountBlockHeader}
import io.horizen.account.chain.AccountFeePaymentsInfo
import io.horizen.account.companion.SidechainAccountTransactionsCompanion
import io.horizen.account.fork.{Version1_3_0Fork, Version1_4_0Fork, Version1_5_0Fork}
import io.horizen.account.node.{AccountNodeView, NodeAccountHistory, NodeAccountMemoryPool, NodeAccountState}
import io.horizen.account.proof.SignatureSecp256k1
import io.horizen.account.proposition.AddressProposition
import io.horizen.account.secret.PrivateKeySecp256k1
import io.horizen.account.state.ForgerStakeV2MsgProcessor.{MAX_REWARD_SHARE, MIN_REGISTER_FORGER_STAKED_AMOUNT_IN_WEI, NUM_OF_EPOCHS_AFTER_FORK_ACTIVATION_FOR_UPDATE_FORGER}
import io.horizen.account.state.McAddrOwnershipMsgProcessor._
import io.horizen.account.state._
import io.horizen.account.state.nativescdata.forgerstakev2.RegisterOrUpdateForgerCmdInputDecoder.NULL_ADDRESS_WITH_PREFIX_HEX_STRING
import io.horizen.account.state.nativescdata.forgerstakev2.{RegisterOrUpdateForgerCmdInput, StakeDataDelegator, StakeDataForger}
import io.horizen.account.transaction.EthereumTransaction
import io.horizen.account.utils.WellKnownAddresses.{FORGER_STAKE_SMART_CONTRACT_ADDRESS, FORGER_STAKE_V2_SMART_CONTRACT_ADDRESS, MC_ADDR_OWNERSHIP_SMART_CONTRACT_ADDRESS, PROXY_SMART_CONTRACT_ADDRESS}
import io.horizen.account.utils.{EthereumTransactionUtils, ZenWeiConverter}
import io.horizen.api.http.JacksonSupport._
import io.horizen.api.http.route.TransactionBaseErrorResponse.{ErrorBadCircuit, ErrorByteTransactionParsing}
import io.horizen.api.http.route.{ErrorNotEnabledOnSeederNode, TransactionBaseApiRoute}
import io.horizen.api.http.{ApiResponseUtil, ErrorResponse, SuccessResponse}
import io.horizen.certificatesubmitter.keys.KeyRotationProofTypes.{MasterKeyRotationProofType, SigningKeyRotationProofType}
import io.horizen.certificatesubmitter.keys.{KeyRotationProof, KeyRotationProofTypes}
import io.horizen.cryptolibprovider.CircuitTypes.{CircuitTypes, NaiveThresholdSignatureCircuit, NaiveThresholdSignatureCircuitWithKeyRotation}
import io.horizen.cryptolibprovider.CryptoLibProvider
import io.horizen.evm.Address
import io.horizen.json.Views
import io.horizen.node.NodeWalletBase
import io.horizen.params.{NetworkParams, RegTestParams}
import io.horizen.proof.{SchnorrSignatureSerializer, Signature25519, VrfProof}
import io.horizen.proposition._
import io.horizen.secret.PrivateKey25519
import io.horizen.utils.BytesUtils
import org.web3j.crypto.Keys
import org.web3j.utils.Numeric.{cleanHexPrefix, hexStringToByteArray, prependHexPrefix}
import sparkz.core.settings.RESTApiSettings

import java.math.BigInteger
import java.util.{Optional => JOptional}
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.concurrent.ExecutionContext
import scala.jdk.OptionConverters.RichOptional
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

case class AccountTransactionApiRoute(override val settings: RESTApiSettings,
                                      sidechainNodeViewHolderRef: ActorRef,
                                      sidechainTransactionActorRef: ActorRef,
                                      companion: SidechainAccountTransactionsCompanion,
                                      params: NetworkParams,
                                      circuitType: CircuitTypes)
                                     (implicit override val context: ActorRefFactory, override val ec: ExecutionContext)
  extends TransactionBaseApiRoute[
    SidechainTypes#SCAT,
    AccountBlockHeader,
    AccountBlock,
    AccountFeePaymentsInfo,
    NodeAccountHistory,
    NodeAccountState,
    NodeWalletBase,
    NodeAccountMemoryPool,
    AccountNodeView](sidechainTransactionActorRef, companion) with SidechainTypes {

  override implicit val tag: ClassTag[AccountNodeView] = ClassTag[AccountNodeView](classOf[AccountNodeView])


  override val route: Route = pathPrefix(transactionPathPrefix) {
    allTransactions ~ createLegacyEIP155Transaction ~ createEIP1559Transaction ~ createLegacyTransaction ~ sendTransaction ~
      signTransaction ~ makeForgerStake ~ withdrawCoins ~ spendForgingStake ~ createSmartContract ~ allWithdrawalRequests ~
      allForgingStakes ~ myForgingStakes ~ decodeTransactionBytes ~ openForgerList ~ allowedForgerList ~ createKeyRotationTransaction ~
      invokeProxyCall ~ invokeProxyStaticCall  ~ sendKeysOwnership ~ getKeysOwnership ~ removeKeysOwnership ~
      getKeysOwnerScAddresses ~ sendMultisigKeysOwnership ~ pagedForgingStakes ~ pagedForgersStakesByForger ~
      pagedForgersStakesByDelegator ~ registerForger ~ updateForger
  }

  private def getFittingSecret(nodeView: AccountNodeView, fromAddress: Option[String], txValueInWei: BigInteger)
  : Option[PrivateKeySecp256k1] = {

    val wallet = nodeView.getNodeWallet
    val allAccounts = wallet.secretsOfType(classOf[PrivateKeySecp256k1])

    val secret = allAccounts.find(
      a => (fromAddress.isEmpty ||
        BytesUtils.toHexString(a.asInstanceOf[PrivateKeySecp256k1].publicImage.address.toBytes).equalsIgnoreCase(fromAddress.get)) &&
        nodeView.getNodeState.getBalance(a.asInstanceOf[PrivateKeySecp256k1].publicImage.address)
          .compareTo(txValueInWei) >= 0
    )

    if (secret.nonEmpty) Option.apply(secret.get.asInstanceOf[PrivateKeySecp256k1])
    else Option.empty[PrivateKeySecp256k1]
  }

  private def signTransactionWithSecret(secret: PrivateKeySecp256k1, tx: EthereumTransaction): EthereumTransaction = {
    val messageToSign = tx.messageToSign()
    val msgSignature = secret.sign(messageToSign)
    new EthereumTransaction(
        tx,
        new SignatureSecp256k1(msgSignature.getV, msgSignature.getR, msgSignature.getS)
    )
  }

  private def signMessageWithSecrets(
                                      nodeView: AccountNodeView,
                                      blockSignPubKey: PublicKey25519Proposition, vrfPublicKey: VrfPublicKey,
                                      messageToSign: Array[Byte]): (Signature25519, VrfProof) = {
    val wallet = nodeView.getNodeWallet

    val signature25519 = wallet.secretByPublicKey25519Proposition(blockSignPubKey).toScala match {
      case None => throw new IllegalArgumentException("No matching secret for input blockSignPubKey")
      case Some(secret) => secret.sign(messageToSign)
    }

    val signatureVrf = wallet.secretByVrfPublicKey(vrfPublicKey).toScala match {
      case None => throw new IllegalArgumentException("No matching secret for input vrfPublicKey")
      case Some(secret) => secret.sign(messageToSign)
    }

    log.debug(s"25519: key=$blockSignPubKey, signature=$signature25519")
    log.debug(s"vrf: key=$vrfPublicKey, signature=$signatureVrf")
    (signature25519, signatureVrf)

  }

  /**
   * Create an unsigned legacy eth transaction, and then:
   *  - if the optional input parameter 'outputRawBytes'=True is set in ReqLegacyTransaction just
   *    return the raw hex bytes representation
   *  - otherwise sign it and send the resulting tx to the network and return the transaction id
   */
  def createLegacyTransaction: Route = (post & path("createLegacyTransaction")) {
    withBasicAuth {
      _ => {
        entity(as[ReqLegacyTransaction]) { body =>
          val txCost = body.value.getOrElse(BigInteger.ZERO)
            .add(body.gasLimit.multiply(body.gasPrice))

          applyOnNodeView { sidechainNodeView =>
            val secret = getFittingSecret(sidechainNodeView, body.from, txCost)
            secret match {
              case Some(secret) =>
                // compute the nonce if not specified in the params
                val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(secret.publicImage.address))

                val unsignedTx = new EthereumTransaction(
                  EthereumTransactionUtils.getToAddressFromString(body.to.orNull),
                  nonce,
                  body.gasPrice,
                  body.gasLimit,
                  body.value.getOrElse(BigInteger.ZERO),
                  EthereumTransactionUtils.getDataFromString(body.data),
                  if (body.signature_v.isDefined)
                    new SignatureSecp256k1(
                      new BigInteger(body.signature_v.get, 16),
                      new BigInteger(body.signature_r.get, 16),
                      new BigInteger(body.signature_s.get, 16))
                  else
                    null
                )
                val resp = if (body.outputRawBytes.getOrElse(false)) {
                  ApiResponseUtil.toResponse(rawTransactionResponseRepresentation(unsignedTx))
                } else {
                  val signedTx = signTransactionWithSecret(secret, unsignedTx)
                  validateAndSendTransaction(signedTx)
                }
                resp

              case None =>
                ApiResponseUtil.toResponse(ErrorInsufficientBalance("ErrorInsufficientBalance", JOptional.empty()))
            }
          }
        }
      }
    }
  }

  /**
   * Create an unsigned EIP155 (Simple replay attack protection) legacy eth transaction, and then:
   *  - if the optional input parameter 'outputRawBytes'=True is set in ReqLegacyTransaction just
   *    return the raw hex bytes representation
   *  - otherwise sign it and send the resulting tx to the network and return the transaction id
   */
  def createLegacyEIP155Transaction: Route = (post & path("createLegacyEIP155Transaction")) {
    withBasicAuth {
      _ => {
        entity(as[ReqLegacyTransaction]) { body =>
          val txCost = body.value.getOrElse(BigInteger.ZERO)
            .add(body.gasLimit.multiply(body.gasPrice))

          applyOnNodeView { sidechainNodeView =>

            val secret = getFittingSecret(sidechainNodeView, body.from, txCost)
            secret match {
              case Some(secret) =>
                // compute the nonce if not specified in the params
                val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(secret.publicImage.address))

              val unsignedTx = new EthereumTransaction(
                params.chainId,
                EthereumTransactionUtils.getToAddressFromString(body.to.orNull),
                nonce,
                body.gasPrice,
                body.gasLimit,
                body.value.getOrElse(BigInteger.ZERO),
                EthereumTransactionUtils.getDataFromString(body.data),
                if (body.signature_v.isDefined)
                  new SignatureSecp256k1(
                    new BigInteger(body.signature_v.get, 16),
                    new BigInteger(body.signature_r.get, 16),
                    new BigInteger(body.signature_s.get, 16))
                else
                  null
              )
              val resp = if (body.outputRawBytes.getOrElse(false)) {
                ApiResponseUtil.toResponse(rawTransactionResponseRepresentation(unsignedTx))
              } else {
                val signedTx = signTransactionWithSecret(secret, unsignedTx)
                validateAndSendTransaction(signedTx)
              }
              resp

            case None =>
              ApiResponseUtil.toResponse(ErrorInsufficientBalance("ErrorInsufficientBalance", JOptional.empty()))
          }
        }
      }
    }
  }
  }

  /**
   * Create an unsigned EIP1559 eth transaction, and then:
   *  - if the optional input parameter 'outputRawBytes'=True is set in ReqLegacyTransaction just
   *    return the raw hex bytes representation
   *  - otherwise sign it and send the resulting tx to the network and return the transaction id
   */
  def createEIP1559Transaction: Route = (post & path("createEIP1559Transaction")) {
    withBasicAuth {
      _ => {
        entity(as[ReqEIP1559Transaction]) { body =>

          val txCost = body.value.getOrElse(BigInteger.ZERO)
            .add(body.gasLimit.multiply(body.maxFeePerGas))

          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val secret = getFittingSecret(sidechainNodeView, body.from, txCost)
            secret match {
              case Some(secret) =>
                val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(secret.publicImage.address))

                val unsignedTx: EthereumTransaction = new EthereumTransaction(
                  params.chainId,
                  EthereumTransactionUtils.getToAddressFromString(body.to.orNull),
                  nonce,
                  body.gasLimit,
                  body.maxPriorityFeePerGas,
                  body.maxFeePerGas,
                  body.value.getOrElse(BigInteger.ZERO),
                  EthereumTransactionUtils.getDataFromString(body.data),
                  if (body.signature_v.isDefined)
                    new SignatureSecp256k1(
                      new BigInteger(body.signature_v.get, 16),
                      new BigInteger(body.signature_r.get, 16),
                      new BigInteger(body.signature_s.get, 16))
                  else
                    null)
                val resp = if (body.outputRawBytes.getOrElse(false)) {
                  ApiResponseUtil.toResponse(rawTransactionResponseRepresentation(unsignedTx))
                } else {
                  val signedTx = signTransactionWithSecret(secret, unsignedTx)
                  validateAndSendTransaction(signedTx)
                }
                resp

              case None =>
                ApiResponseUtil.toResponse(ErrorInsufficientBalance("ErrorInsufficientBalance", JOptional.empty()))
            }
          }
        }
      }
    }
  }

   /**
   * Decode the input raw eth transaction bytes into an obj, sign it using the input 'from' address
   * and return the resulting signed raw eth transaction bytes
   */
  def signTransaction: Route = (post & path("signTransaction")) {
    withBasicAuth {
      _ => {
        entity(as[ReqSignTransaction]) {
          body => {
            applyOnNodeView { sidechainNodeView =>
              companion.parseBytesTry(BytesUtils.fromHexString(body.transactionBytes)) match {
                case Success(unsignedTx) =>
                  val txCost = unsignedTx.maxCost
                  val secret = getFittingSecret(sidechainNodeView, body.from, txCost)
                  secret match {
                    case Some(secret) =>
                      val signedTx = signTransactionWithSecret(secret, unsignedTx.asInstanceOf[EthereumTransaction])
                      ApiResponseUtil.toResponse(rawTransactionResponseRepresentation(signedTx))
                    case None =>
                      ApiResponseUtil.toResponse(ErrorInsufficientBalance("ErrorInsufficientBalance", JOptional.empty()))
                  }
                case Failure(exception) =>
                  ApiResponseUtil.toResponse(ErrorByteTransactionParsing("ErrorByteTransactionParsing", JOptional.of(exception)))
              }
            }
          }
        }
      }
    }
  }

  // creates an eth transaction which makes a call to the forger stake native smart contract
  // expressing the vote for opening the restrict forgers list.
  // Analogous to the UTXO model createOpenStakeTransaction
  def openForgerList: Route = (post & path("openForgerList")) {
    withBasicAuth {
      _ => {
        entity(as[ReqOpenStakeForgerList]) { body =>

          // first of all reject the command if we do not have closed forger list
          if (!params.restrictForgers) {
            ApiResponseUtil.toResponse(ErrorOpenForgersList(
              s"The list of forger is not restricted (see configuration)",
              JOptional.empty()))

          } else {

          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val valueInWei = BigInteger.ZERO
            // default gas related params
            val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
            var maxPriorityFeePerGas = BigInteger.valueOf(120)
            var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
            var gasLimit = BigInteger.valueOf(500000)

            if (body.gasInfo.isDefined) {
              maxFeePerGas = body.gasInfo.get.maxFeePerGas
              maxPriorityFeePerGas = body.gasInfo.get.maxPriorityFeePerGas
              gasLimit = body.gasInfo.get.gasLimit
            }

            val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))

            getBlockSignSecret(body.forgerIndex, sidechainNodeView.getNodeWallet) match {

              case Failure(e) =>
                ApiResponseUtil.toResponse(ErrorOpenForgersList(
                  s"Could not get proposition for forgerIndex=${body.forgerIndex}",
                  JOptional.of(e)))

              case Success(blockSignSecret) =>


                val secret = getFittingSecret(sidechainNodeView, None, txCost)

                secret match {
                  case Some(txCreatorSecret) =>
                    val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(txCreatorSecret.publicImage.address))

                    val msgToSign = ForgerStakeMsgProcessor.getOpenStakeForgerListCmdMessageToSign(
                      body.forgerIndex, txCreatorSecret.publicImage().address(), nonce.toByteArray)

                    val signature = blockSignSecret.sign(msgToSign)

                    val dataBytes = encodeOpenStakeCmdRequest(body.forgerIndex, signature)
                    val tmpTx: EthereumTransaction = new EthereumTransaction(
                      params.chainId,
                      JOptional.of(new AddressProposition(FORGER_STAKE_SMART_CONTRACT_ADDRESS)),
                      nonce,
                      gasLimit,
                      maxPriorityFeePerGas,
                      maxFeePerGas,
                      valueInWei,
                      dataBytes,
                      null
                    )

                    validateAndSendTransaction(signTransactionWithSecret(txCreatorSecret, tmpTx))

                  case None =>
                    ApiResponseUtil.toResponse(ErrorInsufficientBalance("No account with enough balance found", JOptional.empty()))

                }

              }
            }
          }
        }
      }
    }
  }

  private def getBlockSignSecret(forgerIndex: Int, wallet: NodeWalletBase): Try[PrivateKey25519] = Try {
    val blockSignProposition = Try[PublicKey25519Proposition] {
      params.allowedForgersList(forgerIndex)._1
    }
    blockSignProposition match {
      case scala.util.Failure(e) =>
        throw new IllegalArgumentException(s"Could not get proposition for forgerIndex=$forgerIndex; error: " + e.getMessage)
      case Success(prop) =>
        val secret = wallet.secretByPublicKey(prop)
        if (secret.isEmpty) {
          throw new IllegalArgumentException(s"Could not get secret in wallet for proposition for prop=$prop")
        } else {
          secret.get().asInstanceOf[PrivateKey25519]
        }
    }
  }

  def makeForgerStake: Route = (post & path("makeForgerStake")) {
    withBasicAuth {
      _ => {
        entity(as[ReqCreateForgerStake]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val accountState = sidechainNodeView.getNodeState
            val epochNumber = accountState.getConsensusEpochNumber.getOrElse(0)
            if (!accountState.isForgerStakeV1SmartContractDisabled(Version1_4_0Fork.get(epochNumber).active)) {
              if (!sidechainNodeView.getNodeState.isForgerStakeAvailable(Version1_3_0Fork.get(epochNumber).active)) {
                ApiResponseUtil.toResponse(GenericTransactionError("Unable to add", JOptional.empty()))
              } else {
                val valueInWei = ZenWeiConverter.convertZenniesToWei(body.forgerStakeInfo.value)

                // default gas related params
                val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
                var maxPriorityFeePerGas = BigInteger.valueOf(120)
                var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
                var gasLimit = BigInteger.valueOf(500000)

                if (body.gasInfo.isDefined) {
                  maxFeePerGas = body.gasInfo.get.maxFeePerGas
                  maxPriorityFeePerGas = body.gasInfo.get.maxPriorityFeePerGas
                  gasLimit = body.gasInfo.get.gasLimit
                }

                val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))

                val secret = getFittingSecret(sidechainNodeView, None, txCost)

                secret match {
                  case Some(secret) =>

                    val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(secret.publicImage.address))
                    val dataBytes = encodeAddNewStakeCmdRequest(body.forgerStakeInfo)
                    val tmpTx: EthereumTransaction = new EthereumTransaction(
                      params.chainId,
                      JOptional.of(new AddressProposition(FORGER_STAKE_SMART_CONTRACT_ADDRESS)),
                      nonce,
                      gasLimit,
                      maxPriorityFeePerGas,
                      maxFeePerGas,
                      valueInWei,
                      dataBytes,
                      null
                    )
                    validateAndSendTransaction(signTransactionWithSecret(secret, tmpTx))
                  case None =>
                    ApiResponseUtil.toResponse(ErrorInsufficientBalance("No account with enough balance found", JOptional.empty()))
                }
              }
            }
            else
              ApiResponseUtil.toResponse(ErrorDisabledMethod())
          }
        }
      }
    }
  }

  def spendForgingStake: Route = (post & path("spendForgingStake")) {
    withBasicAuth {
      _ => {
        entity(as[ReqSpendForgingStake]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val epochNumber = sidechainNodeView.getNodeState.getConsensusEpochNumber.getOrElse(0)
            if (!sidechainNodeView.getNodeState.isForgerStakeV1SmartContractDisabled(Version1_4_0Fork.get(epochNumber).active)) {
              val valueInWei = BigInteger.ZERO
              // default gas related params
              val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
              var maxPriorityFeePerGas = BigInteger.valueOf(120)
              var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
              var gasLimit = BigInteger.valueOf(500000)

              if (body.gasInfo.isDefined) {
                maxFeePerGas = body.gasInfo.get.maxFeePerGas
                maxPriorityFeePerGas = body.gasInfo.get.maxPriorityFeePerGas
                gasLimit = body.gasInfo.get.gasLimit
              }
              //getFittingSecret needs to take into account only gas
              val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))
              val secret = getFittingSecret(sidechainNodeView, None, txCost)
              secret match {
                case Some(txCreatorSecret) =>
                  val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(txCreatorSecret.publicImage.address))
                  val stakeDataOpt = sidechainNodeView.getNodeState.getForgerStakeData(body.stakeId, Version1_3_0Fork.get(epochNumber).active)
                  stakeDataOpt match {
                    case Some(stakeData) =>
                      val stakeOwnerSecretOpt = sidechainNodeView.getNodeWallet.secretByPublicKey(stakeData.ownerPublicKey)
                      if (stakeOwnerSecretOpt.isEmpty) {
                        ApiResponseUtil.toResponse(ErrorForgerStakeOwnerNotFound(s"Forger Stake Owner not found"))
                      }
                      else {
                        val stakeOwnerSecret = stakeOwnerSecretOpt.get().asInstanceOf[PrivateKeySecp256k1]

                        val msgToSign = ForgerStakeMsgProcessor.getRemoveStakeCmdMessageToSign(BytesUtils.fromHexString(body.stakeId), txCreatorSecret.publicImage().address(), nonce.toByteArray)
                        val signature = stakeOwnerSecret.sign(msgToSign)
                        val dataBytes = encodeSpendStakeCmdRequest(signature, body.stakeId)
                        val tmpTx: EthereumTransaction = new EthereumTransaction(
                          params.chainId,
                          JOptional.of(new AddressProposition(FORGER_STAKE_SMART_CONTRACT_ADDRESS)),
                          nonce,
                          gasLimit,
                          maxPriorityFeePerGas,
                          maxFeePerGas,
                          valueInWei,
                          dataBytes,
                          null
                        )

                        validateAndSendTransaction(signTransactionWithSecret(txCreatorSecret, tmpTx))
                      }
                    case None => ApiResponseUtil.toResponse(ErrorForgerStakeNotFound(s"No Forger Stake found with stake id ${body.stakeId}"))
                  }
                case None =>
                  ApiResponseUtil.toResponse(ErrorInsufficientBalance("No account with enough balance found", JOptional.empty()))
              }
            }
            else
              ApiResponseUtil.toResponse(ErrorDisabledMethod())
          }
        }
      }
    }
  }

  def pagedForgingStakes: Route = (post & path("pagedForgingStakes")) {
    withBasicAuth {
      _ => {
        entity(as[ReqPagedForgingStakes]) { body =>
          withNodeView { sidechainNodeView =>
            val accountState = sidechainNodeView.getNodeState
            val epochNumber = accountState.getConsensusEpochNumber.getOrElse(0)
            if (!sidechainNodeView.getNodeState.isForgerStakeV1SmartContractDisabled(Version1_4_0Fork.get(epochNumber).active)) {
              if (Version1_3_0Fork.get(epochNumber).active) {
                Try {
                  accountState.getPagedListOfForgersStakes(body.startPos, body.size)
                } match {
                  case Success((nextPos, listOfForgerStakes)) =>
                    ApiResponseUtil.toResponse(RespPagedForgerStakes(nextPos, listOfForgerStakes.toList))
                  case Failure(exception) =>
                    ApiResponseUtil.toResponse(GenericTransactionError(s"Invalid input parameters", JOptional.of(exception)))
                }
              } else {
                ApiResponseUtil.toResponse(GenericTransactionError(s"Fork 1.3 is not active, can not invoke this command",
                  JOptional.empty()))
              }
            }
            else
              ApiResponseUtil.toResponse(ErrorDisabledMethod())
          }
        }
      }
    }
  }

  private def addressIsNull(address: String) : Boolean = {
    prependHexPrefix(address) == NULL_ADDRESS_WITH_PREFIX_HEX_STRING
  }

  private def addressStringIsValid(address: String) : Try[Unit] =  Try {
    if (cleanHexPrefix(address).length == 2 * Address.LENGTH) {
        val _ = BytesUtils.fromHexString(cleanHexPrefix(address))
    } else {
      throw new IllegalArgumentException(s"Invalid address string length: ${cleanHexPrefix(address).length}, expected ${2 * Address.LENGTH}")
    }
  }

  def registerForger: Route = (post & path("registerForger")) {
    withBasicAuth {
      _ => {
        entity(as[ReqRegisterForger]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val rewardShare = body.rewardShare.getOrElse(0)

            if (rewardShare < 0 || rewardShare > MAX_REWARD_SHARE) {
              val msg = s"Reward share must be in the range [0, $MAX_REWARD_SHARE]"
              ApiResponseUtil.toResponse(ErrorRegisterForgerInvalidRewardParams(msg))
            }
            else {
              val rewardAddress = body.rewardAddress.getOrElse(NULL_ADDRESS_WITH_PREFIX_HEX_STRING)

              addressStringIsValid(rewardAddress) match {
                case Success(_) =>
                  if (!addressIsNull(rewardAddress) && rewardShare == 0) {
                    val msg = s"Reward share cannot be 0 if reward address is defined - Reward share = ${body.rewardShare}, reward address = $rewardAddress"
                    ApiResponseUtil.toResponse(ErrorRegisterForgerInvalidRewardParams(msg))
                  }
                  else if (addressIsNull(rewardAddress) && rewardShare != 0) {
                    val msg = s"Reward share cannot be different from 0 if reward address is null - Reward share = ${body.rewardShare}, reward address = $rewardAddress"
                    ApiResponseUtil.toResponse(ErrorRegisterForgerInvalidRewardParams(msg))
                  }
                  else {
                    val valueInWei = ZenWeiConverter.convertZenniesToWei(body.stakedAmount)
                    if (valueInWei.compareTo(MIN_REGISTER_FORGER_STAKED_AMOUNT_IN_WEI) < 0) {
                      val msg = s"Value ${valueInWei.toString()} is below the minimum stake amount threshold: $MIN_REGISTER_FORGER_STAKED_AMOUNT_IN_WEI "
                      ApiResponseUtil.toResponse(ErrorRegisterForgerInvalidRewardParams(msg))
                    }
                    else
                      doRegisterOrUpdateForger(ForgerStakeV2MsgProcessor.RegisterForgerCmd, sidechainNodeView, valueInWei, body, rewardShare, rewardAddress)
                  }
                case Failure(ex) =>
                  ApiResponseUtil.toResponse(ErrorRegisterForgerInvalidRewardParams(s"Invalid address: ${ex.getMessage}"))
              }
            }
          }
        }
      }
    }
  }

  def updateForger: Route = (post & path("updateForger")) {
    withBasicAuth {
      _ => {
        entity(as[ReqUpdateForger]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            if (body.rewardShare <= 0 || body.rewardShare > MAX_REWARD_SHARE) {
              val msg = s"Reward share must be in the range (0, $MAX_REWARD_SHARE]"
              ApiResponseUtil.toResponse(ErrorRegisterForgerInvalidRewardParams(msg))
            } else {
              addressStringIsValid(body.rewardAddress) match {
                case Success(_) =>
                  if (addressIsNull(body.rewardAddress)) {
                    val msg = s"Reward address can not be the null address"
                    ApiResponseUtil.toResponse(ErrorRegisterForgerInvalidRewardParams(msg))
                  } else {
                    val valueInWei = BigInteger.ZERO
                    doRegisterOrUpdateForger(ForgerStakeV2MsgProcessor.UpdateForgerCmd, sidechainNodeView, valueInWei, body, body.rewardShare, body.rewardAddress)
                  }
                case Failure(ex) =>
                  ApiResponseUtil.toResponse(ErrorRegisterForgerInvalidRewardParams(s"Invalid address: ${ex.getMessage}"))
              }
            }
          }
        }
      }
    }
  }

  private def doRegisterOrUpdateForger(operation: String, sidechainNodeView: AccountNodeView, valueInWei: BigInteger, body: ReqBaseForger, rewardShare: Int, rewardAddress: String) = {

    val accountState = sidechainNodeView.getNodeState
    val epochNumber = accountState.getConsensusEpochNumber.getOrElse(0)
    if (!Version1_4_0Fork.get(epochNumber).active) {
      ApiResponseUtil.toResponse(GenericTransactionError(s"Fork 1.4 is not active, can not invoke this command",
        JOptional.empty()))
    }
    else if (!accountState.forgerStakesV2IsActive) {
      ApiResponseUtil.toResponse(GenericTransactionError(s"Forger Stake Storage V2 is not active, can not invoke this command",
        JOptional.empty()))
    }
    else if (operation == ForgerStakeV2MsgProcessor.UpdateForgerCmd &&
      epochNumber < (Version1_4_0Fork.getActivationEpoch() + NUM_OF_EPOCHS_AFTER_FORK_ACTIVATION_FOR_UPDATE_FORGER) ) {
      ApiResponseUtil.toResponse(GenericTransactionError(s"Fork 1.4 has been activated at epoch ${Version1_4_0Fork.getActivationEpoch()}, but $NUM_OF_EPOCHS_AFTER_FORK_ACTIVATION_FOR_UPDATE_FORGER epochs must go by before invoking this command (current epoch: $epochNumber)",
        JOptional.empty()))
    } else {
        // default gas related params
        val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
        var maxPriorityFeePerGas = BigInteger.valueOf(120)
        var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
        var gasLimit = BigInteger.valueOf(500000)

        if (body.gasInfo.isDefined) {
          maxFeePerGas = body.gasInfo.get.maxFeePerGas
          maxPriorityFeePerGas = body.gasInfo.get.maxPriorityFeePerGas
          gasLimit = body.gasInfo.get.gasLimit
        }

        val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))

        val secret = getFittingSecret(sidechainNodeView, None, txCost)

        secret match {
          case Some(secret) =>
            val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(secret.publicImage.address))

            val blockSignPubKey = PublicKey25519PropositionSerializer.getSerializer.parseBytesAndCheck(BytesUtils.fromHexString(body.blockSignPubKey))
            val vrfPubKey = VrfPublicKeySerializer.getSerializer.parseBytesAndCheck(BytesUtils.fromHexString(body.vrfPubKey))

            Try {
              val msg = ForgerStakeV2MsgProcessor.getHashedMessageToSign(body.blockSignPubKey, body.vrfPubKey, rewardShare, rewardAddress)
              val signatures = signMessageWithSecrets(sidechainNodeView, blockSignPubKey, vrfPubKey, msg)

              encodeRegisterOrUpdateForgerCmdRequest(operation, blockSignPubKey, vrfPubKey, rewardShare, new AddressProposition(hexStringToByteArray(rewardAddress)), signatures._1, signatures._2)
            } match {
              case Success(dataBytes) =>

                val tmpTx: EthereumTransaction = new EthereumTransaction(
                  params.chainId,
                  JOptional.of(new AddressProposition(FORGER_STAKE_V2_SMART_CONTRACT_ADDRESS)),
                  nonce,
                  gasLimit,
                  maxPriorityFeePerGas,
                  maxFeePerGas,
                  valueInWei,
                  dataBytes,
                  null
                )
                validateAndSendTransaction(signTransactionWithSecret(secret, tmpTx))

              case Failure(exception) =>
                ApiResponseUtil.toResponse(GenericTransactionError(s"Command $operation failed: ", JOptional.of(exception)))

            }

          case None =>
            ApiResponseUtil.toResponse(ErrorInsufficientBalance("No account with enough balance found", JOptional.empty()))

      }
    }
  }


  def pagedForgersStakesByForger: Route = (post & path("pagedForgersStakesByForger")) {
    withBasicAuth {
      _ => {
        entity(as[ReqPagedForgerStakesByForger]) { body =>
          withNodeView { sidechainNodeView =>
            val accountState = sidechainNodeView.getNodeState

            Try {
              val blockSignPubKey = PublicKey25519PropositionSerializer.getSerializer.parseBytesAndCheck(BytesUtils.fromHexString(body.blockSignPubKey))
              val vrfPubKey = VrfPublicKeySerializer.getSerializer.parseBytesAndCheck(BytesUtils.fromHexString(body.vrfPubKey))
              accountState.getPagedForgersStakesByForger(
                ForgerPublicKeys(blockSignPubKey, vrfPubKey), body.startPos, body.size)
            } match {
              case Success(result) =>
                ApiResponseUtil.toResponse(RespPagedForgerStakesByForger(result.nextStartPos, result.stakesData.toList))
              case Failure(exception) =>
                ApiResponseUtil.toResponse(GenericTransactionError(s"Command failed: ", JOptional.of(exception)))
            }
          }
        }
      }
    }
  }

  def pagedForgersStakesByDelegator: Route = (post & path("pagedForgersStakesByDelegator")) {
    withBasicAuth {
      _ => {
        entity(as[ReqPagedForgerStakesByDelegator]) { body =>
          withNodeView { sidechainNodeView =>
            val accountState = sidechainNodeView.getNodeState
            val epochNumber = accountState.getConsensusEpochNumber.getOrElse(0)
            if (Version1_4_0Fork.get(epochNumber).active) {
              Try {
                accountState.getPagedForgersStakesByDelegator(new Address(body.delegatorAddress), body.startPos, body.size)
              } match {
                case Success(result) =>
                  ApiResponseUtil.toResponse(RespPagedForgerStakesByDelegator(result.nextStartPos, result.stakesData.toList))
                case Failure(exception) =>
                  ApiResponseUtil.toResponse(GenericTransactionError(s"Invalid input parameters", JOptional.of(exception)))
              }
            } else {
              ApiResponseUtil.toResponse(GenericTransactionError(s"Fork 1.4 is not active, can not invoke this command",
                JOptional.empty()))
            }
          }
        }
      }
    }
  }

  def allForgingStakes: Route = (post & path("allForgingStakes")) {
    withNodeView { sidechainNodeView =>
      val accountState = sidechainNodeView.getNodeState
      val epochNumber = accountState.getConsensusEpochNumber.getOrElse(0)
      val listOfForgerStakes = accountState.getListOfForgersStakes(Version1_3_0Fork.get(epochNumber).active, Version1_4_0Fork.get(epochNumber).active)
      ApiResponseUtil.toResponse(RespForgerStakes(listOfForgerStakes.toList))
    }
  }

  def allowedForgerList: Route = (post & path("allowedForgerList")) {
    if (params.restrictForgers) {
      // get the restrict list of forgers from configuration
      val allowedForgerKeysList : Seq[(PublicKey25519Proposition, VrfPublicKey)] = params.allowedForgersList

      withNodeView { sidechainNodeView =>
        val accountState = sidechainNodeView.getNodeState
        // get the list of indexes of allowed forgers (0 / 1 depending on the vote expressed so far)
        val forgersIndexList : Seq[Int] = accountState.getAllowedForgerList
        // join the two lists into one
        val resultList : Seq[(Int, (PublicKey25519Proposition, VrfPublicKey))] = forgersIndexList.zip(allowedForgerKeysList)
        // create the result
        val allowedForgerInfoSeq = resultList.map {
          entry => RespForgerInfo(entry._2._1, entry._2._2, entry._1)
        }
        ApiResponseUtil.toResponse(RespAllowedForgerList(allowedForgerInfoSeq.toList))
      }
    } else {
      ApiResponseUtil.toResponse(RespAllowedForgerList(Seq().toList))
    }

  }

  def myForgingStakes: Route = (post & path("myForgingStakes")) {
    withBasicAuth {
      _ => {
        withNodeView { sidechainNodeView =>
          val accountState = sidechainNodeView.getNodeState
          val epochNumber = accountState.getConsensusEpochNumber.getOrElse(0)
          val listOfForgerStakes = accountState.getListOfForgersStakes(Version1_3_0Fork.get(epochNumber).active, Version1_4_0Fork.get(epochNumber).active)
          if (listOfForgerStakes.nonEmpty) {
            val wallet = sidechainNodeView.getNodeWallet
            val walletPubKeys = wallet.allSecrets().map(_.publicImage).toSeq
            val ownedStakes = listOfForgerStakes.view.filter(stake => {
              walletPubKeys.contains(stake.forgerStakeData.ownerPublicKey)
            })
            ApiResponseUtil.toResponse(RespForgerStakes(ownedStakes.toList))
          } else {
            ApiResponseUtil.toResponse(RespForgerStakes(Seq().toList))
          }
        }
      }
    }
  }

  def withdrawCoins: Route = (post & path("withdrawCoins")) {
    withBasicAuth {
      _ => {
        entity(as[ReqWithdrawCoins]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>

            val accountState = sidechainNodeView.getNodeState
            val epochNumber = accountState.getConsensusEpochNumber.getOrElse(0)

            if (Version1_5_0Fork.get(epochNumber).active) {
              ApiResponseUtil.toResponse(GenericTransactionError(s"Fork 1.5 is active, can not invoke this command",
                JOptional.empty()))
            } else {
              val valueInWei = ZenWeiConverter.convertZenniesToWei(body.withdrawalRequest.value)
              val gasInfo = body.gasInfo

              // default gas related params
              val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
              var maxPriorityFeePerGas = BigInteger.valueOf(120)
              var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
              var gasLimit = BigInteger.valueOf(500000)

              if (gasInfo.isDefined) {
                maxFeePerGas = gasInfo.get.maxFeePerGas
                maxPriorityFeePerGas = gasInfo.get.maxPriorityFeePerGas
                gasLimit = gasInfo.get.gasLimit
              }

              val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))
              val secret = getFittingSecret(sidechainNodeView, None, txCost)
              secret match {
                case Some(secret) =>
                  val dataBytes = encodeAddNewWithdrawalRequestCmd(body.withdrawalRequest)
                  dataBytes match {
                    case Success(data) =>
                      val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(secret.publicImage.address))
                      val tmpTx: EthereumTransaction = new EthereumTransaction(
                        params.chainId,
                        JOptional.of(new AddressProposition(WithdrawalMsgProcessor.contractAddress)),
                        nonce,
                        gasLimit,
                        maxPriorityFeePerGas,
                        maxFeePerGas,
                        valueInWei,
                        data,
                        null
                      )
                      validateAndSendTransaction(signTransactionWithSecret(secret, tmpTx))
                    case Failure(exc) =>
                      ApiResponseUtil.toResponse(ErrorInvalidMcAddress(s"Invalid Mc address ${body.withdrawalRequest.mainchainAddress}", JOptional.of(exc)))
                  }
                case None =>
                  ApiResponseUtil.toResponse(ErrorInsufficientBalance("No account with enough balance found", JOptional.empty()))
              }
            }


          }
        }
      }
    }
  }

  def allWithdrawalRequests: Route = (post & path("allWithdrawalRequests")) {
    entity(as[ReqAllWithdrawalRequests]) { body =>
      withNodeView { sidechainNodeView =>
        val accountState = sidechainNodeView.getNodeState
        val listOfWithdrawalRequests = accountState.getWithdrawalRequests(body.epochNum)
        ApiResponseUtil.toResponse(RespAllWithdrawalRequests(listOfWithdrawalRequests.toList))
      }
    }
  }

  def createSmartContract: Route = (post & path("createSmartContract")) {
    withBasicAuth {
      _ => {
        entity(as[ReqCreateContract]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val valueInWei = BigInteger.ZERO

            val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
            var maxPriorityFeePerGas = GasUtil.TxGasContractCreation
            var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
            var gasLimit = BigInteger.valueOf(500000)

            if (body.gasInfo.isDefined) {
              maxFeePerGas = body.gasInfo.get.maxFeePerGas
              maxPriorityFeePerGas = body.gasInfo.get.maxPriorityFeePerGas
              gasLimit = body.gasInfo.get.gasLimit
            }

            val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))
            val secret = getFittingSecret(sidechainNodeView, None, txCost)
            secret match {
              case Some(secret) =>
                val to: String = null
                val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(secret.publicImage.address))
                val data = body.contractCode
                val tmpTx: EthereumTransaction = new EthereumTransaction(
                  params.chainId,
                  EthereumTransactionUtils.getToAddressFromString(to),
                  nonce,
                  gasLimit,
                  maxPriorityFeePerGas,
                  maxFeePerGas,
                  valueInWei,
                  EthereumTransactionUtils.getDataFromString(data),
                  null
                )
                validateAndSendTransaction(signTransactionWithSecret(secret, tmpTx))
              case None =>
                ApiResponseUtil.toResponse(ErrorInsufficientBalance("No account with enough balance found", JOptional.empty()))
            }
          }
        }
      }
    }
  }

  def createKeyRotationTransaction: Route = (post & path("createKeyRotationTransaction")) {
    withBasicAuth {
      _ => {
        entity(as[ReqCreateKeyRotationTransaction]) { body =>
          circuitType match {
            case NaiveThresholdSignatureCircuit =>
              ApiResponseUtil.toResponse(ErrorBadCircuit("The current circuit doesn't support key rotation transaction!", JOptional.empty()))
            case NaiveThresholdSignatureCircuitWithKeyRotation =>
              applyOnNodeView { sidechainNodeView =>

                checkKeyRotationProofValidity(body, sidechainNodeView.getNodeState.getWithdrawalEpochInfo.epoch) match {
                  case Some(errorResponse) => ApiResponseUtil.toResponse(errorResponse)
                  case None =>

                    val data = encodeSubmitKeyRotationRequestCmd(body)
                    val gasInfo = body.gasInfo

                    // default gas related params
                    val baseFee = sidechainNodeView.getNodeHistory.getBestBlock.header.baseFee
                    var maxPriorityFeePerGas = BigInteger.valueOf(120)
                    var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
                    var gasLimit = BigInteger.valueOf(500000)

                    if (gasInfo.isDefined) {
                      maxFeePerGas = gasInfo.get.maxFeePerGas
                      maxPriorityFeePerGas = gasInfo.get.maxPriorityFeePerGas
                      gasLimit = gasInfo.get.gasLimit
                    }

                    val txCost = maxFeePerGas.multiply(gasLimit)
                    val secret = getFittingSecret(sidechainNodeView, None, txCost)
                    secret match {
                      case Some(secret) =>

                        val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(secret.publicImage.address))
                        val tmpTx: EthereumTransaction = new EthereumTransaction(
                          params.chainId,
                          JOptional.of(new AddressProposition(CertificateKeyRotationMsgProcessor.CertificateKeyRotationContractAddress)),
                          nonce,
                          gasLimit,
                          maxPriorityFeePerGas,
                          maxFeePerGas,
                          BigInteger.ZERO,
                          EthereumTransactionUtils.getDataFromString(data),
                          null
                        )
                        validateAndSendTransaction(signTransactionWithSecret(secret, tmpTx))
                      case None =>
                        ApiResponseUtil.toResponse(ErrorInsufficientBalance("No account with enough balance found", JOptional.empty()))
                    }
                }
              }
          }
        }
      }
    }
  }

  def sendMultisigKeysOwnership: Route = (post & path("sendMultisigKeysOwnership")) {
    withBasicAuth {
      _ => {
        entity(as[ReqCreateMultisigMcAddrOwnership]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val valueInWei = BigInteger.ZERO

            // default gas related params
            val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
            var maxPriorityFeePerGas = BigInteger.valueOf(120)
            var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
            var gasLimit = BigInteger.valueOf(500000)

            if (body.gasInfo.isDefined) {
              maxFeePerGas = body.gasInfo.get.maxFeePerGas
              maxPriorityFeePerGas = body.gasInfo.get.maxPriorityFeePerGas
              gasLimit = body.gasInfo.get.gasLimit
            }

            val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))

            val secret = getFittingSecret(sidechainNodeView, Some(body.ownershipInfo.scAddress), txCost)

            secret match {
              case Some(secret) =>
                val fromAddress = secret.publicImage.address
                val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(fromAddress))
                Try {
                  // it throws if the parameters are invalid
                  encodeAddNewOwnershipCmdRequest(body.ownershipInfo)
                } match {

                  case Success(dataBytes) =>

                    val ownershipId = getOwnershipId(body.ownershipInfo.mcMultisigAddress)

                    if (sidechainNodeView.getNodeState.ownershipDataExist(ownershipId)) {
                      ApiResponseUtil.toResponse(GenericTransactionError(s"Mc address: ${body.ownershipInfo.mcMultisigAddress} is already associated", JOptional.empty()))
                    } else {
                      val tmpTx: EthereumTransaction = new EthereumTransaction(
                        params.chainId,
                        JOptional.of(new AddressProposition(MC_ADDR_OWNERSHIP_SMART_CONTRACT_ADDRESS)),
                        nonce,
                        gasLimit,
                        maxPriorityFeePerGas,
                        maxFeePerGas,
                        valueInWei,
                        dataBytes,
                        null
                      )
                      validateAndSendTransaction(signTransactionWithSecret(secret, tmpTx))
                    }

                  case Failure(exception) =>
                    ApiResponseUtil.toResponse(GenericTransactionError(s"Invalid input parameters", JOptional.of(exception)))
                }

              case None =>
                ApiResponseUtil.toResponse(ErrorInsufficientBalance(s"Account ${body.ownershipInfo.scAddress} is invalid or has insufficient balance", JOptional.empty()))
            }
          }
        }
      }
    }
  }

  def sendKeysOwnership: Route = (post & path("sendKeysOwnership")) {
    withBasicAuth {
      _ => {
        entity(as[ReqCreateMcAddrOwnership]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val valueInWei = BigInteger.ZERO

            // default gas related params
            val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
            var maxPriorityFeePerGas = BigInteger.valueOf(120)
            var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
            var gasLimit = BigInteger.valueOf(500000)

            if (body.gasInfo.isDefined) {
              maxFeePerGas = body.gasInfo.get.maxFeePerGas
              maxPriorityFeePerGas = body.gasInfo.get.maxPriorityFeePerGas
              gasLimit = body.gasInfo.get.gasLimit
            }

            val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))

            val secret = getFittingSecret(sidechainNodeView, Some(body.ownershipInfo.scAddress), txCost)

            secret match {
              case Some(secret) =>
                val fromAddress = secret.publicImage.address
                val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(fromAddress))
                Try {
                  // it throws if the parameters are invalid
                  encodeAddNewOwnershipCmdRequest(body.ownershipInfo)
                } match {

                  case Success(dataBytes) =>

                    val ownershipId = getOwnershipId(body.ownershipInfo.mcTransparentAddress)

                    if (sidechainNodeView.getNodeState.ownershipDataExist(ownershipId)) {
                      ApiResponseUtil.toResponse(GenericTransactionError(s"Mc address: ${body.ownershipInfo.mcTransparentAddress} is already associated", JOptional.empty()))
                    } else {
                      val tmpTx: EthereumTransaction = new EthereumTransaction(
                        params.chainId,
                        JOptional.of(new AddressProposition(MC_ADDR_OWNERSHIP_SMART_CONTRACT_ADDRESS)),
                        nonce,
                        gasLimit,
                        maxPriorityFeePerGas,
                        maxFeePerGas,
                        valueInWei,
                        dataBytes,
                        null
                      )
                      validateAndSendTransaction(signTransactionWithSecret(secret, tmpTx))
                    }

                  case Failure(exception) =>
                    ApiResponseUtil.toResponse(GenericTransactionError(s"Invalid input parameters", JOptional.of(exception)))
                }

              case None =>
                ApiResponseUtil.toResponse(ErrorInsufficientBalance(s"Account ${body.ownershipInfo.scAddress} is invalid or has insufficient balance", JOptional.empty()))
            }
          }
        }
      }
    }
  }

  def removeKeysOwnership: Route = (post & path("removeKeysOwnership")) {
    withBasicAuth {
      _ => {
        entity(as[ReqRemoveMcAddrOwnership]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val valueInWei = BigInteger.ZERO

            // default gas related params
            val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
            var maxPriorityFeePerGas = BigInteger.valueOf(120)
            var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
            var gasLimit = BigInteger.valueOf(500000)

            if (body.gasInfo.isDefined) {
              maxFeePerGas = body.gasInfo.get.maxFeePerGas
              maxPriorityFeePerGas = body.gasInfo.get.maxPriorityFeePerGas
              gasLimit = body.gasInfo.get.gasLimit
            }

            val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))

            val secret = getFittingSecret(sidechainNodeView, Some(body.ownershipInfo.scAddress), txCost)

            secret match {
              case Some(secret) =>
                val fromAddress = secret.publicImage.address
                val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(fromAddress))
                Try {
                  // it throws if the parameters are invalid
                  encodeRemoveOwnershipCmdRequest(body.ownershipInfo)
                } match {

                  case Success(dataBytes) =>

                    val ownershipId = body.ownershipInfo.mcTransparentAddress match {
                      case Some(mcAddr) => Some(getOwnershipId(mcAddr))
                      case None => None
                    }

                    if (ownershipId.isDefined && !sidechainNodeView.getNodeState.ownershipDataExist(ownershipId.get)) {
                      ApiResponseUtil.toResponse(GenericTransactionError(s"Account $fromAddress not linked to mc address: ${body.ownershipInfo.mcTransparentAddress.get}", JOptional.empty()))
                    } else {
                      val tmpTx: EthereumTransaction = new EthereumTransaction(
                        params.chainId,
                        JOptional.of(new AddressProposition(MC_ADDR_OWNERSHIP_SMART_CONTRACT_ADDRESS)),
                        nonce,
                        gasLimit,
                        maxPriorityFeePerGas,
                        maxFeePerGas,
                        valueInWei,
                        dataBytes,
                        null
                      )
                      validateAndSendTransaction(signTransactionWithSecret(secret, tmpTx))
                    }

                  case Failure(exception) =>
                    ApiResponseUtil.toResponse(GenericTransactionError(s"Invalid input parameters", JOptional.of(exception)))
                }

              case None =>
                ApiResponseUtil.toResponse(ErrorInsufficientBalance(s"Account ${body.ownershipInfo.scAddress} is invalid or has insufficient balance", JOptional.empty()))
            }
          }
        }
      }
    }
  }


  def getKeysOwnership: Route = (post & path("getKeysOwnership")) {
    withBasicAuth {
      _ => {
        entity(as[ReqGetMcAddrOwnership]) { body =>

          val scAddrNoPrefixOpt = body.scAddressOpt match  {
            case Some(str) =>
              Some(normalizeScAddress(str))
            case None => None
          }
          withNodeView { sidechainNodeView =>
            val accountState = sidechainNodeView.getNodeState
            val listOfMcAddrOwnerships = accountState.getListOfMcAddrOwnerships(scAddrNoPrefixOpt)

            val resultMap = listOfMcAddrOwnerships.groupBy(_.scAddress).map {
              case (k,v) => (Keys.toChecksumAddress(k),v.map(_.mcTransparentAddress))
            }

            ApiResponseUtil.toResponse(RespMcAddrOwnership(resultMap))

          }
        }
      }
    }
  }


  def getKeysOwnerScAddresses: Route = (post & path("getKeysOwnerScAddresses")) {
    withBasicAuth {
      _ => {
        entity(as[ReqGetOwnerScAddresses]) { _ =>
          withNodeView { sidechainNodeView =>
            val scAddresses = sidechainNodeView.getNodeState.getListOfOwnerScAddresses()
            val resultMap = scAddresses.map(k => Keys.toChecksumAddress(k.scAddress))
            ApiResponseUtil.toResponse(RespOwnersScAddresses(resultMap))
          }
        }
      }
    }
  }


  def invokeProxyCall: Route = (post & path("invokeProxyCall")) {
    withBasicAuth {
      _ => {
        entity(as[ReqInvokeProxyCall]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val valueInWei = BigInteger.ZERO

            // default gas related params
            val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
            var maxPriorityFeePerGas = BigInteger.valueOf(120)
            var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
            var gasLimit = BigInteger.valueOf(500000)

            if (body.gasInfo.isDefined) {
              maxFeePerGas = body.gasInfo.get.maxFeePerGas
              maxPriorityFeePerGas = body.gasInfo.get.maxPriorityFeePerGas
              gasLimit = body.gasInfo.get.gasLimit
            }

            val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))

            val secret = getFittingSecret(sidechainNodeView, None, txCost)

            secret match {
              case Some(secret) =>

                val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(secret.publicImage.address))
                val dataBytes = encodeInvokeProxyCallCmdRequest(body.invokeInfo)
                val tmpTx: EthereumTransaction = new EthereumTransaction(
                  params.chainId,
                  JOptional.of(new AddressProposition(PROXY_SMART_CONTRACT_ADDRESS)),
                  nonce,
                  gasLimit,
                  maxPriorityFeePerGas,
                  maxFeePerGas,
                  valueInWei,
                  dataBytes,
                  null
                )
                validateAndSendTransaction(signTransactionWithSecret(secret, tmpTx))
              case None =>
                ApiResponseUtil.toResponse(ErrorInsufficientBalance("No account with enough balance found", JOptional.empty()))
            }
          }
        }
      }
    }
  }



  def invokeProxyStaticCall: Route = (post & path("invokeProxyStaticCall")) {
    withBasicAuth {
      _ => {
        entity(as[ReqInvokeProxyCall]) { body =>
          // lock the view and try to create CoreTransaction
          applyOnNodeView { sidechainNodeView =>
            val valueInWei = ZenWeiConverter.convertZenniesToWei(0)

            // default gas related params
            val baseFee = sidechainNodeView.getNodeState.getNextBaseFee
            var maxPriorityFeePerGas = BigInteger.valueOf(120)
            var maxFeePerGas = BigInteger.TWO.multiply(baseFee).add(maxPriorityFeePerGas)
            var gasLimit = BigInteger.valueOf(500000)

            if (body.gasInfo.isDefined) {
              maxFeePerGas = body.gasInfo.get.maxFeePerGas
              maxPriorityFeePerGas = body.gasInfo.get.maxPriorityFeePerGas
              gasLimit = body.gasInfo.get.gasLimit
            }

            val txCost = valueInWei.add(maxFeePerGas.multiply(gasLimit))

            val secret = getFittingSecret(sidechainNodeView, None, txCost)

            secret match {
              case Some(secret) =>

                val nonce = body.nonce.getOrElse(sidechainNodeView.getNodeState.getNonce(secret.publicImage.address))
                val dataBytes = encodeInvokeProxyStaticCallCmdRequest(body.invokeInfo)
                val tmpTx: EthereumTransaction = new EthereumTransaction(
                  params.chainId,
                  JOptional.of(new AddressProposition(PROXY_SMART_CONTRACT_ADDRESS)),
                  nonce,
                  gasLimit,
                  maxPriorityFeePerGas,
                  maxFeePerGas,
                  valueInWei,
                  dataBytes,
                  null
                )
                validateAndSendTransaction(signTransactionWithSecret(secret, tmpTx))
              case None =>
                ApiResponseUtil.toResponse(ErrorInsufficientBalance("No account with enough balance found", JOptional.empty()))
            }
          }
        }
      }
    }
  }

  def encodeRegisterOrUpdateForgerCmdRequest(operation: String, blockSignPubKey: PublicKey25519Proposition, vrfPubKey: VrfPublicKey, rewardShare: Int, smartcontract_address: AddressProposition, sign1: Signature25519, sign2: VrfProof): Array[Byte] = {
    val cmdInput = RegisterOrUpdateForgerCmdInput(ForgerPublicKeys(blockSignPubKey, vrfPubKey), rewardShare, smartcontract_address.address(), sign1, sign2)
    Bytes.concat(BytesUtils.fromHexString(operation), cmdInput.encode())
  }


  def encodeAddNewStakeCmdRequest(forgerStakeInfo: TransactionForgerOutput): Array[Byte] = {
    val blockSignPublicKey = new PublicKey25519Proposition(BytesUtils.fromHexString(forgerStakeInfo.blockSignPublicKey.getOrElse(forgerStakeInfo.ownerAddress)))
    val vrfPubKey = new VrfPublicKey(BytesUtils.fromHexString(forgerStakeInfo.vrfPubKey))
    val addForgerStakeInput = AddNewStakeCmdInput(ForgerPublicKeys(blockSignPublicKey, vrfPubKey), new Address("0x" + forgerStakeInfo.ownerAddress))

    Bytes.concat(BytesUtils.fromHexString(ForgerStakeMsgProcessor.AddNewStakeCmd), addForgerStakeInput.encode())
  }

  def encodeInvokeProxyStaticCallCmdRequest(invokeInfo: TransactionInvokeProxyCall): Array[Byte] = {
    val invokeInput = InvokeSmartContractCmdInput(new Address("0x" + invokeInfo.contractAddress), invokeInfo.dataStr)

    Bytes.concat(BytesUtils.fromHexString(ProxyMsgProcessor.InvokeSmartContractStaticCallCmd), invokeInput.encode())
  }

  def encodeInvokeProxyCallCmdRequest(invokeInfo: TransactionInvokeProxyCall): Array[Byte] = {
    val invokeInput = InvokeSmartContractCmdInput(new Address("0x" + invokeInfo.contractAddress), invokeInfo.dataStr)

    Bytes.concat(BytesUtils.fromHexString(ProxyMsgProcessor.InvokeSmartContractCallCmd), invokeInput.encode())
  }

  def encodeOpenStakeCmdRequest(forgerIndex: Int, signature: Signature25519): Array[Byte] = {
    val openStakeForgerListInput = OpenStakeForgerListCmdInput(forgerIndex, signature)
    Bytes.concat(BytesUtils.fromHexString(ForgerStakeMsgProcessor.OpenStakeForgerListCmd),
      openStakeForgerListInput.encode())
  }

  def encodeSpendStakeCmdRequest(signatureSecp256k1: SignatureSecp256k1, stakeId: String): Array[Byte] = {
    val spendForgerStakeInput = RemoveStakeCmdInput(BytesUtils.fromHexString(stakeId), signatureSecp256k1)
    Bytes.concat(BytesUtils.fromHexString(ForgerStakeMsgProcessor.RemoveStakeCmd), spendForgerStakeInput.encode())
  }

  def encodeAddNewWithdrawalRequestCmd(withdrawal: TransactionWithdrawalRequest): Try[Array[Byte]] = {
    Try(BytesUtils.fromHorizenMcTransparentKeyAddress(withdrawal.mainchainAddress, params)).map {
      pubKeyHash =>
        // Keep in mind that check MC rpc `getnewaddress` returns standard address with hash inside in LE
        // different to `getnewaddress "" true` hash that is in BE endianness.
        val mcAddrHash = MCPublicKeyHashPropositionSerializer.getSerializer.parseBytes(pubKeyHash)
        val addWithdrawalRequestInput = AddWithdrawalRequestCmdInput(mcAddrHash)
        Bytes.concat(BytesUtils.fromHexString(WithdrawalMsgProcessor.AddNewWithdrawalReqCmdSig), addWithdrawalRequestInput.encode())
    }
  }


  def encodeAddNewOwnershipCmdRequest(ownershipInfo: TransactionCreateMcAddrOwnershipInfo): Array[Byte] = {
    // this throws if any of sc and mc addresses is not valid
    checkMcAddresses(ownershipInfo.mcTransparentAddress, params)

    // this throws if the signature is not correctly base64 encoded
    getMcSignature(ownershipInfo.mcSignature)

    val addMcAddrOwnershipInput = AddNewOwnershipCmdInput(ownershipInfo.mcTransparentAddress, ownershipInfo.mcSignature)

    Bytes.concat(BytesUtils.fromHexString(McAddrOwnershipMsgProcessor.AddNewOwnershipCmd), addMcAddrOwnershipInput.encode())
  }


  def encodeAddNewOwnershipCmdRequest(ownershipInfo: TransactionCreateMultisigMcAddrOwnershipInfo): Array[Byte] = {

    // this throws if an invalid redeemScript structure is passed
    checkMcRedeemScriptForMultisig(ownershipInfo.redeemScript)

    require(checkMultisigAddress(ownershipInfo.mcMultisigAddress, ownershipInfo.redeemScript, params), "Wrong redeem script hash")

    // this throws if the signatures are not correctly base64 encoded
    ownershipInfo.mcSignatures.foreach(getMcSignature)

    val addMcAddrOwnershipInput = AddNewMultisigOwnershipCmdInput(ownershipInfo.mcMultisigAddress, ownershipInfo.redeemScript, ownershipInfo.mcSignatures)

    Bytes.concat(BytesUtils.fromHexString(McAddrOwnershipMsgProcessor.AddNewMultisigOwnershipCmd), addMcAddrOwnershipInput.encode())

  }

  def encodeRemoveOwnershipCmdRequest(ownershipInfo: TransactionRemoveMcAddrOwnershipInfo): Array[Byte] = {
    // this throws if any of sc and mc addresses is not valid
    if (ownershipInfo.mcTransparentAddress.isDefined)
      checkMcAddresses(ownershipInfo.mcTransparentAddress.get, params)

    val removeMcAddrOwnershipInput = RemoveOwnershipCmdInput(ownershipInfo.mcTransparentAddress)

    Bytes.concat(BytesUtils.fromHexString(McAddrOwnershipMsgProcessor.RemoveOwnershipCmd), removeMcAddrOwnershipInput.encode())

  }

  private def checkKeyRotationProofValidity(body: ReqCreateKeyRotationTransaction, epoch: Int): Option[ErrorResponse] = {
    val index = body.keyIndex
    val keyType = body.keyType
    val newKey = SchnorrPropositionSerializer.getSerializer.parseBytesAndCheck(BytesUtils.fromHexString(body.newKey))
    val newKeySignature = SchnorrSignatureSerializer.getSerializer.parseBytes(BytesUtils.fromHexString(body.newKeySignature))
    if (index < 0 || index >= params.signersPublicKeys.length)
      return Some(ErrorInvalidKeyRotationProof(s"Key rotation proof - key index out of range: $index"))

    if (keyType < 0 || keyType >= KeyRotationProofTypes.maxId)
      return Some(ErrorInvalidKeyRotationProof(s"Key rotation proof - key type enumeration value invalid: $keyType"))

    val messageToSign = KeyRotationProofTypes(keyType) match {
      case SigningKeyRotationProofType => CryptoLibProvider.thresholdSignatureCircuitWithKeyRotation
        .getMsgToSignForSigningKeyUpdate(newKey.pubKeyBytes(), epoch, params.sidechainId)
      case MasterKeyRotationProofType => CryptoLibProvider.thresholdSignatureCircuitWithKeyRotation
        .getMsgToSignForMasterKeyUpdate(newKey.pubKeyBytes(), epoch, params.sidechainId)
    }

    if (!newKeySignature.isValid(newKey, messageToSign))
      return Some(ErrorInvalidKeyRotationProof(s"Key rotation proof - self signature is invalid: $index"))
    None
  }

  override def listOfDisabledEndpoints(params: NetworkParams): Seq[(EndpointPrefix, EndpointPath, Option[ErrorMsg])] = {

    val proxyRoutes = params match {
      case _: RegTestParams => Seq.empty
      case _ =>
        val error = Some("This operation is enabled only on RegTest network")
        Seq(
        (transactionPathPrefix, "invokeProxyCall", error),
        (transactionPathPrefix, "invokeProxyStaticCall", error),
      )
    }

    if (!params.isHandlingTransactionsEnabled) {
      val error = Some(ErrorNotEnabledOnSeederNode.description)
      Seq(
        (transactionPathPrefix, "createLegacyEIP155Transaction", error),
        (transactionPathPrefix, "createEIP1559Transaction", error),
        (transactionPathPrefix, "createLegacyTransaction", error),
        (transactionPathPrefix, "sendTransaction", error),
        (transactionPathPrefix, "signTransaction", error),
        (transactionPathPrefix, "makeForgerStake", error),
        (transactionPathPrefix, "withdrawCoins", error),
        (transactionPathPrefix, "spendForgingStake", error),
        (transactionPathPrefix, "createSmartContract", error),
        (transactionPathPrefix, "openForgerList", error),
        (transactionPathPrefix, "createKeyRotationTransaction", error),
        (transactionPathPrefix, "sendKeysOwnership", error),
        (transactionPathPrefix, "removeKeysOwnership", error),
        (transactionPathPrefix, "sendMultisigKeysOwnership", error),
        (transactionPathPrefix, "registerForger", error),
        (transactionPathPrefix, "updateForger", error),
      ) ++ proxyRoutes
    } else
      proxyRoutes
  }

}

object AccountTransactionRestScheme {

  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class RespAllWithdrawalRequests(listOfWR: List[WithdrawalRequest]) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class RespForgerStakes(stakes: List[AccountForgingStakeInfo]) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class RespPagedForgerStakes(nextPos: Int, stakes: List[AccountForgingStakeInfo]) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class RespPagedForgerStakesByForger(nextPos: Int, stakes: List[StakeDataDelegator]) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class RespPagedForgerStakesByDelegator(nextPos: Int, stakes: List[StakeDataForger]) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class RespMcAddrOwnership(keysOwnership: Map[String, Seq[String]]) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class RespOwnersScAddresses(owners: Seq[String]) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
  private [api] case class RespForgerInfo(
                                 blockSign: PublicKey25519Proposition,
                                 vrfPubKey: VrfPublicKey,
                                 openForgersVote: Int)
  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class RespAllowedForgerList(allowedForgers: List[RespForgerInfo]) extends SuccessResponse

  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class TransactionWithdrawalRequest(mainchainAddress: String, @JsonDeserialize(contentAs = classOf[java.lang.Long]) value: Long)

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class TransactionForgerOutput(ownerAddress: String, blockSignPublicKey: Option[String], vrfPubKey: String, value: Long)

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class TransactionCreateMcAddrOwnershipInfo(var scAddress: String, mcTransparentAddress: String, mcSignature: String)

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class TransactionCreateMultisigMcAddrOwnershipInfo(
                                                                            var scAddress: String,
                                                                            mcMultisigAddress: String,
                                                                            mcSignatures: Array[String],
                                                                            redeemScript: String)

  private[horizen] case class TransactionRemoveMcAddrOwnershipInfo(var scAddress: String, mcTransparentAddress: Option[String])

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class TransactionInvokeProxyCall(contractAddress: String, dataStr: String)

  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class EIP1559GasInfo(gasLimit: BigInteger, maxPriorityFeePerGas: BigInteger, maxFeePerGas: BigInteger) {
    require(gasLimit.signum() > 0, "Gas limit can not be 0")
    require(maxPriorityFeePerGas.signum() > 0, "MaxPriorityFeePerGas must be greater than 0")
    require(maxFeePerGas.signum() > 0, "MaxFeePerGas must be greater than 0")
  }

  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class ReqWithdrawCoins(nonce: Option[BigInteger],
                                           withdrawalRequest: TransactionWithdrawalRequest,
                                           gasInfo: Option[EIP1559GasInfo]) {
    require(withdrawalRequest != null, "Withdrawal request info must be provided")
  }

  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class ReqAllWithdrawalRequests(epochNum: Int) {
    require(epochNum >= 0, "Epoch number must be positive")
  }

  def encodeSubmitKeyRotationRequestCmd(request: ReqCreateKeyRotationTransaction): String = {
    val keyType = KeyRotationProofTypes(request.keyType)
    val index = request.keyIndex
    val newKey = SchnorrPropositionSerializer.getSerializer.parseBytesAndCheck(BytesUtils.fromHexString(request.newKey))
    val signingSignature = SchnorrSignatureSerializer.getSerializer.parseBytes(BytesUtils.fromHexString(request.signingKeySignature))
    val masterSignature = SchnorrSignatureSerializer.getSerializer.parseBytes(BytesUtils.fromHexString(request.masterKeySignature))
    val newKeySignature = SchnorrSignatureSerializer.getSerializer.parseBytes(BytesUtils.fromHexString(request.newKeySignature))
    val keyRotationProof = KeyRotationProof(
      keyType,
      index,
      newKey,
      signingSignature,
      masterSignature
    )
    BytesUtils.toHexString(Bytes.concat(
      BytesUtils.fromHexString(CertificateKeyRotationMsgProcessor.SubmitKeyRotationReqCmdSig),
      SubmitKeyRotationCmdInput(keyRotationProof, newKeySignature).encode()
    ))
  }

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqCreateForgerStake(
                                                    nonce: Option[BigInteger],
                                                    forgerStakeInfo: TransactionForgerOutput,
                                                    gasInfo: Option[EIP1559GasInfo]
                                                  ) {
    require(forgerStakeInfo != null, "Forger stake info must be provided")
  }

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqCreateMcAddrOwnership(
                                                        nonce: Option[BigInteger],
                                                        ownershipInfo: TransactionCreateMcAddrOwnershipInfo,
                                                        gasInfo: Option[EIP1559GasInfo]
                                                      ) {
    require(ownershipInfo != null, "MC address ownership info must be provided")
    ownershipInfo.scAddress = normalizeScAddress(ownershipInfo.scAddress)
  }

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqCreateMultisigMcAddrOwnership(
                                                        nonce: Option[BigInteger],
                                                        ownershipInfo: TransactionCreateMultisigMcAddrOwnershipInfo,
                                                        gasInfo: Option[EIP1559GasInfo]
                                                      ) {
    require(ownershipInfo != null, "Multisig MC address ownership info must be provided")
    ownershipInfo.scAddress = normalizeScAddress(ownershipInfo.scAddress)
  }

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqRemoveMcAddrOwnership(
                                                        nonce: Option[BigInteger],
                                                        var ownershipInfo: TransactionRemoveMcAddrOwnershipInfo,
                                                        gasInfo: Option[EIP1559GasInfo]
                                                      ) {
    require(ownershipInfo != null, "MC address ownership info must be provided")
    ownershipInfo.scAddress = normalizeScAddress(ownershipInfo.scAddress)
    require(ownershipInfo.scAddress.length == 2*Address.LENGTH, s"Invalid SC address length=${ownershipInfo.scAddress.length}")
    // for the time being do not allow null mc address. In future we can use a null value for removing all sc address ownerships
    require(ownershipInfo.mcTransparentAddress.isDefined, "MC address must be specified")
  }

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqGetMcAddrOwnership(scAddressOpt: Option[String]) {
    require(scAddressOpt != null, "SC address opt must be provided")
  }

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqGetOwnerScAddresses() {}


  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqInvokeProxyCall(
                                                    nonce: Option[BigInteger],
                                                    invokeInfo: TransactionInvokeProxyCall,
                                                    gasInfo: Option[EIP1559GasInfo]
                                                  ) {
  }

  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class ReqOpenStakeForgerList(
                                                nonce: Option[BigInteger],
                                                forgerIndex: Int,
                                                gasInfo: Option[EIP1559GasInfo]) {
    require (forgerIndex >=0, "Forger index must be non negative")
  }


  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class ReqCreateContract(
                                             nonce: Option[BigInteger],
                                             contractCode: String,
                                             gasInfo: Option[EIP1559GasInfo]) {
    require(contractCode.nonEmpty, "Contract code must be provided")
  }

  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class ReqCreateKeyRotationTransaction(keyType: Int,
                                                          keyIndex: Int,
                                                          newKey: String,
                                                          signingKeySignature: String,
                                                          masterKeySignature: String,
                                                          newKeySignature: String,
                                                          nonce: Option[BigInteger],
                                                          gasInfo: Option[EIP1559GasInfo]) {
    require(keyIndex >= 0, "Key index negative")
    require(newKey.nonEmpty, "newKey is empty")
    require(signingKeySignature.nonEmpty, "signingKeySignature is empty")
    require(masterKeySignature.nonEmpty, "masterKeySignature is empty")
    require(newKeySignature.nonEmpty, "newKeySignature is empty")
  }

  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class ReqSpendForgingStake(
                                                nonce: Option[BigInteger],
                                                stakeId: String,
                                                gasInfo: Option[EIP1559GasInfo]) {
    require(stakeId.nonEmpty, "Signature data must be provided")
  }


  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqPagedForgingStakes(startPos: Int = 0, size: Int = 10) {
    require(size > 0 , "Size must be positive")
  }


  trait ReqBaseForger {
    def nonce: Option[BigInteger]
    def blockSignPubKey: String
    def vrfPubKey: String
    def gasInfo: Option[EIP1559GasInfo]
  }


  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqRegisterForger(
                                                 nonce: Option[BigInteger],
                                                 blockSignPubKey: String,
                                                 vrfPubKey: String,
                                                 rewardShare: Option[Int],
                                                 rewardAddress: Option[String],
                                                 stakedAmount: Long, // in zennies
                                                 gasInfo: Option[EIP1559GasInfo]) extends ReqBaseForger {
  }


  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqUpdateForger(
                                               nonce: Option[BigInteger],
                                               blockSignPubKey: String,
                                               vrfPubKey: String,
                                               rewardShare: Int,
                                               rewardAddress: String,
                                               gasInfo: Option[EIP1559GasInfo]) extends ReqBaseForger {

  }

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqPagedForgerStakesByForger(
                                                            blockSignPubKey: String,
                                                            vrfPubKey: String,
                                                            startPos: Int = 0,
                                                            size: Int = 10) {
     require(size > 0 , "Size must be positive")
  }

  @JsonView(Array(classOf[Views.Default]))
  private[horizen] case class ReqPagedForgerStakesByDelegator(
                                                               delegatorAddress: String,
                                                               startPos: Int = 0,
                                                               size: Int = 10) {
    require(size > 0 , "Size must be positive")
  }


  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class ReqEIP1559Transaction(
                                                 to: Option[String],
                                                 from: Option[String],
                                                 nonce: Option[BigInteger],
                                                 gasLimit: BigInteger,
                                                 maxPriorityFeePerGas: BigInteger,
                                                 maxFeePerGas: BigInteger,
                                                 value: Option[BigInteger],
                                                 data: String,
                                                 signature_v: Option[String] = None,
                                                 signature_r: Option[String] = None,
                                                 signature_s: Option[String] = None,
                                                 outputRawBytes: Option[Boolean] = None) {
    require(
      (signature_v.nonEmpty && signature_r.nonEmpty && signature_s.nonEmpty)
        || (signature_v.isEmpty && signature_r.isEmpty && signature_s.isEmpty),
      "Signature can not be partial"
    )
    require(gasLimit.signum() > 0, "Gas limit can not be 0")
    require(maxPriorityFeePerGas.signum() > 0, "MaxPriorityFeePerGas must be greater than 0")
    require(maxFeePerGas.signum() > 0, "MaxFeePerGas must be greater than 0")
    require(to.isEmpty || to.get.length == 40 /* address length without prefix 0x */ , "to is not empty but has the wrong length - do not use a 0x prefix")
    require(from.isEmpty || from.get.length == 40 /* address length without prefix 0x */ , "from is not empty but has the wrong length - do not use a 0x prefix")
  }

  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class ReqLegacyTransaction(
                                                to: Option[String],
                                                from: Option[String],
                                                nonce: Option[BigInteger],
                                                gasLimit: BigInteger,
                                                gasPrice: BigInteger,
                                                value: Option[BigInteger],
                                                data: String,
                                                signature_v: Option[String] = None,
                                                signature_r: Option[String] = None,
                                                signature_s: Option[String] = None,
                                                outputRawBytes: Option[Boolean] = None) {
    require(
      (signature_v.nonEmpty && signature_r.nonEmpty && signature_s.nonEmpty)
        || (signature_v.isEmpty && signature_r.isEmpty && signature_s.isEmpty),
      "Signature can not be partial"
    )
    require(gasLimit.signum() > 0, "Gas limit can not be 0")
    require(gasPrice.signum() > 0, "Gas price can not be 0")
    require(to.isEmpty || to.get.length == 40 /* address length without prefix 0x */ , "to is not empty but has the wrong length - do not use a 0x prefix")
    require(from.isEmpty || from.get.length == 40 /* address length without prefix 0x */ , "from is not empty but has the wrong length - do not use a 0x prefix")
  }

  @JsonView(Array(classOf[Views.Default]))
   private[horizen] case class ReqSignTransaction(from: Option[String], transactionBytes: String)


  def normalizeScAddress(str: String): String = {
    // we support format with and without prefix and checksum address format too, but
    // we normalize it to lowercase without prefix
    val str2 = if (str.startsWith("0x")) {
      str.substring(2)
    } else {
      str
    }
    str2.toLowerCase()
  }
}

object AccountTransactionErrorResponse {

  case class GenericTransactionError(description: String, exception: JOptional[Throwable]) extends ErrorResponse {
    override val code: String = "0204"
  }

  case class ErrorInsufficientBalance(description: String, exception: JOptional[Throwable]) extends ErrorResponse {
    override val code: String = "0205"
  }

  case class ErrorForgerStakeNotFound(description: String) extends ErrorResponse {
    override val code: String = "0206"
    override val exception: JOptional[Throwable] = JOptional.empty()
  }

  case class ErrorForgerStakeOwnerNotFound(description: String) extends ErrorResponse {
    override val code: String = "0207"
    override val exception: JOptional[Throwable] = JOptional.empty()
  }

  case class ErrorOpenForgersList(description: String, exception: JOptional[Throwable]) extends ErrorResponse {
    override val code: String = "0208"
  }

  case class ErrorInvalidKeyRotationProof(description: String) extends ErrorResponse {
    override val code: String = "0209"
    override val exception: JOptional[Throwable] = JOptional.empty()
  }

  case class ErrorInvalidMcAddress(description: String, exception: JOptional[Throwable]) extends ErrorResponse {
    override val code: String = "0210"
  }

  case class ErrorRegisterForgerInvalidRewardParams(description: String) extends ErrorResponse {
    override val code: String = "0211"
    override val exception: JOptional[Throwable] = JOptional.empty()
  }

  case class ErrorDisabledMethod() extends ErrorResponse {
    override val code: String = "0212"
    override val exception: JOptional[Throwable] = JOptional.empty()
    override val description: String = "Method is disabled after Fork 1.4. Use Forger Stakes Native Smart Contract V2"
  }

}
