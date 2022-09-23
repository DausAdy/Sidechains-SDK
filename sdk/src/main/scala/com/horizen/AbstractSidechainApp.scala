package com.horizen


import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler}
import com.horizen.api.http._
import com.horizen.block.{ProofOfWorkVerifier, SidechainBlockBase, SidechainBlockHeaderBase}
import com.horizen.certificatesubmitter.network.{CertificateSignaturesSpec, GetCertificateSignaturesSpec}
import com.horizen.companion._
import com.horizen.cryptolibprovider.{CommonCircuit, CryptoLibProvider}
import com.horizen.forge.MainchainSynchronizer
import com.horizen.params._
import com.horizen.proposition._
import com.horizen.secret.SecretSerializer
import com.horizen.serialization.JsonHorizenPublicKeyHashSerializer
import com.horizen.storage._
import com.horizen.transaction._
import com.horizen.transaction.mainchain.SidechainCreation
import com.horizen.utils.{BytesUtils, Pair}
import com.horizen.websocket.client._
import sparkz.core.app.Application
import sparkz.core.network.PeerFeature
import sparkz.core.network.message.MessageSpec
import sparkz.core.settings.SparkzSettings
import scorex.util.ScorexLogging

import java.lang.{Byte => JByte}
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{HashMap => JHashMap, List => JList}
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.io.{Codec, Source}
import scala.util.Try


abstract class AbstractSidechainApp
  (val sidechainSettings: SidechainSettings,
   val customSecretSerializers: JHashMap[JByte, SecretSerializer[SidechainTypes#SCS]],
   val customApiGroups: JList[ApplicationApiGroup],
   val rejectedApiPaths : JList[Pair[String, String]],
   val chainInfo : ChainInfo,
  )
  extends Application with ScorexLogging
{
  override type TX <: Transaction
  override type PMOD <: SidechainBlockBase[TX, _ <: SidechainBlockHeaderBase]

  override implicit lazy val settings: SparkzSettings = sidechainSettings.sparkzSettings

  private val storageList = mutable.ListBuffer[Storage]()

  log.info(s"Starting application with settings \n$sidechainSettings")

  override implicit def exceptionHandler: ExceptionHandler = SidechainApiErrorHandler.exceptionHandler

  override implicit def rejectionHandler: RejectionHandler = SidechainApiRejectionHandler.rejectionHandler

  override protected lazy val features: Seq[PeerFeature] = Seq()

  val stopAllInProgress : AtomicBoolean = new AtomicBoolean(false)
  def sidechainStopAll(fromEndpoint: Boolean = false): Unit

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(
    SidechainSyncInfoMessageSpec,
    // It can be no more Certificate signatures than the public keys for the Threshold Signature Circuit
    new GetCertificateSignaturesSpec(sidechainSettings.withdrawalEpochCertificateSettings.signersPublicKeys.size),
    new CertificateSignaturesSpec(sidechainSettings.withdrawalEpochCertificateSettings.signersPublicKeys.size)
  )

  protected val sidechainSecretsCompanion: SidechainSecretsCompanion = SidechainSecretsCompanion(customSecretSerializers)

  // Deserialize genesis block bytes
  val genesisBlock: PMOD

  val genesisPowData: Seq[(Int, Int)] = ProofOfWorkVerifier.parsePowData(sidechainSettings.genesisData.powData)

  val signersPublicKeys: Seq[SchnorrProposition] = sidechainSettings.withdrawalEpochCertificateSettings.signersPublicKeys
    .map(bytes => SchnorrPropositionSerializer.getSerializer.parseBytes(BytesUtils.fromHexString(bytes)))

  val calculatedSysDataConstant: Array[Byte] = CryptoLibProvider.sigProofThresholdCircuitFunctions.generateSysDataConstant(signersPublicKeys.map(_.bytes()).asJava, sidechainSettings.withdrawalEpochCertificateSettings.signersThreshold)
  log.info(s"calculated sysDataConstant is: ${BytesUtils.toHexString(calculatedSysDataConstant)}")

  val sidechainCreationOutput: SidechainCreation

  val forgerList: Seq[(PublicKey25519Proposition, VrfPublicKey)] = sidechainSettings.forger.allowedForgersList.map(el =>
    (PublicKey25519PropositionSerializer.getSerializer.parseBytes(BytesUtils.fromHexString(el.blockSignProposition)), VrfPublicKeySerializer.getSerializer.parseBytes(BytesUtils.fromHexString(el.vrfPublicKey))))

  // Init proper NetworkParams depend on MC network
  val params: NetworkParams = sidechainSettings.genesisData.mcNetwork match {
    case "regtest" => RegTestParams(
      sidechainId = BytesUtils.reverseBytes(BytesUtils.fromHexString(sidechainSettings.genesisData.scId)),
      sidechainGenesisBlockId = genesisBlock.id,
      genesisMainchainBlockHash = genesisBlock.mainchainHeaders.head.hash,
      parentHashOfGenesisMainchainBlock = genesisBlock.mainchainHeaders.head.hashPrevBlock,
      genesisPoWData = genesisPowData,
      mainchainCreationBlockHeight = sidechainSettings.genesisData.mcBlockHeight,
      sidechainGenesisBlockTimestamp = genesisBlock.timestamp,
      withdrawalEpochLength = sidechainSettings.genesisData.withdrawalEpochLength,
      signersPublicKeys = signersPublicKeys,
      signersThreshold = sidechainSettings.withdrawalEpochCertificateSettings.signersThreshold,
      certProvingKeyFilePath = sidechainSettings.withdrawalEpochCertificateSettings.certProvingKeyFilePath,
      certVerificationKeyFilePath = sidechainSettings.withdrawalEpochCertificateSettings.certVerificationKeyFilePath,
      calculatedSysDataConstant = calculatedSysDataConstant,
      initialCumulativeCommTreeHash = BytesUtils.fromHexString(sidechainSettings.genesisData.initialCumulativeCommTreeHash),
      cswProvingKeyFilePath = sidechainSettings.csw.cswProvingKeyFilePath,
      cswVerificationKeyFilePath = sidechainSettings.csw.cswVerificationKeyFilePath,
      restrictForgers = sidechainSettings.forger.restrictForgers,
      allowedForgersList = forgerList,
      sidechainCreationVersion = sidechainCreationOutput.getScCrOutput.version,
      chainId = chainInfo.regtestId
    )

    case "testnet" => TestNetParams(
      sidechainId = BytesUtils.reverseBytes(BytesUtils.fromHexString(sidechainSettings.genesisData.scId)),
      sidechainGenesisBlockId = genesisBlock.id,
      genesisMainchainBlockHash = genesisBlock.mainchainHeaders.head.hash,
      parentHashOfGenesisMainchainBlock = genesisBlock.mainchainHeaders.head.hashPrevBlock,
      genesisPoWData = genesisPowData,
      mainchainCreationBlockHeight = sidechainSettings.genesisData.mcBlockHeight,
      sidechainGenesisBlockTimestamp = genesisBlock.timestamp,
      withdrawalEpochLength = sidechainSettings.genesisData.withdrawalEpochLength,
      signersPublicKeys = signersPublicKeys,
      signersThreshold = sidechainSettings.withdrawalEpochCertificateSettings.signersThreshold,
      certProvingKeyFilePath = sidechainSettings.withdrawalEpochCertificateSettings.certProvingKeyFilePath,
      certVerificationKeyFilePath = sidechainSettings.withdrawalEpochCertificateSettings.certVerificationKeyFilePath,
      calculatedSysDataConstant = calculatedSysDataConstant,
      initialCumulativeCommTreeHash = BytesUtils.fromHexString(sidechainSettings.genesisData.initialCumulativeCommTreeHash),
      cswProvingKeyFilePath = sidechainSettings.csw.cswProvingKeyFilePath,
      cswVerificationKeyFilePath = sidechainSettings.csw.cswVerificationKeyFilePath,
      restrictForgers = sidechainSettings.forger.restrictForgers,
      allowedForgersList = forgerList,
      sidechainCreationVersion = sidechainCreationOutput.getScCrOutput.version,
      chainId = chainInfo.testnetId
    )

    case "mainnet" => MainNetParams(
      sidechainId = BytesUtils.reverseBytes(BytesUtils.fromHexString(sidechainSettings.genesisData.scId)),
      sidechainGenesisBlockId = genesisBlock.id,
      genesisMainchainBlockHash = genesisBlock.mainchainHeaders.head.hash,
      parentHashOfGenesisMainchainBlock = genesisBlock.mainchainHeaders.head.hashPrevBlock,
      genesisPoWData = genesisPowData,
      mainchainCreationBlockHeight = sidechainSettings.genesisData.mcBlockHeight,
      sidechainGenesisBlockTimestamp = genesisBlock.timestamp,
      withdrawalEpochLength = sidechainSettings.genesisData.withdrawalEpochLength,
      signersPublicKeys = signersPublicKeys,
      signersThreshold = sidechainSettings.withdrawalEpochCertificateSettings.signersThreshold,
      certProvingKeyFilePath = sidechainSettings.withdrawalEpochCertificateSettings.certProvingKeyFilePath,
      certVerificationKeyFilePath = sidechainSettings.withdrawalEpochCertificateSettings.certVerificationKeyFilePath,
      calculatedSysDataConstant = calculatedSysDataConstant,
      initialCumulativeCommTreeHash = BytesUtils.fromHexString(sidechainSettings.genesisData.initialCumulativeCommTreeHash),
      cswProvingKeyFilePath = sidechainSettings.csw.cswProvingKeyFilePath,
      cswVerificationKeyFilePath = sidechainSettings.csw.cswVerificationKeyFilePath,
      restrictForgers = sidechainSettings.forger.restrictForgers,
      allowedForgersList = forgerList,
      sidechainCreationVersion = sidechainCreationOutput.getScCrOutput.version,
      chainId = chainInfo.mainnetId
    )
    case _ => throw new IllegalArgumentException("Configuration file scorex.genesis.mcNetwork parameter contains inconsistent value.")
  }

  // Configure Horizen address json serializer specifying proper network type.
  JsonHorizenPublicKeyHashSerializer.setNetworkType(params)

  // Generate Coboundary Marlin Proving System dlog keys
  log.info(s"Generating Coboundary Marlin Proving System dlog keys. It may take some time.")
  if(!CryptoLibProvider.commonCircuitFunctions.generateCoboundaryMarlinDLogKeys()) {
    throw new IllegalArgumentException("Can't generate Coboundary Marlin ProvingSystem dlog keys.")
  }

  // Generate snark keys only if were not present before.
  if (!Files.exists(Paths.get(params.certVerificationKeyFilePath)) || !Files.exists(Paths.get(params.certProvingKeyFilePath))) {
    log.info("Generating Cert snark keys. It may take some time.")
    val expectedNumOfCustomFields = if (params.isCSWEnabled) CommonCircuit.CUSTOM_FIELDS_NUMBER_WITH_ENABLED_CSW else CommonCircuit.CUSTOM_FIELDS_NUMBER_WITH_DISABLED_CSW
    if (!CryptoLibProvider.sigProofThresholdCircuitFunctions.generateCoboundaryMarlinSnarkKeys(
        sidechainSettings.withdrawalEpochCertificateSettings.maxPks, params.certProvingKeyFilePath, params.certVerificationKeyFilePath, expectedNumOfCustomFields)) {
      throw new IllegalArgumentException("Can't generate Cert Coboundary Marlin ProvingSystem snark keys.")
    }
  }

  // Retrieve information for using a web socket connector
  lazy val communicationClient: WebSocketCommunicationClient = new WebSocketCommunicationClient()
  lazy val webSocketReconnectionHandler: WebSocketReconnectionHandler = new DefaultWebSocketReconnectionHandler(sidechainSettings.websocket)

  // Create the web socket connector and configure it
  lazy val webSocketConnector : WebSocketConnector with WebSocketChannel = new WebSocketConnectorImpl(
    sidechainSettings.websocket.address,
    sidechainSettings.websocket.connectionTimeout,
    communicationClient,
    webSocketReconnectionHandler
  )

  // Start the web socket connector
  val connectorStarted : Try[Unit] = webSocketConnector.start()

  // If the web socket connector can be started, maybe we would to associate a client to the web socket channel created by the connector
  if(connectorStarted.isSuccess)
    communicationClient.setWebSocketChannel(webSocketConnector)
  else if (sidechainSettings.withdrawalEpochCertificateSettings.submitterIsEnabled)
    throw new RuntimeException("Unable to connect to websocket. Certificate submitter needs connection to Mainchain.")

  // Init Forger with a proper web socket client
  val mainchainNodeChannel = new MainchainNodeChannelImpl(communicationClient, params)
  val mainchainSynchronizer = new MainchainSynchronizer(mainchainNodeChannel)

  /*
   * TODO this should be common code but nodeViewHolderRef here is still null, and lazy initialization does not always apply
   *  One possible approach could be to add some init() function in this base class and call it from concrete instances.
   *
  // Init Certificate Submitter
  lazy val certificateSubmitterRef: ActorRef = CertificateSubmitterRef(sidechainSettings, nodeViewHolderRef, params, mainchainNodeChannel)
  lazy val certificateSignaturesManagerRef: ActorRef = CertificateSignaturesManagerRef(networkControllerRef, certificateSubmitterRef, params, sidechainSettings.scorexSettings.network)

   * Moreover, for the time being we decide not to use ws server for accounts
  //Websocket server for the Explorer
  if(sidechainSettings.websocket.wsServer) {
    val webSocketServerActor: ActorRef = WebSocketServerRef(nodeViewHolderRef,sidechainSettings.websocket.wsServerPort)
  }

  // Init API
  var rejectedApiRoutes : Seq[SidechainRejectionApiRoute] = Seq[SidechainRejectionApiRoute]()
  rejectedApiPaths.asScala.foreach(path => rejectedApiRoutes = rejectedApiRoutes :+ SidechainRejectionApiRoute(path.getKey, path.getValue, settings.restApi, nodeViewHolderRef))

  // Once received developer's custom api, we need to create, for each of them, a SidechainApiRoute.
  // For do this, we use an instance of ApplicationApiRoute. This is an entry point between SidechainApiRoute and external java api.
  var applicationApiRoutes : Seq[ApplicationApiRoute] = Seq[ApplicationApiRoute]()
  customApiGroups.asScala.foreach(apiRoute => applicationApiRoutes = applicationApiRoutes :+ ApplicationApiRoute(settings.restApi, apiRoute, nodeViewHolderRef))
  */

  override val swaggerConfig: String = Source.fromResource("api/sidechainApi.yaml")(Codec.UTF8).getLines.mkString("\n")

  override def stopAll(): Unit = {
    super.stopAll()
    storageList.foreach(_.close())
  }

  protected def registerStorage(storage: Storage) : Storage = {
    storageList += storage
    storage
  }
}
