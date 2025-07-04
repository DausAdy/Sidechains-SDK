package io.horizen.utxo.certificatesubmitter

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.testkit.{TestActor, TestActorRef, TestProbe}
import akka.util.Timeout
import io.horizen._
import io.horizen.api.http.client.SecureEnclaveApiClient
import io.horizen.block._
import io.horizen.certificatesubmitter.AbstractCertificateSubmitter.InternalReceivableMessages.TryToGenerateCertificate
import io.horizen.certificatesubmitter.AbstractCertificateSubmitter.ReceivableMessages._
import io.horizen.certificatesubmitter.AbstractCertificateSubmitter.Timers.CertificateGenerationTimer
import io.horizen.certificatesubmitter.AbstractCertificateSubmitter._
import io.horizen.certificatesubmitter.dataproof.{CertificateData, CertificateDataWithoutKeyRotation}
import io.horizen.certificatesubmitter.keys.CertifiersKeys
import io.horizen.certificatesubmitter.strategies.{CeasingSidechain, CertificateSubmissionStrategy, CircuitStrategy, WithoutKeyRotationCircuitStrategy}
import io.horizen.chain.{MainchainBlockReferenceInfo, MainchainHeaderInfo, SidechainBlockInfo}
import io.horizen.consensus.ConsensusParamsUtil
import io.horizen.cryptolibprovider.CryptoLibProvider
import io.horizen.fixtures.FieldElementFixture
import io.horizen.fork.{ConsensusParamsFork, ConsensusParamsForkInfo, ForkManagerUtil, SimpleForkConfigurator}
import io.horizen.params.{CommonParams, NetworkParams, RegTestParams}
import io.horizen.proposition.{Proposition, SchnorrProposition}
import io.horizen.secret.{SchnorrKeyGenerator, SchnorrSecret}
import io.horizen.transaction.MC2SCAggregatedTransaction
import io.horizen.transaction.mainchain.{SidechainCreation, SidechainRelatedMainchainOutput}
import io.horizen.utils.{BytesUtils, TimeToEpochUtils, WithdrawalEpochInfo, ZenCoinsUtils}
import io.horizen.utxo.block.{SidechainBlock, SidechainBlockHeader}
import io.horizen.utxo.box.Box
import io.horizen.utxo.history.SidechainHistory
import io.horizen.utxo.mempool.SidechainMemoryPool
import io.horizen.utxo.storage.SidechainHistoryStorage
import io.horizen.utxo.state.SidechainState
import io.horizen.utxo.wallet.SidechainWallet
import io.horizen.websocket.client._
import org.junit.Assert._
import org.junit.{Assert, Before, Test}
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{reset, when}
import org.scalatestplus.junit.JUnitSuite
import org.scalatestplus.mockito.MockitoSugar
import sparkz.core.NodeViewHolder.CurrentView
import sparkz.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import sparkz.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import sparkz.core.settings.{RESTApiSettings, SparkzSettings}
import sparkz.util.ModifierId

import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.compat.java8.OptionConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}
import scala.language.postfixOps
import scala.util.{Random, Try}

class CertificateSubmitterTest extends JUnitSuite with MockitoSugar {
  implicit lazy val actorSystem: ActorSystem = ActorSystem("submitter-actor-test")
  implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("sparkz.executionContext")
  implicit val timeout: Timeout = 100 milliseconds
  private var consensusEpochAtWhichForkIsApplied: Int = _

  @Before
  def init(): Unit = {
    val forkConfigurator = new SimpleForkConfigurator()
    consensusEpochAtWhichForkIsApplied = forkConfigurator.forkActivation.regtest
    ForkManagerUtil.initializeForkManager(new SimpleForkConfigurator(), "regtest")
  }

  private def getMockedSettings(timeoutDuration: FiniteDuration, submitterIsEnabled: Boolean, signerIsEnabled: Boolean): SidechainSettings = {
    val mockedRESTSettings: RESTApiSettings = mock[RESTApiSettings]
    when(mockedRESTSettings.timeout).thenReturn(timeoutDuration)

    val mockedSidechainSettings: SidechainSettings = mock[SidechainSettings]
    when(mockedSidechainSettings.sparkzSettings).thenAnswer(_ => {
      val mockedSparkzSettings: SparkzSettings = mock[SparkzSettings]
      when(mockedSparkzSettings.restApi).thenAnswer(_ => mockedRESTSettings)
      mockedSparkzSettings
    })
    when(mockedSidechainSettings.withdrawalEpochCertificateSettings).thenAnswer(_ => {
      val mockedWithdrawalEpochCertificateSettings: WithdrawalEpochCertificateSettings = mock[WithdrawalEpochCertificateSettings]
      when(mockedWithdrawalEpochCertificateSettings.submitterIsEnabled).thenReturn(submitterIsEnabled)
      when(mockedWithdrawalEpochCertificateSettings.certificateSigningIsEnabled).thenReturn(signerIsEnabled)
      mockedWithdrawalEpochCertificateSettings
    })
    when(mockedSidechainSettings.remoteKeysManagerSettings).thenReturn(RemoteKeysManagerSettings())

    mockedSidechainSettings
  }

  def mockedMcBlockWithScCreation(genSysConstantOpt: Option[Array[Byte]]): MainchainBlockReference = {
    val mockedOutput = mock[MainchainTxSidechainCreationCrosschainOutput]
    when(mockedOutput.constantOpt).thenAnswer(_ => genSysConstantOpt)

    val mockedRefData: MainchainBlockReferenceData = mock[MainchainBlockReferenceData]
    when(mockedRefData.sidechainRelatedAggregatedTransaction).thenAnswer(_ => {
      val outputs: ListBuffer[SidechainRelatedMainchainOutput[_ <: Box[_ <: Proposition]]] = ListBuffer()
      outputs.append(new SidechainCreation(mockedOutput, new Array[Byte](32), 0))
      Some(new MC2SCAggregatedTransaction(outputs.asJava, MC2SCAggregatedTransaction.MC2SC_AGGREGATED_TRANSACTION_VERSION))
    })
    new MainchainBlockReference(mock[MainchainHeader], mockedRefData)
  }

  @Test
  def initializationFailure_MissingNodeViewHolder(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration, submitterIsEnabled = true, signerIsEnabled = true)

    val mockedSidechainNodeViewHolder = TestProbe()
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref

    val params: NetworkParams = mock[NetworkParams]
    val mainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]

    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolderRef, mock[SecureEnclaveApiClient], params, mainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))
    actorSystem.eventStream.subscribe(certificateSubmitterRef, SidechainAppEvents.SidechainApplicationStart.getClass)

    actorSystem.eventStream.publish(SidechainAppEvents.SidechainApplicationStart)

    val deathWatch = TestProbe()
    deathWatch.watch(certificateSubmitterRef)
    deathWatch.expectTerminated(certificateSubmitterRef, timeout.duration * 2)
  }

  @Test
  def initializationFailure_InvalidActualConstantData(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)

    // Initialization should fail because of invalid constant
    val calculatedSysDataConstant = new Array[Byte](32)
    Random.nextBytes(calculatedSysDataConstant)
    val expectedSysDataConstant = new Array[Byte](32)
    Random.nextBytes(expectedSysDataConstant)
    val expectedSysDataConstantOpt: Option[Array[Byte]] = Some(expectedSysDataConstant)

    val mockedSidechainNodeViewHolder = TestProbe()
    mockedSidechainNodeViewHolder.setAutoPilot((sender: ActorRef, msg: Any) => {
      msg match {
        case GetDataFromCurrentView(f) =>
          val history: SidechainHistory = mock[SidechainHistory]
          when(history.getMainchainBlockReferenceByHash(ArgumentMatchers.any[Array[Byte]]()))
            .thenAnswer(_ => Some(mockedMcBlockWithScCreation(expectedSysDataConstantOpt)).asJava)
          sender ! f(CurrentView(history, mock[SidechainState], mock[SidechainWallet], mock[SidechainMemoryPool]))
      }
      TestActor.KeepRunning
    })
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref

    val provingKeyPath: String = ""
    val params: NetworkParams = RegTestParams(
      calculatedSysDataConstant = calculatedSysDataConstant,
      certProvingKeyFilePath = provingKeyPath)

    val mainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]

    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolderRef, mock[SecureEnclaveApiClient], params, mainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))
    actorSystem.eventStream.subscribe(certificateSubmitterRef, SidechainAppEvents.SidechainApplicationStart.getClass)

    actorSystem.eventStream.publish(SidechainAppEvents.SidechainApplicationStart)

    val deathWatch = TestProbe()
    deathWatch.watch(certificateSubmitterRef)
    deathWatch.expectTerminated(certificateSubmitterRef, timeout.duration * 2)
  }

  @Test
  def initializationFailure_EmptyProvingKeyFilePath(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)

    val calculatedSysDataConstant = new Array[Byte](32)
    Random.nextBytes(calculatedSysDataConstant)
    val expectedSysDataConstantOpt: Option[Array[Byte]] = Some(calculatedSysDataConstant.clone())

    val mockedSidechainNodeViewHolder = TestProbe()
    mockedSidechainNodeViewHolder.setAutoPilot((sender: ActorRef, msg: Any) => {
      msg match {
        case GetDataFromCurrentView(f) =>
          val history: SidechainHistory = mock[SidechainHistory]
          when(history.getMainchainBlockReferenceByHash(ArgumentMatchers.any[Array[Byte]]()))
            .thenAnswer(_ => Some(mockedMcBlockWithScCreation(expectedSysDataConstantOpt)).asJava)
          sender ! f(CurrentView(history, mock[SidechainState], mock[SidechainWallet], mock[SidechainMemoryPool]))
      }
      TestActor.KeepRunning
    })
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref

    // Test should fail because of empty path.
    val provingKeyPath: String = ""
    val params: NetworkParams = RegTestParams(
      calculatedSysDataConstant = calculatedSysDataConstant,
      certProvingKeyFilePath = provingKeyPath)

    val mainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]

    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolderRef, mock[SecureEnclaveApiClient], params, mainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))
    actorSystem.eventStream.subscribe(certificateSubmitterRef, SidechainAppEvents.SidechainApplicationStart.getClass)

    actorSystem.eventStream.publish(SidechainAppEvents.SidechainApplicationStart)

    val deathWatch = TestProbe()
    deathWatch.watch(certificateSubmitterRef)
    deathWatch.expectTerminated(certificateSubmitterRef, timeout.duration * 2)
  }

  @Test
  def initializationFailure_InvalidProvingKeyFilePath(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)

    val calculatedSysDataConstant = new Array[Byte](32)
    Random.nextBytes(calculatedSysDataConstant)
    val expectedSysDataConstantOpt: Option[Array[Byte]] = Some(calculatedSysDataConstant.clone())

    val mockedSidechainNodeViewHolder = TestProbe()
    mockedSidechainNodeViewHolder.setAutoPilot((sender: ActorRef, msg: Any) => {
      msg match {
        case GetDataFromCurrentView(f) =>
          val history: SidechainHistory = mock[SidechainHistory]
          when(history.getMainchainBlockReferenceByHash(ArgumentMatchers.any[Array[Byte]]()))
            .thenAnswer(_ => Some(mockedMcBlockWithScCreation(expectedSysDataConstantOpt)).asJava)
          sender ! f(CurrentView(history, mock[SidechainState], mock[SidechainWallet], mock[SidechainMemoryPool]))
      }
      TestActor.KeepRunning
    })
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref

    // Test should fail because of empty path.
    val provingKeyPath: String = "wrong_file_path"
    val params: NetworkParams = RegTestParams(
      calculatedSysDataConstant = calculatedSysDataConstant,
      certProvingKeyFilePath = provingKeyPath)

    val mainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]

    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolderRef, mock[SecureEnclaveApiClient], params, mainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))
    actorSystem.eventStream.subscribe(certificateSubmitterRef, SidechainAppEvents.SidechainApplicationStart.getClass)

    actorSystem.eventStream.publish(SidechainAppEvents.SidechainApplicationStart)

    val deathWatch = TestProbe()
    deathWatch.watch(certificateSubmitterRef)
    deathWatch.expectTerminated(certificateSubmitterRef, timeout.duration * 2)
  }

  @Test
  def initialization(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)

    val calculatedSysDataConstant = new Array[Byte](32)
    Random.nextBytes(calculatedSysDataConstant)
    val expectedSysDataConstantOpt: Option[Array[Byte]] = Some(calculatedSysDataConstant.clone())

    val mockedSidechainNodeViewHolder = TestProbe()
    mockedSidechainNodeViewHolder.setAutoPilot((sender: ActorRef, msg: Any) => {
      msg match {
        case GetDataFromCurrentView(f) =>
          val history: SidechainHistory = mock[SidechainHistory]
          when(history.getMainchainBlockReferenceByHash(ArgumentMatchers.any[Array[Byte]]()))
            .thenAnswer(_ => Some(mockedMcBlockWithScCreation(expectedSysDataConstantOpt)).asJava)
          sender ! f(CurrentView(history, mock[SidechainState], mock[SidechainWallet], mock[SidechainMemoryPool]))
      }
      TestActor.KeepRunning
    })
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref

    //
    // Test should fail because of empty path.
    val provingKeyPath: String = getClass.getClassLoader.getResource("mcblock473173_mainnet").getFile
    val params: NetworkParams = RegTestParams(
      calculatedSysDataConstant = calculatedSysDataConstant,
      certProvingKeyFilePath = provingKeyPath)

    val mainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]

    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolderRef, mock[SecureEnclaveApiClient], params, mainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))
    actorSystem.eventStream.subscribe(certificateSubmitterRef, SidechainAppEvents.SidechainApplicationStart.getClass)

    actorSystem.eventStream.publish(SidechainAppEvents.SidechainApplicationStart)

    val watch = TestProbe()
    watch.watch(certificateSubmitterRef)
    watch.expectNoMessage(timeout.duration)

    // check if actor is Alive and switched the behaviour to working cycle
    try {
      val certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
      assertFalse("Actor expected not submitting at the moment", certState)
    } catch {
      case _ : TimeoutException => Assert.fail("Actor expected to be initialized and switched to working cycle")
    }
  }

  @Test
  def certificateSubmissionEvents(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)

    val mockedSidechainNodeViewHolder = TestProbe()
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref

    val mainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]
    val params: NetworkParams = mock[NetworkParams]
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]
    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolderRef, mock[SecureEnclaveApiClient], mock[NetworkParams], mainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))

    val submitter: CertificateSubmitter[CertificateDataWithoutKeyRotation] = certificateSubmitterRef.underlyingActor

    // Skip initialization
    submitter.context.become(submitter.workingCycle)

    // Test: check current submission state
    var certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
    assertFalse("Certificate generation expected to be disabled", certState)

    // Update the State as Started and check
    actorSystem.eventStream.publish(CertificateSubmissionStarted)
    certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
    assertTrue("Certificate generation expected to be enabled", certState)

    // Update the State as Stopped and check
    actorSystem.eventStream.publish(CertificateSubmissionStopped)
    certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
    assertFalse("Certificate generation expected to be enabled", certState)
  }

  @Test
  def getSignaturesStatus(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)

    val mockedSidechainNodeViewHolder = TestProbe()
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref

    val mainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]
    val params: NetworkParams = mock[NetworkParams]
    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolderRef, mock[SecureEnclaveApiClient], mock[NetworkParams], mainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))

    val submitter: CertificateSubmitter[CertificateDataWithoutKeyRotation] = certificateSubmitterRef.underlyingActor

    // Skip initialization
    submitter.context.become(submitter.workingCycle)


    // Test 1: get signatures status outside the Submission Window
    var statusOpt = Await.result(certificateSubmitterRef ? GetSignaturesStatus, timeout.duration).asInstanceOf[Option[SignaturesStatus]]
    assertTrue("Status expected to be None", statusOpt.isEmpty)


    // Test 2: get signatures status inside the Submission Window
    val referencedEpochNumber = 20
    val messageToSign = FieldElementFixture.generateFieldElement()
    val knownSigs = ArrayBuffer[CertificateSignatureInfo]()

    val schnorrSecret = SchnorrKeyGenerator.getInstance().generateSecret("seeeeed".getBytes(StandardCharsets.UTF_8))
    knownSigs.append(CertificateSignatureInfo(0, schnorrSecret.sign(messageToSign)))

    submitter.signaturesStatus = Some(SignaturesStatus(referencedEpochNumber, messageToSign, knownSigs, params.signersPublicKeys))

    statusOpt = Await.result(certificateSubmitterRef ? GetSignaturesStatus, timeout.duration).asInstanceOf[Option[SignaturesStatus]]
    assertTrue("Status expected to be defined", statusOpt.isDefined)

    val status = statusOpt.get
    assertEquals("Referenced epoch number is different.", referencedEpochNumber, status.referencedEpoch)
    assertArrayEquals("Message to sign is different.", messageToSign, status.messageToSign)
    assertEquals("Known sigs array is different.", knownSigs, status.knownSigs)
    assertEquals("Public keys are different.", params.signersPublicKeys, status.signersPublicKeys)
  }
  @Test
  def newBlockArrived(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)

    // Set 3 keys for the Certificate signatures
    val keyGenerator = SchnorrKeyGenerator.getInstance()
    val schnorrSecrets: Seq[SchnorrSecret] = Seq(
      keyGenerator.generateSecret("seed1".getBytes(StandardCharsets.UTF_8)),
      keyGenerator.generateSecret("seed2".getBytes(StandardCharsets.UTF_8)),
      keyGenerator.generateSecret("seed3".getBytes(StandardCharsets.UTF_8))
    )

    val signersThreshold = 2
    val params: RegTestParams = RegTestParams(
      signersPublicKeys = schnorrSecrets.map(_.publicImage()),
      signersThreshold = signersThreshold
    )

    ConsensusParamsUtil.setConsensusParamsForkActivation(Seq(
      ConsensusParamsForkInfo(0, ConsensusParamsFork.DefaultConsensusParamsFork)
    ))
    ConsensusParamsUtil.setConsensusParamsForkTimestampActivation(Seq(TimeToEpochUtils.virtualGenesisBlockTimeStamp(params.sidechainGenesisBlockTimestamp)))


    val mockedMainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]

    val history: SidechainHistory = mock[SidechainHistory]
    val storage: SidechainHistoryStorage = mock[SidechainHistoryStorage]
    val state: SidechainState = mock[SidechainState]
    val wallet: SidechainWallet = mock[SidechainWallet]

    val mockedSidechainNodeViewHolder = TestProbe()
    mockedSidechainNodeViewHolder.setAutoPilot((sender: ActorRef, msg: Any) => {
      msg match {
        case GetDataFromCurrentView(f) =>
          sender ! f(CurrentView(history, state, wallet, mock[SidechainMemoryPool]))
      }
      TestActor.KeepRunning
    })
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref

    // TODO: add cases for NonCeasingStrategy
    val mainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]
    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val submissionStrategy: CertificateSubmissionStrategy = new CeasingSidechain(mockedMainchainChannel, params)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolderRef, mock[SecureEnclaveApiClient], params, mainchainChannel, submissionStrategy, keyRotationStrategy)))

    val submitter: CertificateSubmitter[CertificateDataWithoutKeyRotation] = certificateSubmitterRef.underlyingActor

    val watch = TestProbe()
    watch.watch(certificateSubmitterRef)

    val broadcastSignatureEventListener = TestProbe()

    actorSystem.eventStream.subscribe(broadcastSignatureEventListener.ref, classOf[BroadcastLocallyGeneratedSignature])

    // Skip initialization
    submitter.context.become(submitter.workingCycle)


    // Test 1: block outside the epoch, no timer activated
    val epochNumber = 10
    val referencedEpochNumber = epochNumber - 1
    val epochInfoOutsideWindow = WithdrawalEpochInfo(epochNumber, lastEpochIndex = params.withdrawalEpochLength)
    when(history.blockInfoById(ArgumentMatchers.any[ModifierId])).thenAnswer(_ => {
      val blockInfo: SidechainBlockInfo = mock[SidechainBlockInfo]
      when(blockInfo.withdrawalEpochInfo).thenAnswer(_ => epochInfoOutsideWindow)
      blockInfo
    })

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    assertEquals("Signature status expected to be not defined.", None, submitter.signaturesStatus)
    assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))

    // Test 2: block inside the epoch first time with 1 Sig out of 2
    val epochInfoInsideWindow = WithdrawalEpochInfo(epochNumber, lastEpochIndex = 0)
    reset(state)
    when(state.backwardTransfers(ArgumentMatchers.any[Int])).thenAnswer(answer => {
      assertEquals("Invalid referenced epoch number retrieved for state.backwardTransfers.", referencedEpochNumber, answer.getArgument(0).asInstanceOf[Int])
      Seq()
    })
    when(state.utxoMerkleTreeRoot(ArgumentMatchers.any[Int])).thenAnswer(answer => {
      assertEquals("Invalid referenced epoch number retrieved for state.utxoMerkleTreeRoot.", referencedEpochNumber, answer.getArgument(0).asInstanceOf[Int])
     Some(BytesUtils.fromHexString("0000000000000000000000000000000000000000000000000000000000000000"))
    })
    var walletSecrets: Seq[SchnorrSecret] = schnorrSecrets.take(1)
    when(state.certifiersKeys(ArgumentMatchers.any[Int])).thenAnswer(answer => {
      assert(answer.getArgument(0).asInstanceOf[Int] >= 0)
      val sp: Vector[SchnorrProposition] = walletSecrets.map(ws => new SchnorrProposition(ws.getPublicBytes)).toVector
      Option[CertifiersKeys](CertifiersKeys(sp, Vector[SchnorrProposition]()))
    })

    reset(history)
    when(history.storage).thenAnswer { _ =>
      val blockInfoMock = mock[SidechainBlockInfo]
      when(storage.blockInfoById(any())).thenReturn(blockInfoMock)
      when(blockInfoMock.timestamp).thenReturn(params.sidechainGenesisBlockTimestamp * 2)
      storage
    }
    when(history.blockInfoById(ArgumentMatchers.any[ModifierId])).thenAnswer(_ => {
      val blockInfo: SidechainBlockInfo = mock[SidechainBlockInfo]
      when(blockInfo.withdrawalEpochInfo).thenAnswer(_ => epochInfoInsideWindow)
      when(blockInfo.timestamp).thenReturn(params.sidechainGenesisBlockTimestamp * 2)
      blockInfo
    })
    when(history.getMainchainBlockReferenceInfoByMainchainBlockHeight(ArgumentMatchers.any[Int])).thenAnswer(_ => {
      val randomArray: Array[Byte] = new Array[Byte](CommonParams.mainchainBlockHashLength)
      val info: MainchainBlockReferenceInfo = new MainchainBlockReferenceInfo(randomArray, randomArray, 0, randomArray, randomArray)
      Some(info).asJava
    })
    when(history.mainchainHeaderInfoByHash(ArgumentMatchers.any[Array[Byte]])).thenAnswer(_ => {
      val info: MainchainHeaderInfo = mock[MainchainHeaderInfo]
      when(info.cumulativeCommTreeHash).thenReturn(new Array[Byte](32))
      when(info.sidechainBlockId).thenReturn(ModifierId @@ "some_block_id")
      Some(info)
    })
    val secureEnclaveManagedSecret = schnorrSecrets.take(1)
    val mockedSecureEnclaveApiClient = mock[SecureEnclaveApiClient]
    when(mockedSecureEnclaveApiClient.isEnabled).thenReturn(true)
    when(mockedSecureEnclaveApiClient.listPublicKeys()).thenReturn(Future(secureEnclaveManagedSecret.map(_.publicImage)))
    when(mockedSecureEnclaveApiClient.signWithEnclave(any(), any()))
      .thenAnswer { request =>
        val message = request.getArgument(0).asInstanceOf[Array[Byte]]
        val pk_index = request.getArgument(1).asInstanceOf[(SchnorrProposition, Int)]
          Future.successful(Some(CertificateSignatureInfo(pk_index._2,secureEnclaveManagedSecret.head.sign(message))))
      }

    when(wallet.secret(ArgumentMatchers.any[Proposition])).thenAnswer(answer => {
      val pubKey = answer.getArgument(0).asInstanceOf[SchnorrProposition]
      walletSecrets.find(s => s.owns(pubKey))
    })

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    assertTrue("Signature status expected to be defined.", submitter.signaturesStatus.isDefined)
    assertEquals("Different referenced epoch expected.", referencedEpochNumber, submitter.signaturesStatus.get.referencedEpoch)
    assertEquals("Different signatures number expected.", 1, submitter.signaturesStatus.get.knownSigs.size)
    assertTrue("MessageToSign should be defined.", submitter.signaturesStatus.get.messageToSign.nonEmpty)
    assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))
    val sigInfo = submitter.signaturesStatus.get.knownSigs.head
    assertTrue("Signature expected to be valid", sigInfo.signature.isValid(secureEnclaveManagedSecret.head.publicImage(), submitter.signaturesStatus.get.messageToSign))

    // Verify BroadcastLocallyGeneratedSignature event
    broadcastSignatureEventListener.fishForMessage(timeout.duration) { case m =>
      m match {
        case BroadcastLocallyGeneratedSignature(info) =>
          assertArrayEquals("Invalid Broadcast signature event data: messageToSign.",
            submitter.signaturesStatus.get.messageToSign, info.messageToSign)
          assertEquals("Invalid Broadcast signature event data: pubKeyIndex.",
            submitter.signaturesStatus.get.knownSigs.head.pubKeyIndex, info.pubKeyIndex)
          assertEquals("Invalid Broadcast signature event data: signature.",
            submitter.signaturesStatus.get.knownSigs.head.signature, info.signature)
          true
        case _ => false
      }
    }

    when(mockedSecureEnclaveApiClient.isEnabled).thenReturn(false)
    // Test 3: another block inside the window, check that no signatures were generated.

    // Add one more known key to the wallet
    walletSecrets = schnorrSecrets.take(2)

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    // Data expected to be the same
    assertTrue("Signature status expected to be defined.", submitter.signaturesStatus.isDefined)
    assertEquals("Different referenced epoch expected.", referencedEpochNumber, submitter.signaturesStatus.get.referencedEpoch)
    assertEquals("Different signatures number expected.", 1, submitter.signaturesStatus.get.knownSigs.size)
    assertTrue("MessageToSign should be defined.", submitter.signaturesStatus.get.messageToSign.nonEmpty)
    assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))


    // Test 4: reset SignatureStatus and test block inside the window with a disabled certificate signer
    submitter.signaturesStatus = None
    submitter.certificateSigningEnabled = false

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    // Data expected to be the same
    assertTrue("Signature status expected to be defined.", submitter.signaturesStatus.isDefined)
    assertEquals("Different referenced epoch expected.", referencedEpochNumber, submitter.signaturesStatus.get.referencedEpoch)
    assertEquals("Different signatures number expected.", 0, submitter.signaturesStatus.get.knownSigs.size)
    assertTrue("MessageToSign should be defined.", submitter.signaturesStatus.get.messageToSign.nonEmpty)
    assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))

    submitter.certificateSigningEnabled = true


    // Test 5: reset SignatureStatus and test block inside the window that will lead to generating 2 sigs which is exactly the threshold.
    // But in MC the better quality Cert exists -> no cert scheduling
    submitter.signaturesStatus = None

    val mcTopCertQuality: Long = 3
    when(mockedMainchainChannel.getTopQualityCertificates(ArgumentMatchers.any[String]())).thenAnswer(_ => Try {
      // Cert with mcTopCertQuality presents in the MC
      TopQualityCertificates(
        Some(MempoolTopQualityCertificateInfo("", referencedEpochNumber, mcTopCertQuality, 0.0)),
        None
      )
    })

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    assertTrue("Signature status expected to be defined.", submitter.signaturesStatus.isDefined)
    assertEquals("Different referenced epoch expected.", referencedEpochNumber, submitter.signaturesStatus.get.referencedEpoch)
    assertEquals("Different signatures number expected.", walletSecrets.size, submitter.signaturesStatus.get.knownSigs.size)
    assertTrue("MessageToSign should be defined.", submitter.signaturesStatus.get.messageToSign.nonEmpty)
    assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))


    // Verify BroadcastLocallyGeneratedSignature events
    // First sig event
    broadcastSignatureEventListener.fishForMessage(timeout.duration) { case m =>
      m match {
        case BroadcastLocallyGeneratedSignature(info) =>
          assertArrayEquals("Invalid Broadcast signature event data: messageToSign.",
            submitter.signaturesStatus.get.messageToSign, info.messageToSign)
          assertEquals("Invalid Broadcast signature event data: pubKeyIndex.",
            submitter.signaturesStatus.get.knownSigs.head.pubKeyIndex, info.pubKeyIndex)
          assertEquals("Invalid Broadcast signature event data: signature.",
            submitter.signaturesStatus.get.knownSigs.head.signature, info.signature)
          true
        case _ => false
      }
    }
    // Second sig event
    broadcastSignatureEventListener.fishForMessage(timeout.duration) { case m =>
      m match {
        case BroadcastLocallyGeneratedSignature(info) =>
          assertArrayEquals("Invalid Broadcast signature event data: messageToSign.",
            submitter.signaturesStatus.get.messageToSign, info.messageToSign)
          assertEquals("Invalid Broadcast signature event data: pubKeyIndex.",
            submitter.signaturesStatus.get.knownSigs(1).pubKeyIndex, info.pubKeyIndex)
          assertEquals("Invalid Broadcast signature event data: signature.",
            submitter.signaturesStatus.get.knownSigs(1).signature, info.signature)
          true
        case _ => false
      }
    }
    var certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
    assertFalse("Actor expected not submitting at the moment", certState)


    // Test 6: reset SignatureStatus and test block inside the window that will lead to generating 2 sigs which is exactly the threshold.
    // Certificate submitter is disabled
    submitter.signaturesStatus = None
    submitter.submitterEnabled = false

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    assertTrue("Signature status expected to be defined.", submitter.signaturesStatus.isDefined)
    assertEquals("Different referenced epoch expected.", referencedEpochNumber, submitter.signaturesStatus.get.referencedEpoch)
    assertEquals("Different signatures number expected.", walletSecrets.size, submitter.signaturesStatus.get.knownSigs.size)
    assertTrue("MessageToSign should be defined.", submitter.signaturesStatus.get.messageToSign.nonEmpty)
    assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))

    certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
    assertFalse("Actor expected being NOT submitting at the moment.", certState)

    submitter.submitterEnabled = true

    // Test 7: reset SignatureStatus and test block inside the window that will lead to generating 2 sigs which is exactly the threshold.
    // No better cert quality found
    submitter.signaturesStatus = None

    when(mockedMainchainChannel.getTopQualityCertificates(ArgumentMatchers.any[String]())).thenAnswer(_ => Try {
      // No certs in MC
      TopQualityCertificates(None, None)
    })

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    assertTrue("Signature status expected to be defined.", submitter.signaturesStatus.isDefined)
    assertEquals("Different referenced epoch expected.", referencedEpochNumber, submitter.signaturesStatus.get.referencedEpoch)
    assertEquals("Different signatures number expected.", walletSecrets.size, submitter.signaturesStatus.get.knownSigs.size)
    assertTrue("MessageToSign should be defined.", submitter.signaturesStatus.get.messageToSign.nonEmpty)
    assertTrue("Certificate generation schedule expected to be enabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))

    certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
    assertTrue("Actor expected being submitting at the moment.", certState)
    // Stop events
    submitter.timers.cancelAll()
    submitter.certGenerationState = false


    // Test 8: reset SignatureStatus and test block inside the window that will lead to generating 2 sigs which is exactly the threshold.
    // Get quality exception occurred

    // 8.1 Get quality failed with the MC server internal error -> we expect to continue the flow, so to schedule the generation
    submitter.signaturesStatus = None
    when(mockedMainchainChannel.getTopQualityCertificates(ArgumentMatchers.any[String]())).thenAnswer(_ => Try {
      throw new WebsocketErrorResponseException("ERROR")
    })

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    assertTrue("Signature status expected to be defined.", submitter.signaturesStatus.isDefined)
    assertEquals("Different referenced epoch expected.", referencedEpochNumber, submitter.signaturesStatus.get.referencedEpoch)
    assertEquals("Different signatures number expected.", walletSecrets.size, submitter.signaturesStatus.get.knownSigs.size)
    assertTrue("MessageToSign should be defined.", submitter.signaturesStatus.get.messageToSign.nonEmpty)
    assertTrue("Certificate generation schedule expected to be enabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))

    certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
    assertTrue("Actor expected being submitting at the moment.", certState)
    // Stop events
    submitter.timers.cancelAll()
    submitter.certGenerationState = false

    // 8.2 Get quality failed with the MC server inconsistent error message -> we expect to continue the flow, so to schedule the generation
    submitter.signaturesStatus = None
    reset(mockedMainchainChannel)
    when(mockedMainchainChannel.getTopQualityCertificates(ArgumentMatchers.any[String]())).thenAnswer(_ => Try {
      throw new WebsocketInvalidErrorMessageException("Inconsistent error message")
    })

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    assertTrue("Signature status expected to be defined.", submitter.signaturesStatus.isDefined)
    assertEquals("Different referenced epoch expected.", referencedEpochNumber, submitter.signaturesStatus.get.referencedEpoch)
    assertEquals("Different signatures number expected.", walletSecrets.size, submitter.signaturesStatus.get.knownSigs.size)
    assertTrue("MessageToSign should be defined.", submitter.signaturesStatus.get.messageToSign.nonEmpty)
    assertTrue("Certificate generation schedule expected to be enabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))

    certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
    assertTrue("Actor expected being submitting at the moment.", certState)
    // Stop events
    submitter.timers.cancelAll()
    submitter.certGenerationState = false

    // 8.3 Get quality failed with any other error (connection/network error, for example) -> no cert generation expected
    submitter.signaturesStatus = None
    reset(mockedMainchainChannel)
    when(mockedMainchainChannel.getTopQualityCertificates(ArgumentMatchers.any[String]())).thenAnswer(_ => Try {
      throw new RuntimeException("other exception")
    })

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    assertTrue("Signature status expected to be defined.", submitter.signaturesStatus.isDefined)
    assertEquals("Different referenced epoch expected.", referencedEpochNumber, submitter.signaturesStatus.get.referencedEpoch)
    assertEquals("Different signatures number expected.", walletSecrets.size, submitter.signaturesStatus.get.knownSigs.size)
    assertTrue("MessageToSign should be defined.", submitter.signaturesStatus.get.messageToSign.nonEmpty)

    assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))
    certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
    assertFalse("Actor expected not submitting at the moment.", certState)


    // Test 9: block outside the epoch when the cert submission is scheduled
    reset(history)
    when(history.blockInfoById(ArgumentMatchers.any[ModifierId])).thenAnswer(_ => {
      val blockInfo: SidechainBlockInfo = mock[SidechainBlockInfo]
      when(blockInfo.withdrawalEpochInfo).thenAnswer(_ => epochInfoOutsideWindow)
      blockInfo
    })

    actorSystem.eventStream.publish(SemanticallySuccessfulModifier(mock[SidechainBlock]))
    watch.expectNoMessage(timeout.duration)

    assertEquals("Signature status expected to be not defined.", None, submitter.signaturesStatus)
    assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))
    certState = Await.result(certificateSubmitterRef ? GetCertificateGenerationState, timeout.duration).asInstanceOf[Boolean]
    assertFalse("Actor expected not submitting at the moment.", certState)
  }

  @Test
  def getFtMinAmount(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)
    val dustThreshold = ZenCoinsUtils.getMinDustThreshold(ZenCoinsUtils.MC_DEFAULT_FEE_RATE)
    val params: NetworkParams = mock[NetworkParams]
    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]

    val submitter: CertificateSubmitter[CertificateDataWithoutKeyRotation] = TestActorRef(Props(
      new CertificateSubmitter(mockedSettings, mock[ActorRef], mock[SecureEnclaveApiClient], mock[NetworkParams], mock[MainchainNodeChannel], mockedSubmissionStrategy, keyRotationStrategy)
    )).underlyingActor

    assertEquals("Before the fork, ftMinAmount should be 0",
      0, submitter.getFtMinAmount(consensusEpochAtWhichForkIsApplied - 1))
    assertEquals(s"After the fork, ftMinAmount should be equal to dust threshold [$dustThreshold]",
      dustThreshold, submitter.getFtMinAmount(consensusEpochAtWhichForkIsApplied))
    assertEquals(s"After the fork, ftMinAmount should be equal to dust threshold [$dustThreshold]",
      dustThreshold, submitter.getFtMinAmount(consensusEpochAtWhichForkIsApplied + 1))
  }

  @Test
  def signatureFromRemote(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)

    // Set 3 keys for the Certificate signatures
    val keyGenerator = SchnorrKeyGenerator.getInstance()
    val schnorrSecrets: Seq[SchnorrSecret] = Seq(
      keyGenerator.generateSecret("seed1".getBytes(StandardCharsets.UTF_8)),
      keyGenerator.generateSecret("seed2".getBytes(StandardCharsets.UTF_8)),
      keyGenerator.generateSecret("seed3".getBytes(StandardCharsets.UTF_8))
    )

    val signersThreshold = 2
    val params: RegTestParams = RegTestParams(
      signersPublicKeys = schnorrSecrets.map(_.publicImage()),
      signersThreshold = signersThreshold
    )

    val mockedMainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]

    val mockedSidechainNodeViewHolder = TestProbe()
    mockedSidechainNodeViewHolder.setAutoPilot((sender: ActorRef, msg: Any) => {
      msg match {
        case GetDataFromCurrentView(f) =>
          sender ! f(CurrentView(mock[SidechainHistory], mock[SidechainState], mock[SidechainWallet], mock[SidechainMemoryPool]))
      }
      TestActor.KeepRunning
    })
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref

    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]

    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolderRef, mock[SecureEnclaveApiClient], params, mockedMainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))

    val submitter: CertificateSubmitter[CertificateDataWithoutKeyRotation] = certificateSubmitterRef.underlyingActor

    // Skip initialization
    submitter.context.become(submitter.workingCycle)

    assertTrue("Signatures Status object expected to be undefined.", submitter.signaturesStatus.isEmpty)


    // Test 1: Retrieve signature from remote when not inside the Submission Window
    val messageToSign = FieldElementFixture.generateFieldElement()
    val signature = schnorrSecrets.head.sign(messageToSign)

    val remoteSignInfo = CertificateSignatureFromRemoteInfo(0, messageToSign, signature)

    try {
      val res = Await.result(certificateSubmitterRef ? SignatureFromRemote(remoteSignInfo), timeout.duration).asInstanceOf[SignatureProcessingStatus]
      assertEquals("Different remote signature processing result expected.", SubmitterIsOutsideSubmissionWindow, res)
    } catch {
      case _ : TimeoutException => Assert.fail("Response expected for the signature from remote request.")
    }


    // Test 2: Retrieve signature from remote with different message to sign when inside the Submission Window
    // Emulate in window status
    val referencedEpochNumber = 10
    submitter.signaturesStatus = Some(SignaturesStatus(referencedEpochNumber, messageToSign, ArrayBuffer(), params.signersPublicKeys))

    val anotherMessageToSign = FieldElementFixture.generateFieldElement()
    val anotherSignature = schnorrSecrets.head.sign(anotherMessageToSign)
    val remoteSignInfoWithDiffMessage = CertificateSignatureFromRemoteInfo(0, anotherMessageToSign, anotherSignature)

    try {
      val res = Await.result(certificateSubmitterRef ? SignatureFromRemote(remoteSignInfoWithDiffMessage), timeout.duration).asInstanceOf[SignatureProcessingStatus]
      assertEquals("Different remote signature processing result expected.", DifferentMessageToSign, res)
    } catch {
      case _ : TimeoutException => Assert.fail("Response expected for the signature from remote request.")
    }


    // Test 3: Retrieve signature from remote with invalid pub key index (out of range) when inside the Submission Window
    val invalidPubKeyIndex = params.signersPublicKeys.size
    val remoteSignInfoWithInvalidIndex = CertificateSignatureFromRemoteInfo(invalidPubKeyIndex, messageToSign, signature)

    try {
      val res = Await.result(certificateSubmitterRef ? SignatureFromRemote(remoteSignInfoWithInvalidIndex), timeout.duration).asInstanceOf[SignatureProcessingStatus]
      assertEquals("Different remote signature processing result expected.", InvalidPublicKeyIndex, res)
    } catch {
      case _ : TimeoutException => Assert.fail("Response expected for the signature from remote request.")
    }


    // Test 4: Retrieve signature from remote with invalid signature when inside the Submission Window
    val remoteSignInfoWithInvalidSignature = CertificateSignatureFromRemoteInfo(0, messageToSign, anotherSignature)

    try {
      val res = Await.result(certificateSubmitterRef ? SignatureFromRemote(remoteSignInfoWithInvalidSignature), timeout.duration).asInstanceOf[SignatureProcessingStatus]
      assertEquals("Different remote signature processing result expected.", InvalidSignature, res)
    } catch {
      case _ : TimeoutException => Assert.fail("Response expected for the signature from remote request.")
    }


    // Test 5: Retrieve valid UNIQUE signature from remote when inside the Submission Window
    assertEquals("Different signatures number expected.", 0, submitter.signaturesStatus.get.knownSigs.size)
    try {
      val res = Await.result(certificateSubmitterRef ? SignatureFromRemote(remoteSignInfo), timeout.duration).asInstanceOf[SignatureProcessingStatus]
      assertEquals("Different remote signature processing result expected.", ValidSignature, res)

      assertEquals("Different signatures number expected.", 1, submitter.signaturesStatus.get.knownSigs.size)
      assertEquals("Inconsistent remote signature info stored data: pubKeyIndex.",
        remoteSignInfo.pubKeyIndex, submitter.signaturesStatus.get.knownSigs.head.pubKeyIndex)
      assertEquals("Inconsistent remote signature info stored data: signature.",
        remoteSignInfo.signature, submitter.signaturesStatus.get.knownSigs.head.signature)
      assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))

    } catch {
      case _ : TimeoutException => Assert.fail("Response expected for the signature from remote request.")
    }


    // Test 6: Retrieve valid ALREADY PRESENT signature from remote when inside the Submission Window
    try {
      val res = Await.result(certificateSubmitterRef ? SignatureFromRemote(remoteSignInfo), timeout.duration).asInstanceOf[SignatureProcessingStatus]
      assertEquals("Different remote signature processing result expected.", KnownSignature, res)

      assertEquals("Different signatures number expected.", 1, submitter.signaturesStatus.get.knownSigs.size)
      assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))

    } catch {
      case _ : TimeoutException => Assert.fail("Response expected for the signature from remote request.")
    }


    // Test 7: Key rotation keys: retrieve valid signature from remote,
    // when public key in the Status is different to the genesis one (from params).
    val newSecret: SchnorrSecret = keyGenerator.generateSecret("rotated_key_seed".getBytes(StandardCharsets.UTF_8))
    val updatedSignerKeys = newSecret.publicImage() +: params.signersPublicKeys.tail
    submitter.signaturesStatus = Some(SignaturesStatus(referencedEpochNumber, messageToSign, ArrayBuffer(), updatedSignerKeys))

    val updatedKeySignature = newSecret.sign(messageToSign)
    val remoteUpdatedKeySignInfo = CertificateSignatureFromRemoteInfo(0, messageToSign, updatedKeySignature)

    try {
      val res = Await.result(certificateSubmitterRef ? SignatureFromRemote(remoteUpdatedKeySignInfo), timeout.duration).asInstanceOf[SignatureProcessingStatus]
      assertEquals("Different remote signature processing result expected.", ValidSignature, res)

      assertEquals("Different signatures number expected.", 1, submitter.signaturesStatus.get.knownSigs.size)
      assertEquals("Inconsistent remote signature info stored data: pubKeyIndex.",
        remoteUpdatedKeySignInfo.pubKeyIndex, submitter.signaturesStatus.get.knownSigs.head.pubKeyIndex)
      assertEquals("Inconsistent remote signature info stored data: signature.",
        remoteUpdatedKeySignInfo.signature, submitter.signaturesStatus.get.knownSigs.head.signature)
      assertFalse("Certificate generation schedule expected to be disabled.", submitter.timers.isTimerActive(CertificateGenerationTimer))

    } catch {
      case _: TimeoutException => Assert.fail("Response expected for the signature from remote request.")
    }
  }

  @Test
  def tryToSubmitCertificate(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)

    // Set 3 keys for the Certificate signatures
    val keyGenerator = SchnorrKeyGenerator.getInstance()
    val schnorrSecrets: Seq[SchnorrSecret] = Seq(
      keyGenerator.generateSecret("seed1".getBytes(StandardCharsets.UTF_8)),
      keyGenerator.generateSecret("seed2".getBytes(StandardCharsets.UTF_8)),
      keyGenerator.generateSecret("seed3".getBytes(StandardCharsets.UTF_8))
    )

    val signersThreshold = 2
    val params: RegTestParams = RegTestParams(
      signersPublicKeys = schnorrSecrets.map(_.publicImage()),
      signersThreshold = signersThreshold
    )

    val mockedMainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]

    val history: SidechainHistory = mock[SidechainHistory]
    val state: SidechainState = mock[SidechainState]
    val wallet: SidechainWallet = mock[SidechainWallet]

    val mockedSidechainNodeViewHolder = TestProbe()
    mockedSidechainNodeViewHolder.setAutoPilot((sender: ActorRef, msg: Any) => {
      msg match {
        case GetDataFromCurrentView(f) =>
          sender ! f(CurrentView(history, state, wallet, mock[SidechainMemoryPool]))
      }
      TestActor.KeepRunning
    })
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref

    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]

    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolderRef, mock[SecureEnclaveApiClient], params, mockedMainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))

    val submitter: CertificateSubmitter[CertificateDataWithoutKeyRotation] = certificateSubmitterRef.underlyingActor


    val certSubmissionEventListener = TestProbe()
    actorSystem.eventStream.subscribe(certSubmissionEventListener.ref, CertificateSubmissionStopped.getClass)

    // Skip initialization
    submitter.context.become(submitter.workingCycle)


    // Test 1: Try to generate Certificate when there is no Signatures Status defined - should skip
    assertTrue("Signatures Status object expected to be undefined.", submitter.signaturesStatus.isEmpty)
    certificateSubmitterRef ! TryToGenerateCertificate
    certSubmissionEventListener.fishForMessage(timeout.duration) { case m => m == CertificateSubmissionStopped }


    // Test 2: Try to generate Certificate when there is not enough known sigs (< threshold) - should skip
    val referencedEpochNumber = 100
    val messageToSign = FieldElementFixture.generateFieldElement()
    submitter.signaturesStatus = Some(SignaturesStatus(referencedEpochNumber, messageToSign, ArrayBuffer(), params.signersPublicKeys))

    certificateSubmitterRef ! TryToGenerateCertificate
    certSubmissionEventListener.fishForMessage(timeout.duration) { case m => m == CertificateSubmissionStopped }


    // Test 3: Try to generate Certificate when there is enough known sigs (>= threshold),
    // but less than in the top quality cert known by the MC - should skip
    // Note: it may occur, if during the scheduled delay new topQualityCert appeared in the MC

    // Add signatures up to the threshold
    assertTrue(params.signersThreshold < schnorrSecrets.size)
    schnorrSecrets.take(params.signersThreshold).zipWithIndex.foreach{
      case (secret, index) =>
        val signature = secret.sign(messageToSign)
        submitter.signaturesStatus.get.knownSigs.append(CertificateSignatureInfo(index, signature))
    }
    when(mockedMainchainChannel.getTopQualityCertificates(ArgumentMatchers.any[String]())).thenAnswer(_ => Try {
      // In-chain Cert in the MC
      TopQualityCertificates(
        None,
        Some(ChainTopQualityCertificateInfo("", referencedEpochNumber, schnorrSecrets.size))
      )
    })

    certificateSubmitterRef ! TryToGenerateCertificate
    certSubmissionEventListener.fishForMessage(timeout.duration) { case m => m == CertificateSubmissionStopped }
  }

  @Test
  def switchSubmitterStatus(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)
    val params: RegTestParams = RegTestParams()
    val mockedMainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]
    val mockedSidechainNodeViewHolder = TestProbe()
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]

    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolder.ref, mock[SecureEnclaveApiClient], params, mockedMainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))

    val submitter: CertificateSubmitter[CertificateDataWithoutKeyRotation] = certificateSubmitterRef.underlyingActor

    // Skip initialization
    submitter.context.become(submitter.workingCycle)

    // Check initial state
    try {
      val submitterEnabled: Boolean = Await.result(certificateSubmitterRef ? IsSubmitterEnabled, timeout.duration).asInstanceOf[Boolean]
      assertTrue("Submitter expected to be enabled", submitterEnabled)
    } catch {
      case _ : TimeoutException => Assert.fail("Actor expected to be initialized and switched to working cycle")
    }

    // Disable submitter and check
    certificateSubmitterRef ! DisableSubmitter
    try {
      val submitterEnabled: Boolean = Await.result(certificateSubmitterRef ? IsSubmitterEnabled, timeout.duration).asInstanceOf[Boolean]
      assertFalse("Submitter expected to be disabled", submitterEnabled)
    } catch {
      case _ : TimeoutException => Assert.fail("Actor expected to be initialized and switched to working cycle")
    }

    // Enable submitter and check
    certificateSubmitterRef ! EnableSubmitter
    try {
      val submitterEnabled: Boolean = Await.result(certificateSubmitterRef ? IsSubmitterEnabled, timeout.duration).asInstanceOf[Boolean]
      assertTrue("Submitter expected to be enabled", submitterEnabled)
    } catch {
      case _ : TimeoutException => Assert.fail("Actor expected to be initialized and switched to working cycle")
    }
  }

  @Test
  def switchCertificateSigning(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration * 100, submitterIsEnabled = true, signerIsEnabled = true)
    val params: RegTestParams = RegTestParams()
    val mockedMainchainChannel: MainchainNodeChannel = mock[MainchainNodeChannel]
    val mockedSidechainNodeViewHolder = TestProbe()
    val mockedSubmissionStrategy: CertificateSubmissionStrategy = mock[CertificateSubmissionStrategy]

    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings, mockedSidechainNodeViewHolder.ref, mock[SecureEnclaveApiClient], params, mockedMainchainChannel, mockedSubmissionStrategy, keyRotationStrategy)))

    val submitter: CertificateSubmitter[CertificateDataWithoutKeyRotation] = certificateSubmitterRef.underlyingActor

    // Skip initialization
    submitter.context.become(submitter.workingCycle)

    // Check initial state
    try {
      val signingEnabled: Boolean = Await.result(certificateSubmitterRef ? IsCertificateSigningEnabled, timeout.duration).asInstanceOf[Boolean]
      assertTrue("Certificate signing expected to be enabled", signingEnabled)
    } catch {
      case _ : TimeoutException => Assert.fail("Actor expected to be initialized and switched to working cycle")
    }

    // Disable singing and check
    certificateSubmitterRef ! DisableCertificateSigner
    try {
      val signingEnabled: Boolean = Await.result(certificateSubmitterRef ? IsCertificateSigningEnabled, timeout.duration).asInstanceOf[Boolean]
      assertFalse("Certificate signing expected to be disabled", signingEnabled)
    } catch {
      case _ : TimeoutException => Assert.fail("Actor expected to be initialized and switched to working cycle")
    }

    // Enable singing and check
    certificateSubmitterRef ! EnableCertificateSigner
    try {
      val signingEnabled: Boolean = Await.result(certificateSubmitterRef ? IsCertificateSigningEnabled, timeout.duration).asInstanceOf[Boolean]
      assertTrue("Certificate signing expected to be enabled", signingEnabled)
    } catch {
      case _ : TimeoutException => Assert.fail("Actor expected to be initialized and switched to working cycle")
    }
  }

  @Test
  def signaturesFromEnclave(): Unit = {
    val mockedSettings: SidechainSettings = getMockedSettings(timeout.duration, submitterIsEnabled = true, signerIsEnabled = true)
    when(mockedSettings.remoteKeysManagerSettings).thenReturn(RemoteKeysManagerSettings(requestTimeout = 100 milliseconds))
    val mockedSidechainNodeViewHolder = TestProbe()
    val mockedSidechainNodeViewHolderRef: ActorRef = mockedSidechainNodeViewHolder.ref
    val mockedSecureEnclaveApiClient = mock[SecureEnclaveApiClient]

    val params: NetworkParams = mock[NetworkParams]
    val keyRotationStrategy: CircuitStrategy[SidechainTypes#SCBT, SidechainBlockHeader, SidechainBlock, SidechainHistory, SidechainState, _ <: CertificateData] = new WithoutKeyRotationCircuitStrategy(mockedSettings, params, CryptoLibProvider.sigProofThresholdCircuitFunctions)
    val certificateSubmitterRef: TestActorRef[CertificateSubmitter[CertificateDataWithoutKeyRotation]] = TestActorRef(
      Props(new CertificateSubmitter(mockedSettings,
        mockedSidechainNodeViewHolderRef,
        mockedSecureEnclaveApiClient,
        mock[NetworkParams],
        mock[MainchainNodeChannel], mock[CertificateSubmissionStrategy], keyRotationStrategy)))

    val submitter: CertificateSubmitter[CertificateDataWithoutKeyRotation] = certificateSubmitterRef.underlyingActor

    val keyGenerator = SchnorrKeyGenerator.getInstance()
    val indexedPublicKeys: Seq[(SchnorrProposition, Int)] = Seq(
      (keyGenerator.generateSecret("seed1".getBytes(StandardCharsets.UTF_8)).publicImage(), 0),
      (keyGenerator.generateSecret("seed2".getBytes(StandardCharsets.UTF_8)).publicImage(), 2),
      (keyGenerator.generateSecret("seed3".getBytes(StandardCharsets.UTF_8)).publicImage(), 3),
      (keyGenerator.generateSecret("seed4".getBytes(StandardCharsets.UTF_8)).publicImage(), 4),
      (keyGenerator.generateSecret("seed5".getBytes(StandardCharsets.UTF_8)).publicImage(), 5),
    )

    val messageToSign = FieldElementFixture.generateFieldElement()


    //Test 1: secureEnclaveApiClient.isEnabled = false
    when(mockedSecureEnclaveApiClient.isEnabled).thenReturn(false)
    assertEquals("List of signatures is not empty when enclave is disabled", Seq(),
      submitter.signaturesFromEnclave(messageToSign, indexedPublicKeys))

    //Test 2: listPublicKeys() fails
    reset(mockedSecureEnclaveApiClient)
    when(mockedSecureEnclaveApiClient.isEnabled).thenReturn(true)
    when(mockedSecureEnclaveApiClient.listPublicKeys()).thenReturn(Future[Seq[SchnorrProposition]]( throw new Exception()))
    assertEquals("List of signatures is not empty when enclave returns an exception when retrieving keys", Seq(),
      submitter.signaturesFromEnclave(messageToSign, indexedPublicKeys))

    //Test 3: listPublicKeys() time out
    reset(mockedSecureEnclaveApiClient)
    when(mockedSecureEnclaveApiClient.isEnabled).thenReturn(true)
    when(mockedSecureEnclaveApiClient.listPublicKeys()).thenReturn (Future{
        Thread.sleep(1 + mockedSettings.remoteKeysManagerSettings.requestTimeout.length);
        indexedPublicKeys.map(_._1)
      }
    )

    assertEquals("List of signatures is not empty when time out occurs when retrieving keys", Seq(),
      submitter.signaturesFromEnclave(messageToSign, indexedPublicKeys))

    //Test 4: returns list of signatures
    reset(mockedSecureEnclaveApiClient)
    when(mockedSecureEnclaveApiClient.isEnabled).thenReturn(true)
    var remoteKeysList = indexedPublicKeys.map(_._1)
    remoteKeysList = keyGenerator.generateSecret("seed11".getBytes(StandardCharsets.UTF_8)).publicImage() +: remoteKeysList
    when(mockedSecureEnclaveApiClient.listPublicKeys()).thenReturn(Future(remoteKeysList))

    val secureEnclaveManagedSecret = keyGenerator.generateSecret("seed1".getBytes(StandardCharsets.UTF_8))

    when(mockedSecureEnclaveApiClient.signWithEnclave(any(), any()))
      .thenAnswer { request =>
        val message = request.getArgument(0).asInstanceOf[Array[Byte]]
        val pk_index = request.getArgument(1).asInstanceOf[(SchnorrProposition, Int)]
        Future(Some(CertificateSignatureInfo(pk_index._2, secureEnclaveManagedSecret.sign(message))))
      }

    var listOfSignatures = submitter.signaturesFromEnclave(messageToSign, indexedPublicKeys)
    assertEquals("Wrong number of signatures", indexedPublicKeys.size, listOfSignatures.size)
    assertEquals("Wrong pub key index", indexedPublicKeys.head._2, listOfSignatures.head.pubKeyIndex)

    //Test 5: 1 signature fails
    reset(mockedSecureEnclaveApiClient)
    when(mockedSecureEnclaveApiClient.isEnabled).thenReturn(true)
    remoteKeysList = indexedPublicKeys.map(_._1)
    when(mockedSecureEnclaveApiClient.listPublicKeys()).thenReturn (Future(remoteKeysList))

    when(mockedSecureEnclaveApiClient.signWithEnclave(any(), any()))
      .thenAnswer { request =>
        val message = request.getArgument(0).asInstanceOf[Array[Byte]]
        val pk_index = request.getArgument(1).asInstanceOf[(SchnorrProposition, Int)]
        if (pk_index._2 == 5)
          Future(throw new Exception())
        else
          Future(Some(CertificateSignatureInfo(pk_index._2, secureEnclaveManagedSecret.sign(message))))
      }
    listOfSignatures = submitter.signaturesFromEnclave(messageToSign, indexedPublicKeys)
    assertEquals("Wrong number of signatures", 4, listOfSignatures.size)
    assertTrue(listOfSignatures.find(x => x.pubKeyIndex == 5).isEmpty)

    //Test 6: 1 signature time out
    reset(mockedSecureEnclaveApiClient)
    when(mockedSecureEnclaveApiClient.isEnabled).thenReturn(true)
    remoteKeysList = indexedPublicKeys.map(_._1)
    when(mockedSecureEnclaveApiClient.listPublicKeys()).thenReturn(Future(remoteKeysList))


    when(mockedSecureEnclaveApiClient.signWithEnclave(any(), any()))
      .thenAnswer { request =>
        val message = request.getArgument(0).asInstanceOf[Array[Byte]]
        val pk_index = request.getArgument(1).asInstanceOf[(SchnorrProposition, Int)]
        if (pk_index._2 == 0)
          Future {
            Thread.sleep(2 * mockedSettings.remoteKeysManagerSettings.requestTimeout.length);
            Some(CertificateSignatureInfo(pk_index._2, secureEnclaveManagedSecret.sign(message)))
          }
        else
          Future(Some(CertificateSignatureInfo(pk_index._2, secureEnclaveManagedSecret.sign(message))))
      }
    listOfSignatures = submitter.signaturesFromEnclave(messageToSign, indexedPublicKeys)
    assertEquals("Wrong number of signatures", 4, listOfSignatures.size)
    assertTrue(listOfSignatures.find(x => x.pubKeyIndex == 0).isEmpty)

    //Test 7: 1 no signature
    reset(mockedSecureEnclaveApiClient)
    when(mockedSecureEnclaveApiClient.isEnabled).thenReturn(true)
    remoteKeysList = indexedPublicKeys.map(_._1)
    when(mockedSecureEnclaveApiClient.listPublicKeys()).thenReturn(Future(remoteKeysList))

    when(mockedSecureEnclaveApiClient.signWithEnclave(any(), any()))
      .thenAnswer { request =>
        val message = request.getArgument(0).asInstanceOf[Array[Byte]]
        val pk_index = request.getArgument(1).asInstanceOf[(SchnorrProposition, Int)]
        if (pk_index._2 == 4)
          Future(None)
        else
          Future(Some(CertificateSignatureInfo(pk_index._2, secureEnclaveManagedSecret.sign(message))))
      }
    listOfSignatures = submitter.signaturesFromEnclave(messageToSign, indexedPublicKeys)
    assertEquals("Wrong number of signatures", 4, listOfSignatures.size)
    assertTrue(listOfSignatures.find(x => x.pubKeyIndex == 4).isEmpty)

  }


}
