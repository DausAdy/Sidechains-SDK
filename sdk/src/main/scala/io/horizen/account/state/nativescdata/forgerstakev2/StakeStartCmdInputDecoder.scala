package io.horizen.account.state.nativescdata.forgerstakev2

import io.horizen.account.abi.{ABIDecoder, ABIEncodable, MsgProcessorInputDecoder}
import io.horizen.account.state.ForgerPublicKeys
import io.horizen.evm.Address
import io.horizen.proposition.PublicKey25519Proposition
import org.web3j.abi.TypeReference
import org.web3j.abi.datatypes.generated.{Bytes1, Bytes32}
import org.web3j.abi.datatypes.{StaticStruct, Type, Address => AbiAddress}

import java.util

object StakeStartCmdInputDecoder
    extends ABIDecoder[StakeStartCmdInput]
    with MsgProcessorInputDecoder[StakeStartCmdInput]
    with VRFDecoder {

  override val getListOfABIParamTypes: util.List[TypeReference[Type[_]]] =
    org.web3j.abi.Utils.convert(
      util.Arrays.asList(
        new TypeReference[Bytes32]() {},
        new TypeReference[Bytes32]() {},
        new TypeReference[Bytes1]() {},
        new TypeReference[AbiAddress]() {},
      ),
    )

  override def createType(listOfParams: util.List[Type[_]]): StakeStartCmdInput = {
    val forgerPublicKey = new PublicKey25519Proposition(listOfParams.get(0).asInstanceOf[Bytes32].getValue)
    val vrfKey = decodeVrfKey(listOfParams.get(1).asInstanceOf[Bytes32], listOfParams.get(2).asInstanceOf[Bytes1])
    val forgerPublicKeys = ForgerPublicKeys(forgerPublicKey, vrfKey)
    val delegator = new Address(listOfParams.get(3).asInstanceOf[AbiAddress].toString)
    StakeStartCmdInput(forgerPublicKeys, delegator)
  }

}

case class StakeStartCmdInput(forgerPublicKeys: ForgerPublicKeys, delegator: Address)
    extends ABIEncodable[StaticStruct] {

  override def asABIType(): StaticStruct = {
    val forgerPublicKeysAbi = forgerPublicKeys.asABIType()
    val listOfParams: util.List[Type[_]] =
      new util.ArrayList(forgerPublicKeysAbi.getValue.asInstanceOf[util.List[Type[_]]])
    listOfParams.add(new AbiAddress(delegator.toString))
    new StaticStruct(listOfParams)
  }

  override def toString: String =
    "%s(forgerPubKeys: %s, delegator: %s)"
      .format(this.getClass.toString, forgerPublicKeys, delegator)
}
