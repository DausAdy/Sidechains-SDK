package io.horizen.account.state.nativescdata.forgerstakev2

import io.horizen.account.abi.{ABIDecoder, ABIEncodable, MsgProcessorInputDecoder}
import io.horizen.account.state.{AddNewStakeCmdInput, ForgerPublicKeys}
import io.horizen.evm.Address
import io.horizen.proposition.{PublicKey25519Proposition, VrfPublicKey}
import org.web3j.abi.TypeReference
import org.web3j.abi.datatypes.generated.{Bytes1, Bytes32, Uint256, Uint32}
import org.web3j.abi.datatypes.{StaticStruct, Type, Address => AbiAddress}

import java.math.BigInteger
import java.util

object WithdrawCmdInputDecoder
  extends ABIDecoder[WithdrawCmdInput]
    with MsgProcessorInputDecoder[WithdrawCmdInput] {

  override val getListOfABIParamTypes: util.List[TypeReference[Type[_]]] =
    org.web3j.abi.Utils.convert(util.Arrays.asList(
      new TypeReference[Bytes32]() {},
      new TypeReference[Bytes32]() {},
      new TypeReference[Bytes1]() {},
      new TypeReference[Uint256]() {}
      ))

  override def createType(listOfParams: util.List[Type[_]]): WithdrawCmdInput = {
    val forgerPublicKey = new PublicKey25519Proposition(listOfParams.get(0).asInstanceOf[Bytes32].getValue)
    val vrfKey = decodeVrfKey(listOfParams.get(1).asInstanceOf[Bytes32], listOfParams.get(2).asInstanceOf[Bytes1])
    val forgerPublicKeys = ForgerPublicKeys(forgerPublicKey, vrfKey)
    val value: BigInteger = listOfParams.get(3).asInstanceOf[Uint256].getValue
    WithdrawCmdInput(forgerPublicKeys, value)
  }

  private[horizen] def decodeVrfKey(vrfFirst32Bytes: Bytes32, vrfLastByte: Bytes1): VrfPublicKey = {
    val vrfinBytes = vrfFirst32Bytes.getValue ++ vrfLastByte.getValue
    new VrfPublicKey(vrfinBytes)
  }
}

case class WithdrawCmdInput(forgerPublicKeys: ForgerPublicKeys, value: BigInteger) extends ABIEncodable[StaticStruct] {

  override def asABIType(): StaticStruct = {
    val forgerPublicKeysAbi = forgerPublicKeys.asABIType()
    val listOfParams: util.List[Type[_]] = new util.ArrayList(forgerPublicKeysAbi.getValue.asInstanceOf[util.List[Type[_]]])
    listOfParams.add(new Uint256(value))
    new StaticStruct(listOfParams)
  }

  override def toString: String = "%s(forgerPubKeys: %s, value: %s)"
    .format(this.getClass.toString, forgerPublicKeys, value.toString)
}
