#!/usr/bin/env python3
import logging
import pprint
from binascii import a2b_hex, b2a_hex
from decimal import Decimal
from eth_utils import add_0x_prefix, remove_0x_prefix
from SidechainTestFramework.account.ac_chain_setup import AccountChainSetup
from SidechainTestFramework.account.httpCalls.transaction.createRawEIP1559Transaction import createRawEIP1559Transaction
from SidechainTestFramework.account.httpCalls.transaction.createRawLegacyEIP155Transaction import \
    createRawLegacyEIP155Transaction
from SidechainTestFramework.account.httpCalls.transaction.createRawLegacyTransaction import createRawLegacyTransaction
from SidechainTestFramework.account.httpCalls.transaction.decodeTransaction import decodeTransaction
from SidechainTestFramework.account.httpCalls.transaction.sendTransaction import sendTransaction
from SidechainTestFramework.account.httpCalls.transaction.signTransaction import signTransaction
from SidechainTestFramework.scutil import generate_next_block
from httpCalls.block.best import http_block_best
from httpCalls.transaction.allTransactions import allTransactions
from SidechainTestFramework.account.utils import convertZenToWei
from test_framework.util import (assert_equal, assert_true, fail)

"""
Configuration: 
    - 2 SC nodes NOT connected with each other
    - 1 MC node

Test:
    Test ethereum transactions of type legacy/eip155/eip1559 using these steps:
    - create unsigned raw tx
    - decode raw tx
    - sign raw tx
    - send raw tx to the network
    
    Negative tests
    - Try send an tx with invalid signature to mempool
    - Try send an unsigned tx to mempool
    - Try send a tx with bad chainid to mempool
    - Try to forge a block forcing an unsigned tx (see api)
    - Try the same with a tx with bad chainid
     
"""


def b2x(b):
    return b2a_hex(b).decode('ascii')


class SCEvmRawTxHttpApi(AccountChainSetup):
    def __init__(self):
        super().__init__(number_of_sidechain_nodes=2)

    def do_test_raw_tx(self, *, raw_tx, sc_node, evm_signer_address):
        self.sc_sync_all()

        # get mempool contents and check it is empty
        response = allTransactions(sc_node, False)
        assert_equal(0, len(response['transactionIds']))

        signed_raw_tx = signTransaction(sc_node, fromAddress=evm_signer_address, payload=raw_tx)

        tx_json = decodeTransaction(sc_node, payload=signed_raw_tx)
        assert_equal(tx_json['signed'], True)

        tx_hash = sendTransaction(sc_node, payload=signed_raw_tx)
        self.sc_sync_all()

        # get mempool contents and check tx is there
        response = allTransactions(sc_node, False)
        assert_true(tx_hash in response['transactionIds'])

        generate_next_block(sc_node, "first node")
        self.sc_sync_all()

        receipt = sc_node.rpc_eth_getTransactionReceipt(add_0x_prefix(tx_hash))

        status = int(receipt['result']['status'], 16)
        assert_equal(status, 1)



    def run_test(self):
        ft_amount_in_zen = Decimal('500.0')

        self.sc_ac_setup(ft_amount_in_zen=ft_amount_in_zen)

        sc_node_1 = self.sc_nodes[0]
        sc_node_2 = self.sc_nodes[1]

        # transfer some fund from MC to SC1 at a new evm address, then mine mc block
        evm_address_sc1 = remove_0x_prefix(self.evm_address)
        evm_address_sc2 = sc_node_2.wallet_createPrivateKeySecp256k1()["result"]["proposition"]["address"]

        transferred_amount_in_zen = Decimal('1.2')

        raw_tx = createRawLegacyTransaction(sc_node_1,
                                            fromAddress=evm_address_sc1,
                                            toAddress=evm_address_sc2,
                                            value=convertZenToWei(transferred_amount_in_zen))
        tx_json = decodeTransaction(sc_node_2, payload=raw_tx)
        assert_equal(tx_json['legacy'], True)
        assert_equal(tx_json['eip155'], False)
        assert_equal(tx_json['eip1559'], False)
        assert_equal(tx_json['signed'], False)


        self.do_test_raw_tx(
            raw_tx=raw_tx, sc_node=sc_node_1, evm_signer_address=evm_address_sc1)



        raw_tx = createRawLegacyEIP155Transaction(sc_node_1,
                                            fromAddress=evm_address_sc1,
                                            toAddress=evm_address_sc2,
                                            value=convertZenToWei(transferred_amount_in_zen))
        tx_json = decodeTransaction(sc_node_2, payload=raw_tx)
        assert_equal(tx_json['legacy'], True)
        assert_equal(tx_json['eip155'], True)
        assert_equal(tx_json['eip1559'], False)
        assert_equal(tx_json['signed'], False)

        self.do_test_raw_tx(
            raw_tx=raw_tx, sc_node=sc_node_1, evm_signer_address=evm_address_sc1)




        raw_tx = createRawEIP1559Transaction(sc_node_1,
                                            fromAddress=evm_address_sc1,
                                            toAddress=evm_address_sc2,
                                            value=convertZenToWei(transferred_amount_in_zen))
        tx_json = decodeTransaction(sc_node_2, payload=raw_tx)
        assert_equal(tx_json['legacy'], False)
        assert_equal(tx_json['eip155'], False)
        assert_equal(tx_json['eip1559'], True)
        assert_equal(tx_json['signed'], False)

        self.do_test_raw_tx(
            raw_tx=raw_tx, sc_node=sc_node_1, evm_signer_address=evm_address_sc1)

        # Negative tests
        # 1) use a wrong address for signature
        wrong_signer_address = sc_node_2.wallet_createPrivateKeySecp256k1()["result"]["proposition"]["address"]

        raw_tx = createRawLegacyTransaction(sc_node_1,
                                            fromAddress=evm_address_sc1,
                                            toAddress=evm_address_sc2,
                                            value=convertZenToWei(transferred_amount_in_zen))
        try:
            signTransaction(sc_node_1, fromAddress=wrong_signer_address, payload=raw_tx)
            fail("Wrong from address for signing should not work")
        except RuntimeError as err:
            print("Expected exception thrown: {}".format(err))
            # error is raised from API since the address has no balance
            assert_true("ErrorInsufficientBalance"  in str(err) )

        self.sc_sync_all()

        # 2) use a valid but wrong signature in the raw tx
        sig_v = "1b"
        sig_r = "20d7f34682e1c2834fcb0838e08be184ea6eba5189eda34c9a7561a209f7ed04"
        sig_s = "7c63c158f32d26630a9732d7553cfc5b16cff01f0a72c41842da693821ccdfcb"

        raw_tx = createRawLegacyTransaction(sc_node_1,
                                            fromAddress=evm_address_sc1,
                                            toAddress=evm_address_sc2,
                                            value=convertZenToWei(Decimal('0.1')),
                                            signature_v=sig_v, signature_r=sig_r, signature_s=sig_s)
        sig_json = decodeTransaction(sc_node_2, payload=raw_tx)['signature']
        assert_equal(sig_json['v'], sig_v)
        assert_equal(sig_json['r'], sig_r)
        assert_equal(sig_json['s'], sig_s)

        try:
            sendTransaction(sc_node_1, payload=raw_tx)
            fail("Valid but wrong signature should not work")
        except RuntimeError as err:
            print("Expected exception thrown: {}".format(err))
            # error is raised from state txModify since the address resulting from the valid but wrong signature has no balance
            assert_true("Insufficient funds" in str(err) )

        self.sc_sync_all()


        # 3.1) try sending an unsigned tx
        unsigned_raw_tx = createRawLegacyEIP155Transaction(sc_node_1,
                                            fromAddress=evm_address_sc1,
                                            toAddress=evm_address_sc2,
                                            value=convertZenToWei(Decimal('0.1')))
        tx_json = decodeTransaction(sc_node_2, payload=unsigned_raw_tx)
        assert_equal(tx_json['signed'], False)

        try:
            sendTransaction(sc_node_1, payload=unsigned_raw_tx)
            fail("Valid but wrong signature should not work")
        except RuntimeError as err:
            print("Expected exception thrown: {}".format(err))
            assert_true("is not signed" in str(err) )


        self.sc_sync_all()


        # 3.2) try forging a block with the same unsigned tx.
        #      The block is forged but the tx is not included in the block because is not signed
        bhash = generate_next_block(sc_node_1, "first node", forced_tx=[unsigned_raw_tx])
        self.sc_sync_all()
        block_json = http_block_best(sc_node_1)
        assert_equal(block_json["header"]["id"], bhash)
        assert_true(len(block_json["sidechainTransactions"]) == 0)


        self.sc_sync_all()

        # 4.1) try sending a tx with wrong chainId
        raw_tx = createRawLegacyEIP155Transaction(sc_node_1,
                                            fromAddress=evm_address_sc1,
                                            toAddress=evm_address_sc2,
                                            value=convertZenToWei(Decimal('0.1')))
        chainId_ok = decodeTransaction(sc_node_2, payload=raw_tx)['chainId']

        # get the last byte of chain id value in the hex representation (bytes 44 and 45 in this tx) and decrement it
        tx_hex_array = list(bytearray(a2b_hex(raw_tx)))
        tx_hex_array[45] -= 1
        new_eip155_raw_tx = b2x(bytearray(tx_hex_array))

        chainId_bad = decodeTransaction(sc_node_2, payload=new_eip155_raw_tx)['chainId']
        assert_equal(chainId_bad, chainId_ok-1)

        bad_chainid_raw_tx = signTransaction(sc_node_1, fromAddress=evm_address_sc1, payload=new_eip155_raw_tx)

        tx_json = decodeTransaction(sc_node_1, payload=bad_chainid_raw_tx)
        assert_equal(tx_json['signed'], True)

        try:
            sendTransaction(sc_node_1, payload=bad_chainid_raw_tx)
            fail("Wrong chainId should not work")
        except RuntimeError as err:
            print("Expected exception thrown: {}".format(err))
            assert_true("different from expected SC chainId" in str(err) )

        self.sc_sync_all()



        # 4.2) try forging a block with the same tx
        try:
            ret = generate_next_block(sc_node_1, "first node", forced_tx=[bad_chainid_raw_tx])
            fail("Wrong chainId should not work")
        except Exception as e:
            logging.info("We had an exception as expected: {}".format(str(e)))
            assert_true("does not match network chain ID " in str(e))

        self.sc_sync_all()





if __name__ == "__main__":
    SCEvmRawTxHttpApi().main()
