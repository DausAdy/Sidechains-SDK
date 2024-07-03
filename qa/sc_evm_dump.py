#!/usr/bin/env python3
import json
import os
import pprint
from decimal import Decimal
from stat import S_IREAD, S_IRGRP, S_IROTH

from eth_utils import add_0x_prefix

from SidechainTestFramework.account.ac_chain_setup import AccountChainSetup
from SidechainTestFramework.account.ac_use_smart_contract import SmartContract
from SidechainTestFramework.account.ac_utils import rpc_get_balance
from SidechainTestFramework.account.httpCalls.transaction.createEIP1559Transaction import createEIP1559Transaction
from SidechainTestFramework.account.utils import WITHDRAWAL_REQ_SMART_CONTRACT_ADDRESS, \
    FORGER_STAKE_SMART_CONTRACT_ADDRESS
from SidechainTestFramework.sc_boostrap_info import SCNodeConfiguration, MCConnectionInfo, SCNetworkConfiguration, \
    SCCreationInfo
from SidechainTestFramework.scutil import generate_next_block, \
    connect_sc_nodes, assert_equal, assert_true, bootstrap_sidechain_nodes, AccountModel, EVM_APP_SLOT_TIME
from test_framework.util import forward_transfer_to_sidechain, websocket_port_by_mc_node_index

"""
Test zen_dump rpc method.

Configuration:
    - 2 SC nodes connected with each other, one of them has dump enabled.
    - 1 MC node

Test: 
    - Try zen_dump on node with dump disabled: it should not be allowed
    - try dump on different blocks
    - try with wrong params
    - load test with lots of accounts (only if uncommented)
    
"""


def deploy_smart_contract(node, smart_contract, from_address, *args,
                          nonce=None):
    _, address = smart_contract.deploy(node, *args,
                                       fromAddress=from_address,
                                       gasLimit=200000,
                                       nonce=nonce)

    return address


class SCEvmDump(AccountChainSetup):

    def __init__(self):
        super().__init__(number_of_sidechain_nodes=2, connect_nodes=False,
                         block_timestamp_rewind=1500 * EVM_APP_SLOT_TIME * 1000)

    def sc_setup_chain(self):
        mc_node = self.nodes[0]
        sc_node_configuration = [
            SCNodeConfiguration(
                MCConnectionInfo(
                    address="ws://{0}:{1}".format(mc_node.hostname, websocket_port_by_mc_node_index(0))),
                api_key=self.API_KEY,
                remote_keys_manager_enabled=self.remote_keys_manager_enabled,
                max_nonce_gap=110,
                max_account_slots=110,
                evm_state_dump_enabled=False),
            SCNodeConfiguration(
                MCConnectionInfo(
                    address="ws://{0}:{1}".format(mc_node.hostname, websocket_port_by_mc_node_index(0))),
                api_key=self.API_KEY,
                remote_keys_manager_enabled=self.remote_keys_manager_enabled,
                max_nonce_gap=110,
                max_account_slots=110,
                evm_state_dump_enabled=True)
        ]

        network = SCNetworkConfiguration(SCCreationInfo(mc_node, self.forward_amount, self.withdrawalEpochLength),
                                         *sc_node_configuration)
        self.sc_nodes_bootstrap_info = bootstrap_sidechain_nodes(self.options, network,
                                                                 block_timestamp_rewind=self.block_timestamp_rewind,
                                                                 model=AccountModel)

    def run_test(self):
        mc_node = self.nodes[0]
        sc_node_without_dump = self.sc_nodes[0]
        sc_node_with_dump = self.sc_nodes[1]
        connect_sc_nodes(sc_node_without_dump, 1)

        # transfer some fund from MC to SC1 at a new evm address, then mine mc block
        evm_address_sc1 = sc_node_without_dump.wallet_createPrivateKeySecp256k1()["result"]["proposition"]["address"]
        evm_address_sc1_prefix = add_0x_prefix(evm_address_sc1)

        ft_amount_in_zen = Decimal('500.0')

        mc_return_address = mc_node.getnewaddress()
        forward_transfer_to_sidechain(self.sc_nodes_bootstrap_info.sidechain_id,
                                      mc_node,
                                      evm_address_sc1,
                                      ft_amount_in_zen,
                                      mc_return_address=mc_return_address,
                                      generate_block=True)

        self.sync_all()

        generate_next_block(sc_node_without_dump, "first node")
        self.sc_sync_all()

        # Create a transaction on node 1. Verify that the tx is in node 3 mempool but seeder node mempool is empty
        nonce_addr_1 = 0
        createEIP1559Transaction(sc_node_without_dump, fromAddress=evm_address_sc1, toAddress=evm_address_sc1,
                                 nonce=nonce_addr_1, gasLimit=230000, maxPriorityFeePerGas=900000000,
                                 maxFeePerGas=900000000, value=1)
        nonce_addr_1 += 1

        # Generate a block in order to clean the mempool
        block_id_1 = generate_next_block(sc_node_without_dump, "first node")
        self.sc_sync_all()

        dump_file = os.path.join(self.options.tmpdir, "dump.json")

        contract = SmartContract("Storage")
        contract_address = add_0x_prefix(deploy_smart_contract(sc_node_without_dump, contract, evm_address_sc1,
                                                               15)).lower()
        nonce_addr_1 += 1

        block_id_2 = generate_next_block(sc_node_without_dump, "first node")
        self.sc_sync_all()

        # Try the dump
        sc_node_with_dump.rpc_zen_dump(add_0x_prefix(block_id_2), dump_file)

        with open(dump_file, 'r') as file:
            dump_data = json.load(file)
        accounts = dump_data["accounts"]
        assert_equal(4, len(accounts))
        assert_true(add_0x_prefix(WITHDRAWAL_REQ_SMART_CONTRACT_ADDRESS) in accounts)
        assert_true(add_0x_prefix(FORGER_STAKE_SMART_CONTRACT_ADDRESS) in accounts)
        assert_true(evm_address_sc1_prefix in accounts)
        evm_address_sc1_account = accounts[evm_address_sc1_prefix]
        assert_equal(nonce_addr_1, evm_address_sc1_account["nonce"])
        evm_address_sc1_balance = rpc_get_balance(sc_node_with_dump, evm_address_sc1_prefix)
        assert_equal(str(evm_address_sc1_balance), evm_address_sc1_account["balance"])
        assert_true(contract_address in accounts)
        assert_equal(1, accounts[contract_address]["nonce"])

        response = sc_node_without_dump.rpc_zen_dump(add_0x_prefix(block_id_1), dump_file)
        self.check_rpc_not_allowed(response)

        # Try the dump at an old block
        sc_node_with_dump.rpc_zen_dump(add_0x_prefix(block_id_1), dump_file)

        with open(dump_file, 'r') as file:
            dump_data = json.load(file)

        accounts = dump_data["accounts"]
        assert_equal(3, len(dump_data["accounts"]))
        assert_true(add_0x_prefix(WITHDRAWAL_REQ_SMART_CONTRACT_ADDRESS) in accounts)
        assert_true(add_0x_prefix(FORGER_STAKE_SMART_CONTRACT_ADDRESS) in accounts)
        assert_true(evm_address_sc1_prefix in accounts)
        evm_address_sc1_account = accounts[evm_address_sc1_prefix]
        assert_equal(nonce_addr_1 - 1, evm_address_sc1_account["nonce"])
        eip1898_block_hash = {
            "blockHash": add_0x_prefix(block_id_1)
        }

        res = sc_node_with_dump.rpc_eth_getBalance(evm_address_sc1_prefix, eip1898_block_hash)
        evm_address_sc1_balance = int(res['result'], 16)
        assert_equal(str(evm_address_sc1_balance), evm_address_sc1_account["balance"])

        # Make the dump file readonly and check that the dump will fail
        os.chmod(dump_file, S_IREAD | S_IRGRP | S_IROTH)
        response = sc_node_with_dump.rpc_zen_dump(add_0x_prefix(block_id_1), dump_file)
        assert_true("error" in response)

        # # Uncomment to do the load test
        # # Load test: creates a lot of accounts and tries dump
        # for i in range(0, 1000):
        #     pprint.pprint("Iteration {}".format(i))
        #     for k in range(0, 100):
        #         deploy_smart_contract(sc_node_without_dump, contract, evm_address_sc1, k, nonce=nonce_addr_1)
        #         nonce_addr_1 += 1
        #
        #     generate_next_block(sc_node_without_dump, "first node")
        #     # Generate a second block, so the base fee remains low
        #     block_id = generate_next_block(sc_node_without_dump, "first node")
        #     mc_node.generate(1)
        #     self.sc_sync_all()
        #
        # self.sc_sync_all()
        #
        # dump_file = os.path.join(self.options.tmpdir, "load_dump.json")
        #
        # sc_node_with_dump.rpc_zen_dump(add_0x_prefix(block_id), dump_file)
        # with open(dump_file, 'r') as file:
        #     dump_data = json.load(file)
        # accounts = dump_data["accounts"]
        # # There are 100005 instead of 100004 because the redistribution of the fees creates the account related to the default delegator
        # assert_equal(100005, len(accounts))

    @staticmethod
    def check_rpc_not_allowed(response):
        assert_true("error" in response)
        assert_equal("Action not allowed", response['error']['message'])
        assert_equal(2, response['error']['code'])


if __name__ == "__main__":
    SCEvmDump().main()
