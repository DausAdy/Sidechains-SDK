import json


def createRawEIP1559Transaction(sidechainNode, *, fromAddress=None, toAddress=None, nonce=None, gasLimit=230000,
                             maxPriorityFeePerGas=900000000, maxFeePerGas=900000000, value=0, data='', api_key=None):
    j = {
        "from": fromAddress,
        "to": toAddress,
        "nonce": nonce,
        "gasLimit": gasLimit,
        "maxPriorityFeePerGas": maxPriorityFeePerGas,
        "maxFeePerGas": maxFeePerGas,
        "value": value,
        "data": data,
        "outputRawBytes": True
    }

    request = json.dumps(j)
    if api_key is not None:
        response = sidechainNode.transaction_createEIP1559Transaction(request, api_key)
    else:
        response = sidechainNode.transaction_createEIP1559Transaction(request)

    if "result" in response:
        if "transactionBytes" in response["result"]:
            return response["result"]["transactionBytes"]

    raise RuntimeError("Something went wrong, see {}".format(str(response)))

