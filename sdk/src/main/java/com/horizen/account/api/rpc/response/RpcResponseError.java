package com.horizen.account.api.rpc.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.horizen.account.api.rpc.utils.RpcError;
import com.horizen.api.http.ErrorResponse;

import java.util.Optional;

public class RpcResponseError extends RpcResponse implements ErrorResponse {
    protected final RpcError error;

    public RpcResponseError(String id, RpcError error) {
        super(id);
        this.error = error;
    }

    @JsonInclude()
    public RpcError getError() {
        return error;
    }

    @Override
    public String code() {
        return String.valueOf(error.getCode());
    }

    @Override
    public String description() {
        return error.getMessage();
    }

    @Override
    public Optional<Throwable> exception() {
        return Optional.empty();
    }

    @Override
    public String toString() {
        return String.format("RpcResponseError{jsonrpc='%s', id='%s', error=%s}", jsonrpc, id, error);
    }
}
