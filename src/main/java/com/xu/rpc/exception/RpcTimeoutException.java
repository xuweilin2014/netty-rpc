package com.xu.rpc.exception;

public class RpcTimeoutException extends RemotingException {

    public RpcTimeoutException(String s) {
        super(s);
    }

}
