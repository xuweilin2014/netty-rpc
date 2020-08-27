package com.xu.rpc.exception;

import com.xu.rpc.core.RpcInvocation;

public class RpcException extends RuntimeException {

    public RpcException(){
        super();
    }

    public RpcException(String message){
        super(message);
    }

    public RpcException(String message, Throwable cause){
        super(message, cause);
    }

}
