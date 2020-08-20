package com.xu.rpc.exception;

public class RpcException extends RuntimeException {

    public RpcException(){
        super();
    }

    public RpcException(String message, Throwable cause){
        super(message, cause);
    }

}
