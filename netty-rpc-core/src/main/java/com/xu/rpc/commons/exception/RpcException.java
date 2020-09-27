package com.xu.rpc.commons.exception;

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
