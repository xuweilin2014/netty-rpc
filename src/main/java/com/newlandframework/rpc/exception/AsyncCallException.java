package com.newlandframework.rpc.exception;


public class AsyncCallException extends RuntimeException {
    public AsyncCallException() {
        super();
    }

    public AsyncCallException(String message, Throwable cause) {
        super(message, cause);
    }

    public AsyncCallException(String message) {
        super(message);
    }

    public AsyncCallException(Throwable cause) {
        super(cause);
    }
}

