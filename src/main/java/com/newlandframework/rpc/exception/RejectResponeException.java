package com.newlandframework.rpc.exception;


public class RejectResponeException extends RuntimeException {
    public RejectResponeException() {
        super();
    }

    public RejectResponeException(String message, Throwable cause) {
        super(message, cause);
    }

    public RejectResponeException(String message) {
        super(message);
    }

    public RejectResponeException(Throwable cause) {
        super(cause);
    }
}

