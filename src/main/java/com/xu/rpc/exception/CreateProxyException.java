package com.xu.rpc.exception;

public class CreateProxyException extends RuntimeException {
    public CreateProxyException() {
        super();
    }

    public CreateProxyException(String message, Throwable cause) {
        super(message, cause);
    }

    public CreateProxyException(String message) {
        super(message);
    }

    public CreateProxyException(Throwable cause) {
        super(cause);
    }
}

