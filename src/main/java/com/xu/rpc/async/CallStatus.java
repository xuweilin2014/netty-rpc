package com.xu.rpc.async;


public enum CallStatus {
    RUN,
    TIMEOUT,
    DONE;

    public boolean isRun() {
        return this == RUN;
    }

    public boolean isTimeout() {
        return this == TIMEOUT;
    }

    public boolean isDone() {
        return this == DONE;
    }
}

