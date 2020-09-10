package com.xu.rpc.remoting.support;

public enum MethodInvokeStatus {

    INIT,

    DONE,

    EXCEPTIONAL,

    TIMEOUT,

    CANCELLED;

    public boolean isDone(){
        return this == DONE;
    }

    public boolean isExceptional(){
        return this == EXCEPTIONAL;
    }

    public boolean isTimeout(){
        return this == TIMEOUT;
    }

    public boolean isCancelled() {
        return this == CANCELLED;
    }
}
