package com.newlandframework.rpc.netty.server;

public enum MethodInvokeStatus {
    INIT,
    REJECTED,
    DONE,
    EXCEPTIONAL;

    public boolean isRejected(){
        return this == REJECTED;
    }

    public boolean isDone(){
        return this == DONE;
    }

    public boolean isExceptional(){
        return this == EXCEPTIONAL;
    }
}
