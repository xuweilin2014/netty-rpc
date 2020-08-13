package com.newlandframework.rpc.remoting.execution;

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
