package com.newlandframework.rpc.remoting.execution;

public enum MethodInvokeStatus {

    INIT,

    DONE,

    EXCEPTIONAL;

    public boolean isDone(){
        return this == DONE;
    }

    public boolean isExceptional(){
        return this == EXCEPTIONAL;
    }
}
