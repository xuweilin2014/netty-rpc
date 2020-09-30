package com.xu.rpc.remoting.support;

public enum MethodInvokeStatus {

    INIT,
    // 正常执行完成
    DONE,
    // 发生异常
    EXCEPTIONAL,
    // 超时
    TIMEOUT,
    // 被取消
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
