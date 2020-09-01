package com.xu.rpc.core;

import com.xu.rpc.remoting.support.MethodInvokeStatus;

import java.io.Serializable;

public class RpcResult implements Serializable {

    private static final long serialVersionUID = -6925924956850004727L;

    private Object result;

    private Throwable exception;

    private MethodInvokeStatus invokeStatus;

    private long invokeTimespan;

    public RpcResult(){

    }

    public RpcResult(Object result){
        this.result = result;
    }

    public long getInvokeTimespan() {
        return invokeTimespan;
    }

    public void setInvokeTimespan(long invokeTimespan) {
        this.invokeTimespan = invokeTimespan;
    }

    public RpcResult(Throwable t){
        this.exception = t;
    }

    public Object getResult() throws Throwable {
        if (exception != null)
            throw exception;
        return result;
    }

    public void setResult(Object result) {
        this.result = result;
    }

    public Throwable getException() {
        return exception;
    }

    public void setException(Throwable exception) {
        this.exception = exception;
    }

    public MethodInvokeStatus getInvokeStatus() {
        return invokeStatus;
    }

    public void setInvokeStatus(MethodInvokeStatus invokeStatus) {
        this.invokeStatus = invokeStatus;
    }


}
