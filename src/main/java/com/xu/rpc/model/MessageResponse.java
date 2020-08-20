package com.xu.rpc.model;

import java.io.Serializable;

import com.xu.rpc.remoting.execution.MethodInvokeStatus;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

/**
 * RPC应答消息结构
 * 消息ID、错误信息、Rpc调用的结果
 */
public class MessageResponse implements Serializable {

    private String messageId;

    private Throwable error;

    private Object result;

    private MethodInvokeStatus invokeStatus;

    @Override
    public String toString() {
        return ReflectionToStringBuilder.toString(this);
    }

    public String getMessageId() {
        return messageId;
    }

    public void setMessageId(String messageId) {
        this.messageId = messageId;
    }

    public Throwable getError() {
        return error;
    }

    public void setError(Throwable error) {
        this.error = error;
    }

    public Object getResult() {
        return result;
    }

    public void setResult(Object result) {
        this.result = result;
    }

    public MethodInvokeStatus getInvokeStatus() {
        return invokeStatus;
    }

    public void setInvokeStatus(MethodInvokeStatus invokeStatus) {
        this.invokeStatus = invokeStatus;
    }
}
