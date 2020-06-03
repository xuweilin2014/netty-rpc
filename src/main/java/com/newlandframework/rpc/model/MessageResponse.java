package com.newlandframework.rpc.model;

import java.io.Serializable;

import com.newlandframework.rpc.netty.server.MethodInvokeStatus;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

/**
 * RPC应答消息结构
 * 消息ID、错误信息、Rpc调用的结果
 */
public class MessageResponse implements Serializable {

    private String messageId;
    private String error;
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

    public String getError() {
        return error;
    }

    public void setError(String error) {
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

