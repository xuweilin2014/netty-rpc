package com.newlandframework.rpc.model;

import java.io.Serializable;

import lombok.Data;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

/**
 * RPC应答消息结构
 * 消息ID、错误信息、Rpc调用的结果
 */
@Data
public class MessageResponse implements Serializable {

    private String messageId;
    private String error;
    private Object result;
    private boolean returnNotNull;

    @Override
    public String toString() {
        return ReflectionToStringBuilder.toString(this);
    }
}

