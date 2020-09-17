package com.xu.rpc.model;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

/**
 * Rpc请求消息的结构
 * 消息Id、类的名字、方法名字、各个参数的类型、各个参数的值
 */
@Getter
@Setter
public class MessageRequest implements Serializable {

    private String messageId;

    private String methodName;

    private Class<?>[] typeParameters;

    private Object[] parametersVal;

    private String interfaceName;

    private boolean isHeartbeat;

    private String token;

    @Override
    public String toString() {
        return ReflectionToStringBuilder.toStringExclude(this, new String[]{"typeParameters", "parametersVal"});
    }
}

