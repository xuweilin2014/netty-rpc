package com.xu.rpc.model;

import java.io.Serializable;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

/**
 * Rpc请求消息的结构
 * 消息Id、类的名字、方法名字、各个参数的类型、各个参数的值
 */
public class MessageRequest implements Serializable {

    private String messageId;

    private String methodName;

    private Class<?>[] typeParameters;

    private Object[] parametersVal;

    private String interfaceName;

    private boolean isHeartbeat;

    public String getMessageId() {
        return messageId;
    }

    public void setMessageId(String messageId) {
        this.messageId = messageId;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public Class<?>[] getTypeParameters() {
        return typeParameters;
    }

    public void setTypeParameters(Class<?>[] typeParameters) {
        this.typeParameters = typeParameters;
    }

    public Object[] getParametersVal() {
        return parametersVal;
    }

    public void setParametersVal(Object[] parametersVal) {
        this.parametersVal = parametersVal;
    }

    public String getInterfaceName() {
        return interfaceName;
    }

    public void setInterfaceName(String interfaceName) {
        this.interfaceName = interfaceName;
    }

    public boolean isHeartbeat() {
        return isHeartbeat;
    }

    public void setHeartbeat(boolean heatbeat) {
        isHeartbeat = heatbeat;
    }

    @Override
    public String toString() {
        return ReflectionToStringBuilder.toStringExclude(this, new String[]{"typeParameters", "parametersVal"});
    }
}

