package com.xu.rpc.core;

import lombok.Getter;
import lombok.Setter;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Setter
@Getter
public class RpcInvocation {

    private String methodName;

    private Method method;

    private Object[] parameters;

    private Class<?>[] parameterTypes;

    private Class<?> serviceType;

    private Map<String, String> attachments;

    public RpcInvocation(Method method, Object[] args){
        this.method = method;
        this.methodName = method.getName();
        this.parameterTypes = method.getParameterTypes();
        this.serviceType = method.getDeclaringClass();
        this.parameters = args;
        this.attachments = new ConcurrentHashMap<>();
    }

    @SuppressWarnings("StringBufferReplaceableByString")
    public String key(){
        return new StringBuilder(serviceType.getName()).append(methodName)
                .append(Arrays.toString(parameterTypes)).append(Arrays.toString(parameters)).toString();
    }
}
