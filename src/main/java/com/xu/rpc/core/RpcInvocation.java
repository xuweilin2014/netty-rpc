package com.xu.rpc.core;

import java.lang.reflect.Method;
import java.util.Arrays;

public class RpcInvocation {

    private String methodName;

    private Object[] parameters;

    private Class<?>[] parameterTypes;

    private Class<?> serviceType;

    private String token;

    public RpcInvocation(Method method, Object[] args){
        this.methodName = method.getName();
        this.parameterTypes = method.getParameterTypes();
        this.serviceType = method.getDeclaringClass();
        this.parameters = args;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public Object[] getParameters() {
        return parameters;
    }

    public void setParameters(Object[] parameters) {
        this.parameters = parameters;
    }

    public Class<?>[] getParameterTypes() {
        return parameterTypes;
    }

    public void setParameterTypes(Class<?>[] parameterTypes) {
        this.parameterTypes = parameterTypes;
    }

    public Class<?> getServiceType() {
        return serviceType;
    }

    public void setServiceType(Class<?> serviceType) {
        this.serviceType = serviceType;
    }

    @SuppressWarnings("StringBufferReplaceableByString")
    public String key(){
        return new StringBuilder(serviceType.getName()).append(methodName)
                .append(Arrays.toString(parameterTypes)).append(Arrays.toString(parameters)).toString();
    }

    public String getToken() {
        return token;
    }

    public void setToken(String token) {
        this.token = token;
    }
}
