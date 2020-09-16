package com.xu.rpc.remoting.client;

import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.protocol.Invoker;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;

public class ProxyWrapper implements InvocationHandler {

    private Invoker<?> invoker;

    public <T> ProxyWrapper(Invoker<T> invoker){
        this.invoker = invoker;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        String methodName = method.getName();
        if (method.getDeclaringClass() == Object.class){
            return method.invoke(proxy, args);
        }

        if ("toString".equals(methodName)){
            return invoker.toString();
        }
        if ("hashcode".equals(methodName)){
            return invoker.hashCode();
        }
        if ("equals".equals(methodName)){
            return invoker.equals(args[0]);
        }

        RpcResult result = invoker.invoke(new RpcInvocation(method, args));
        if (result.getException() != null)
            throw result.getException();
        return result.getResult();
    }

}
