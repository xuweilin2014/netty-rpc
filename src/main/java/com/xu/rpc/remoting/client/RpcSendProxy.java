package com.xu.rpc.remoting.client;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.protocol.Invoker;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class RpcSendProxy implements InvocationHandler {

    private Invoker invoker;
    
    public RpcSendProxy(Invoker invoker){
        this.invoker = invoker;
    }
    
    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        // TODO: 2020/8/30
        return invoker.invoke(new RpcInvocation());
    }

}
