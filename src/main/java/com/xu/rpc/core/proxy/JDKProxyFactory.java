package com.xu.rpc.core.proxy;

import com.xu.rpc.exception.RpcException;
import com.xu.rpc.remoting.client.MessageSendProxy;
import com.xu.rpc.protocol.AbstractProxyInvoker;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.util.URL;
import org.apache.commons.lang3.reflect.MethodUtils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;

public class JDKProxyFactory{

    public static Object getProxy(Invoker invoker) throws RpcException {
        return Proxy.newProxyInstance(Thread.currentThread().getContextClassLoader(),
                new Class<?>[]{invoker.getInterface()}, new MessageSendProxy());
    }

    public static Invoker getInvoker(Object proxy, URL url) {
        return new AbstractProxyInvoker(proxy, url) {
            @Override
            public Class<?> getInterface() {
                return proxy.getClass();
            }

            @Override
            public Object doInvoke(Object serviceBean, String methodName, Object[] parameters)
                    throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
                return MethodUtils.invokeMethod(serviceBean, methodName, parameters);
            }
        };
    }
}
