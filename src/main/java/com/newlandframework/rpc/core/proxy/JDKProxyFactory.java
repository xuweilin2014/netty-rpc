package com.newlandframework.rpc.core.proxy;

import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.remoting.client.MessageSendProxy;
import com.newlandframework.rpc.protocol.AbstractProxyInvoker;
import com.newlandframework.rpc.protocol.Invoker;
import com.newlandframework.rpc.util.URL;
import org.apache.commons.lang3.reflect.MethodUtils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;

public class JDKProxyFactory{

    public static Object getProxy(Class<?> interfaces) throws RpcException {
        return Proxy.newProxyInstance(Thread.currentThread().getContextClassLoader(),
                new Class<?>[]{interfaces}, new MessageSendProxy());
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
