package com.xu.rpc.core.proxy;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.protocol.AbstractProxyInvoker;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.remoting.client.ProxyWrapper;
import com.xu.rpc.core.RpcConfig;
import org.apache.commons.lang3.reflect.MethodUtils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;

public class JDKProxyFactory {

    public static <T> T getProxy(Invoker<T> invoker) throws RpcException {
        // 如果用户开启了桩 Stub 模式的话，会将生成的 proxy 再进行一次封装
        return StubProxyWrapper.createProxyStub(invoker);
    }

    public static <T> Invoker<T> getInvoker(Object proxy, URL url, Class<?> type) {
        return new AbstractProxyInvoker<T>(proxy, url, type) {
            @Override
            public Object doInvoke(Object serviceBean, String methodName, Object[] parameters)
                    throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
                return MethodUtils.invokeMethod(serviceBean, methodName, parameters);
            }
        };
    }

    static class StubProxyWrapper{

        @SuppressWarnings("unchecked")
        static <T> T createProxyStub(Invoker<T> invoker){
            T proxy = (T) Proxy.newProxyInstance(Thread.currentThread().getContextClassLoader(),
                    new Class<?>[]{invoker.getInterface()}, new ProxyWrapper(invoker));
            // stub 属性的值可以有两种：true/false 以及 stub 类的名字
            String stub = invoker.getUrl().getParameter(RpcConfig.STUB_KEY);
            if (stub != null && stub.length() != 0){
                if (RpcConfig.FALSE.equals(stub))
                    return proxy;

                if (RpcConfig.TRUE.equals(stub)){
                    stub = invoker.getInterface().getName() + "Stub";
                }

                try {
                    // 获取存根的实现类，并且判断是否实现了接口
                    Class<?> stubClass = Thread.currentThread().getContextClassLoader().loadClass(stub);
                    if (!invoker.getInterface().isAssignableFrom(stubClass)){
                        throw new IllegalStateException("stub class " + stubClass.getName() + " did not " +
                                "implement interface " + invoker.getInterface());
                    }
                    // 将 proxy 传入存根类的构造函数中，然后创建存根类对象赋值给 proxy，然后返回
                    // 因此存根类中必须要有一个拷贝构造函数，方便框架将远程调用的 proxy 对象传递进来
                    proxy = (T) stubClass.getConstructor(invoker.getInterface()).newInstance(proxy);
                } catch (ClassNotFoundException e) {
                    throw new IllegalStateException("stub class " + stub + " is not found.");
                } catch (NoSuchMethodException e) {
                    throw new IllegalStateException("no such constructor with parameter " + invoker.getInterface().getName());
                } catch (Throwable t) {
                    throw new IllegalStateException("error occurs when initiating the stub, caused by " + t.getMessage());
                }
            }

            return proxy;
        }

    }
}
