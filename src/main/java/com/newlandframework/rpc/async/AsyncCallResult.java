package com.newlandframework.rpc.async;

import com.newlandframework.rpc.core.ReflectionUtils;
import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.exception.AsyncCallException;
import com.newlandframework.rpc.exception.InvokeTimeoutException;
import net.sf.cglib.proxy.Callback;
import net.sf.cglib.proxy.Enhancer;

import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class AsyncCallResult {
    private Class returnClass;
    private Future future;
    private Long timeout;

    public AsyncCallResult(Class returnClass, Future future, Long timeout) {
        this.returnClass = returnClass;
        this.future = future;
        this.timeout = timeout;
    }

    public Object loadFuture() throws AsyncCallException {
        try {
            if (timeout <= 0L) {
                return future.get();
            } else {
                return future.get(timeout, TimeUnit.MILLISECONDS);
            }
        } catch (TimeoutException e) {
            future.cancel(true);
            throw new AsyncCallException(e);
        } catch (InterruptedException e) {
            throw new AsyncCallException(e);
        } catch (Exception e) {
            translateTimeoutException(e);
            throw new AsyncCallException(e);
        }
    }

    private void translateTimeoutException(Throwable t) {
        int index = t.getMessage().indexOf(RpcSystemConfig.TIMEOUT_RESPONSE_MSG);
        if (index != -1) {
            throw new InvokeTimeoutException(t);
        }
    }

    public Object getResult() {
        Class proxyClass = AsyncProxyCache.get(returnClass.getName());

        if (proxyClass == null) {
            Enhancer enhancer = new Enhancer();
            if (returnClass.isInterface()) {
                enhancer.setInterfaces(new Class[]{AsyncCallObject.class, returnClass});
            } else {
                enhancer.setInterfaces(new Class[]{AsyncCallObject.class});
                enhancer.setSuperclass(returnClass);
            }
            enhancer.setCallbackFilter(new AsyncCallFilter());
            enhancer.setCallbackTypes(new Class[]{
                    AsyncLoadResultInterceptor.class, AsyncLoadStatusInterceptor.class
            });
            proxyClass = enhancer.createClass();
            AsyncProxyCache.save(returnClass.getName(), proxyClass);
        }

        // Enhancer.registerCallbacks不放到if方法里面是因为，传进去的两个Interceptor需要一个新的future对象
        // 如果把这段代码放到if中，就可能会导致每次创建的代理对象的拦截方法中，future始终是同一个对象
        Enhancer.registerCallbacks(proxyClass, new Callback[]{
                new AsyncLoadResultInterceptor(this),
                new AsyncLoadStatusInterceptor(future)});

        try {
            return ReflectionUtils.newInstance(proxyClass);
        } finally {
            Enhancer.registerStaticCallbacks(proxyClass, null);
        }
    }
}

