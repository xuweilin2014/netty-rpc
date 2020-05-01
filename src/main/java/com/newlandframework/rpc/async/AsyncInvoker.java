package com.newlandframework.rpc.async;

import com.newlandframework.rpc.core.ReflectionUtils;
import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.exception.AsyncCallException;
import com.newlandframework.rpc.parallel.RpcThreadPool;

import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadPoolExecutor;

public class AsyncInvoker {
    private ThreadPoolExecutor executor = (ThreadPoolExecutor) RpcThreadPool.getExecutor(
            RpcSystemConfig.SYSTEM_PROPERTY_THREADPOOL_THREAD_NUMS, RpcSystemConfig.SYSTEM_PROPERTY_THREADPOOL_QUEUE_NUMS);

    public <T> T submit(final AsyncCallback<T> callback) {
        // getGenericInterfaces返回对象实现的接口信息的Type数组，包含泛型信息，举例来说，
        // 调用AsyncRpcCallTest方法传进来的callback对象是AsyncCallback接口的一个实现类，即 AsyncRpcCall<CostTime>。
        // 所以调用getGenericInterfaces()[0]所得到的就是AsyncCallback<CostTime>对象（包含泛型信息）。由于使用了泛型，
        // 所以它是ParameterizedType类型。
        Type type = callback.getClass().getGenericInterfaces()[0];
        if (type instanceof ParameterizedType) {
            // getGenericClass方法返回值returnClass为泛型中实际参数类型，比如AsyncCallback<CostTime>中的CostTime.class
            Class returnClass = (Class) ReflectionUtils.getGenericClass((ParameterizedType) type, 0);
            return intercept(callback, returnClass);
        } else {
            throw new AsyncCallException("NettyRPC AsyncCallback must be parameterized type!");
        }
    }


    private <T> AsyncFuture<T> submit(Callable<T> task) {
        AsyncFuture future = new AsyncFuture<T>(task);
        executor.submit(future);
        return future;
    }

    private <R> R intercept(final AsyncCallback<R> callback, Class<?> returnClass) {
        if (!Modifier.isPublic(returnClass.getModifiers())) {
            return callback.call();
        } else if (Modifier.isFinal(returnClass.getModifiers())) {
            return callback.call();
        } else if (Void.TYPE.isAssignableFrom(returnClass)) {
            return callback.call();
        } else if (returnClass.isPrimitive() || returnClass.isArray()) {
            return callback.call();
        } else if (returnClass == Object.class) {
            return callback.call();
        } else {
            return submit(callback, returnClass);
        }
    }

    private <T> T submit(final AsyncCallback<T> callback, Class<?> returnClass) {
        Future future = submit(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return callback.call();
            }
        });

        // SYSTEM_PROPERTY_ASYNC_MESSAGE_CALLBACK_TIMEOUT 的值默认为60s
        AsyncCallResult result = new AsyncCallResult(returnClass, future,
                RpcSystemConfig.SYSTEM_PROPERTY_ASYNC_MESSAGE_CALLBACK_TIMEOUT);

        T asyncProxy = (T) result.getResult();

        return asyncProxy;
    }
}

