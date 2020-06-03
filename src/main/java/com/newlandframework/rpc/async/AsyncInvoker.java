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
        //getGenericInterfaces返回一个Type数组，表示此对象直接实现的接口的信息，其中包含了泛型信息，举例来说：
        //调用AsyncRpcCallTest方法传进来的callback对象是AsyncCallback接口的一个实现类的对象，即 AsyncRpcCall<CostTime>。
        //调用getGenericInterfaces()[0]所得到的就是AsyncCallback<CostTime>的Type对象（包含泛型信息）。由于使用了泛型，所以它是ParameterizedType类型。
        //调用getInterfaces()[0]得到的就是AsyncCallBack的Class对象（不包含泛型信息）。
        Type type = callback.getClass().getGenericInterfaces()[0];
        if (type instanceof ParameterizedType) {
            // getGenericClass方法返回值returnClass为泛型中实际参数类型，比如AsyncCallback<CostTime>中的CostTime.class
            Class<?> returnClass = ReflectionUtils.getGenericClass((ParameterizedType) type, 0);
            return intercept(callback, returnClass);
        } else {
            throw new AsyncCallException("NettyRPC AsyncCallback must be parameterized type!");
        }
    }

    //submit方法把传进来的Callable对象，封装进AsyncFuture对象（实现了FutureTask接口），并且提交到线程池中去执行。
    //这个future是此项目中异步调用的关键
    private <T> AsyncFuture<T> submit(Callable<T> task) {
        AsyncFuture future = new AsyncFuture<T>(task);
        executor.submit(future);
        return future;
    }

    private <T> T intercept(final AsyncCallback<T> callback, Class<?> returnClass) {
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
        Future<T> future = submit(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return callback.call();
            }
        });

        // SYSTEM_PROPERTY_ASYNC_MESSAGE_CALLBACK_TIMEOUT 的值默认为60s，这里的returnClass是CostTime.class，
        // future就是AsyncFuture，也就是一个FutureTask类型，根据这个future可以得知线程池运行任务的结果。
        // 这是因为，在FutureTask在线程池中被执行结束之后，上面实现Callable接口的匿名内部类中的call方法返回的结果，
        // 会保存在AsyncFuture对象中（其实是保存在FutureTask对象中的outcome属性中），可以通过FutureTask的get方法获取到。
        AsyncCallResult result = new AsyncCallResult(returnClass, future,
                RpcSystemConfig.SYSTEM_PROPERTY_ASYNC_MESSAGE_CALLBACK_TIMEOUT);

        T asyncProxy = (T) result.getResult();

        return asyncProxy;
    }
}

