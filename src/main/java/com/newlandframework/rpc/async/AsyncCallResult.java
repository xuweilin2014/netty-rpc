package com.newlandframework.rpc.async;

import com.newlandframework.rpc.util.ReflectionUtil;
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

    /**
     * 要异步执行的任务最终封装成一个FutureTask，丢到线程池中去执行（具体的就是调用FuturetTask中的run方法）。以AsyncRpcTimeoutCallTest为例，
     * 线程池中的线程会首先执行calculate.busy方法，然后进入到MessageSendProxy去向Rpc服务器发送请求方法，接着调用callBack.start()阻塞，
     * 等待调用的结果返回。默认等待的时间为30s，如果等待超时，就会抛出自定义的InvokeTimeoutException异常。这个异常在FutureTask的run方法中会被捕获，
     * 并且通过setException设置给outcome属性，同时会把FutureTask的状态state修改为EXCEPTIONAL。这是线程池中的线程所做的事情。
     *
     * 在AsyncRpcTimeoutCallTest中的main线程中，调用代理对象的方法（除了_getStatus之外），会调用下面的loadFuture方法来获取上面线程池执行
     * 任务返回的对象。
     *
     * future.get(long timeout, TimeUnit unit)方法返回juc包中的TimeoutException，必须是在state一直为NEW和COMPELTEING
     * ，也就是等待的时间到了，任务仍然在运行过程中，不包括发生异常的情况。而由于上面线程池中抛出异常，把state设置为EXCEPTIONAL，
     * 因此调用report(s)，以InvokeTimeoutException为参数，抛出ExecutionException异常。接着调用translateTimeoutException
     * 将其转化为我们自定义的InvokeTimeoutException异常。
     *
     * 注意，如果是AsyncCallResult#loadFuture中future.get(timeout, unit)抛出超时异常的话，会抛出juc包中的TimeoutException，进入第1个catch语句块
     * 如果是因为MessageSendProxy#invoke方法中的callBack.start()抛出超时异常的话，则future.get(timeout, unit)会抛出ExecutionException，进入第3个catch语句块
     */
    public Object loadFuture() throws AsyncCallException {
        try {
            //当getResult方法返回的代理对象第一次调用其它方法时，就会调用这个loadFuture方法，从
            //future属性中得到返回的结果。如果timeout<=0，则一直阻塞等待；否则的话，就等待指定时间，直到超时
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
        //这里的returnClass是CostTime.class类型的，首先从缓存中查找，看看是否有先前进行了保存。
        Class proxyClass = AsyncProxyCache.get(returnClass.getName());

        //这里的returnClass不是接口类型的。在这里我们使用CGLIB来创建一个动态代理对象，这个对象要实现AsyncCallObject接口，
        //同时也要继承CostTime类。最后，会设置好两个拦截器AsyncCallResultInterceptor和AsyncCallObjectInterceptor，
        //并且设置一个过滤器AsyncCallFilter来决定当这个代理对象调用一个方法时，会使用哪个拦截器来进行拦截。
        //具体的逻辑是这样：当调用AsyncCallObject接口中的方法_getStatus时，就会被AsyncLoadStatusInterceptor拦截，
        //返回一个AsyncCallStatus对象。如果调用的是其他的方法，则被AsyncCallResultInterceptor拦截，这拦截器实现了LazyLoader接口，
        //当第一次调用设置了这个拦截器的动态代理对象的时候，CGLIB会调用这个loadObject()方法获取方法调用实际的返回对象，
        //之后所有拦截到的方法调用都会分派给这个返回的对象。在这里获取到的对象就是前面在线程池中运行，最后保存在FutureTask对象中的结果，
        //在这个例子中是CostTime。
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

        //Enhancer.registerCallbacks不放到if方法里面是因为，传进去的两个Interceptor都需要一个新的future对象
        //如果把这段代码放到if中，就可能会导致每次创建的代理对象的拦截方法中，future始终是同一个对象
        Enhancer.registerCallbacks(proxyClass, new Callback[]{
                new AsyncLoadResultInterceptor(this),
                new AsyncLoadStatusInterceptor(future)});

        try {
            //根据proxyClass每次创建一个不同的代理对象返回
            return ReflectionUtil.newInstance(proxyClass);
        } finally {
            Enhancer.registerStaticCallbacks(proxyClass, null);
        }
    }
}

