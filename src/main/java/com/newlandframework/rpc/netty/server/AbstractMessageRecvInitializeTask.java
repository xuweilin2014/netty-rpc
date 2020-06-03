package com.newlandframework.rpc.netty.server;

import com.newlandframework.rpc.core.ChainFilterInvoker;
import com.newlandframework.rpc.core.ChainFilterInvokerProvider;
import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.filter.FilterChainBuilder;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;
import com.newlandframework.rpc.spring.BeanFactoryUtils;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.aop.support.NameMatchMethodPointcutAdvisor;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Map;
import java.util.concurrent.Callable;


public abstract class AbstractMessageRecvInitializeTask implements Callable<Boolean> {

    protected MessageRequest request;

    protected MessageResponse response;

    protected Map<String, Object> handlerMap;

    protected static final String METHOD_MAPPED_NAME = "invoke";

    protected MethodInvokeStatus invokeStatus = MethodInvokeStatus.INIT;

    protected long invokeTimespan;

    protected FilterChainBuilder chainBuilder = BeanFactoryUtils.getBean("filterChain");

    public AbstractMessageRecvInitializeTask(MessageRequest request, MessageResponse response, Map<String, Object> handlerMap) {
        this.request = request;
        this.response = response;
        this.handlerMap = handlerMap;
    }

    /**
     * 每一个客户端发起的一次Rpc请求，都会在服务器端将其包装成一个Task，然后放到线程池中去执行。这些Task的类型是MessageRecvInitializeTask
     * （开启了JMX监控）或者MessageRecvInitializeTaskAdapter（没有开启JMX）这两类，他们都实现了Callable接口。每个Task任务的执行流程如下：
     * 1.调用injectInvoke方法，增加方法的调用次数，不过如果没有开启JMX，也就是task是MessageRecvInitializeTaskAdapter的话，这个方法是个空方法，
     * 2.调用reflect来执行客户端要调用的方法，并且获取到执行的结果。
     * 3.如果调用成功的话，就会修改方法调用成功的次数、累积耗时、最大耗时、最小耗时
     * 4.如果方法调用被拦截，就会修改方法被拦截的次数
     * 5.如果方法调用时抛出异常，就会修改方法调用的失败次数、方法调用失败的时间以及失败的堆栈明细
     */
    @Override
    public Boolean call() {
        try {
            response.setMessageId(request.getMessageId());
            //增加方法的调用次数
            injectInvoke();
            Object result = reflect(request);

            // 调用本地方法成功的话，就将结果信息封装到MessageResponse对象中
            if (invokeStatus.isDone()) {
                response.setResult(result);
                response.setError("");
                // 调用本地方法成功的话，就将结果信息封装到MessageResponse对象中
                injectSuccInvoke(invokeTimespan);
            } else if (invokeStatus.isRejected()){
                System.err.println(RpcSystemConfig.FILTER_RESPONSE_MSG);
                response.setResult(null);
                response.setError(RpcSystemConfig.FILTER_RESPONSE_MSG);
                //修改方法被拦截的次数
                injectFilterInvoke();
            }
            return Boolean.TRUE;
        } catch (Throwable t) {
            //如果通过反射调用方法的过程中发生了异常，并且这个异常没有被捕获的话，就会在此被捕获，
            //并且设置到MessageResponse的error属性中，并且打印出来。
            response.setError(getStackTrace(t));
            t.printStackTrace();
            System.err.printf("Rpc Server invoke error!\n");
            //修改方法调用的失败次数、方法调用失败的时间以及失败的堆栈明细
            injectFailInvoke(t);
            return Boolean.FALSE;
        } finally {
            response.setInvokeStatus(invokeStatus);
        }
    }

    /**
     * i.如果没有配置过滤器链的话，就直接通过反射调用客户端要求的方法，并且返回结果
     * ii.如果配置了过滤器链的话，就会先执行过滤器链中多个过滤器的intercept方法，对客户端的调用请求进行一些处理，然后再反射调用客户端要求的方法
     */
    private Object invoke(MethodInvoker mi, MessageRequest request) throws Throwable {
        if (chainBuilder != null) {
            ChainFilterInvokerProvider provider = chainBuilder.buildChain(new ChainFilterInvoker() {

                @Override
                public Class getInterface() {
                    return mi.getClass().getInterfaces()[0];
                }

                //ChainFilterInvoker对象一般是ChanFilterInvoker链中的最后一个，用来真正执行客户端要求调用的方法
                @Override
                public Object invoke(MessageRequest request) throws Throwable {
                    return mi.invoke(request);
                }

                @Override
                public void destroy() {
                }

            }, request);
            return provider.getInvoker().invoke(request);
        } else {
            return mi.invoke(request);
        }
    }

    public MethodInvokeStatus getInvokeStatus() {
        return invokeStatus;
    }

    public void setInvokeStatus(MethodInvokeStatus invokeStatus) {
        this.invokeStatus = invokeStatus;
    }

    /**
     * 使用Spring AOP的代理功能，来对真正执行客户端要求调用的方法进行增强。增强的目的是考虑到过滤器对请求进行一些处理
     */
    private Object reflect(MessageRequest request) throws Throwable {
        // 创建一个 ProxyFactory 对象，并且设置目标对象为 MethodInvoker
        ProxyFactory weaver = new ProxyFactory(new MethodInvoker());
        // Spring AOP中有两个PointcutAdvisor：RegexpMethodPointcutAdvisor和 NameMatchMethodPointcutAdvisor，
        // 它们都在org.springframework.aop.support包中。它们都可以过滤要拦截的方法，即对目标方法进行增强，配置方法也大致相同，其中一个最主要的区别：
        // RegexpMethodPointcutAdvisor：需要加上完整的类名和方法名
        // NameMatchMethodPointcutAdvisor：只需要方法名
        NameMatchMethodPointcutAdvisor advisor = new NameMatchMethodPointcutAdvisor();

        // METHOD_MAPPED_NAME的值为字符串 invoke，配置此 Advisor 要进行增强的目标方法的名字，
        // 在这里是 MethodInvoker 中的 invoke 方法
        advisor.setMappedName(METHOD_MAPPED_NAME);
        // 配置增强对象，也就是实现了 MethodInterceptor 接口的对象，在这里是MethodProxyAdvice对象，当调用 MethodInvoker 中的 invoke 方法时，
        // 就会被拦截，先调用 MethodProxyAdvice 中的 invoke 方法。
        // （说明一下，这里MethodProxyAdvisor 中的invoke方法和 MethodInvoker中的invoke方法不同，假如MethodInvoker类中的目标方法为hello，并且METHOD_MAPPED_NAME为hello，
        // 那么当MethodInvoker调用hello方法时，也会先调用MethodProxyAdvisor中的invoke方法，也就是对hello方法进行增强，然后再调用MethodInvoker中的hello方法）
        advisor.setAdvice(new MethodProxyAdvice(handlerMap));
        weaver.addAdvisor(advisor);

        // 返回SpringAOP创建的代理对象，这里是CglibAopProxy（因为MethodInvoker没有实现接口）
        MethodInvoker mi = (MethodInvoker) weaver.getProxy();
        Object obj = invoke(mi, request);
        invokeTimespan = mi.getInvokeTimespan();

        //setReturnNotNull(((MethodProxyAdvice) advisor.getAdvice()).isReturnNotNull());
        setInvokeStatus(((MethodProxyAdvice) advisor.getAdvice()).getInvokeStatus());

        return obj;
    }

    public String getStackTrace(Throwable ex) {
        StringWriter buf = new StringWriter();
        ex.printStackTrace(new PrintWriter(buf));

        return buf.toString();
    }

    public MessageResponse getResponse() {
        return response;
    }

    public MessageRequest getRequest() {
        return request;
    }

    public void setRequest(MessageRequest request) {
        this.request = request;
    }

    protected abstract void injectInvoke();

    protected abstract void injectSuccInvoke(long invokeTimespan);

    protected abstract void injectFailInvoke(Throwable error);

    protected abstract void injectFilterInvoke();
}

