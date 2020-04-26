package com.newlandframework.rpc.netty;

import com.newlandframework.rpc.core.Modular;
import com.newlandframework.rpc.core.ModuleInvoker;
import com.newlandframework.rpc.core.ModuleProvider;
import com.newlandframework.rpc.core.RpcSystemConfig;
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
    protected MessageRequest request = null;
    protected MessageResponse response = null;
    protected Map<String, Object> handlerMap = null;
    protected static final String METHOD_MAPPED_NAME = "invoke";
    protected boolean returnNotNull = true;
    protected long invokeTimespan;
    protected Modular modular = BeanFactoryUtils.getBean("filterChain");

    public AbstractMessageRecvInitializeTask(MessageRequest request, MessageResponse response, Map<String, Object> handlerMap) {
        this.request = request;
        this.response = response;
        this.handlerMap = handlerMap;
    }

    @Override
    public Boolean call() {
        try {
            acquire();
            response.setMessageId(request.getMessageId());
            injectInvoke();
            Object result = reflect(request);
            boolean isInvokeSucc = ((returnNotNull && result != null) || !returnNotNull);
            // 调用本地方法成功的话，就将结果信息封装到MessageResponse对象中
            if (isInvokeSucc) {
                response.setResult(result);
                response.setError("");
                response.setReturnNotNull(returnNotNull);
                injectSuccInvoke(invokeTimespan);
            } else {
                System.err.println(RpcSystemConfig.FILTER_RESPONSE_MSG);
                response.setResult(null);
                response.setError(RpcSystemConfig.FILTER_RESPONSE_MSG);
                injectFilterInvoke();
            }
            return Boolean.TRUE;
        } catch (Throwable t) {
            response.setError(getStackTrace(t));
            t.printStackTrace();
            System.err.printf("RPC Server invoke error!\n");
            injectFailInvoke(t);
            return Boolean.FALSE;
        } finally {
            release();
        }
    }

    private Object invoke(MethodInvoker mi, MessageRequest request) throws Throwable {
        if (modular != null) {
            ModuleProvider provider = modular.getProvider(new ModuleInvoker() {

                @Override
                public Class getInterface() {
                    return mi.getClass().getInterfaces()[0];
                }

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
        // 配置增强对象，也就是 MethodInterceptor 对象，当调用 MethodInvoker 中的 invoke 方法时，
        // 就会被此对象拦截，先调用 MethodProxyAdvisor 中的 invoke 方法
        // （说明一下，这里的invoke方法和 MethodInvoker中的invoke方法不同，假如MethodInvoker类中的目标方法为hello，并且METHOD_MAPPED_NAME为hello，
        // 那么当MethodInvoker调用hello方法时，也会先调用MethodProxyAdvisor中的invoke方法，也就是对hello方法进行增强，然后再调用MethodInvoker中的hello方法）
        advisor.setAdvice(new MethodProxyAdvisor(handlerMap));
        weaver.addAdvisor(advisor);

        // 返回SpringAOP创建的代理对象，这里是CglibAopProxy（因为MethodInvoker没有实现接口）
        MethodInvoker mi = (MethodInvoker) weaver.getProxy();
        Object obj = invoke(mi, request);
        invokeTimespan = mi.getInvokeTimespan();
        setReturnNotNull(((MethodProxyAdvisor) advisor.getAdvice()).isReturnNotNull());
        return obj;
    }

    public String getStackTrace(Throwable ex) {
        StringWriter buf = new StringWriter();
        ex.printStackTrace(new PrintWriter(buf));

        return buf.toString();
    }

    public boolean isReturnNotNull() {
        return returnNotNull;
    }

    public void setReturnNotNull(boolean returnNotNull) {
        this.returnNotNull = returnNotNull;
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

    protected abstract void acquire();

    protected abstract void release();
}

