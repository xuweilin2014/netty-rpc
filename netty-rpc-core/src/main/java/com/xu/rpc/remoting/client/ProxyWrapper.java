package com.xu.rpc.remoting.client;

import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.protocol.Invoker;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class ProxyWrapper implements InvocationHandler {

    private Invoker<?> invoker;

    public <T> ProxyWrapper(Invoker<T> invoker){
        this.invoker = invoker;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        String methodName = method.getName();
        if (method.getDeclaringClass() == Object.class){
            return method.invoke(proxy, args);
        }

        if ("toString".equals(methodName)){
            return invoker.toString();
        }
        if ("hashcode".equals(methodName)){
            return invoker.hashCode();
        }
        if ("equals".equals(methodName)){
            return invoker.equals(args[0]);
        }

        RpcInvocation invocation = new RpcInvocation(method, args);
        // 如果客户端配置要求异步调用，则设置保存到 RpcInvocation 中
        boolean async = invoker.getUrl().getParameter(RpcConfig.ASYNC_KEY, false);
        invocation.getAttachments().put(RpcConfig.ASYNC_KEY, async ? RpcConfig.TRUE : RpcConfig.FALSE);
        // 如果要调用的方法没有返回值的话，则设置 oneWay 属性为 true，保存在 RpcInvocation 之中
        boolean oneWay = method.getReturnType() == void.class;
        invocation.getAttachments().put(RpcConfig.ONE_WAY_KEY, oneWay ? RpcConfig.TRUE : RpcConfig.FALSE);

        RpcResult result = invoker.invoke(invocation);
        if (result == null)
            throw new RpcException("failed o invoke the method " + methodName);
        // 如果 result 中的 RpcException 属性不为 null，表明调用发生了异常，抛出异常
        if (result.getException() != null)
            throw result.getException();

        // 如果要调用的方法没有返回值，直接返回 null
        if (oneWay){
            return null;
        }

        if (result.getResult() == null) {
            // 如果方法的返回值为原始类型（int,boolean等），那么就不能直接返回 null，否则会报错
            if (boolean.class == method.getReturnType())
                return false;
            else if (method.getReturnType().isPrimitive())
                return 0;
        }

        return result.getResult();
    }

}
