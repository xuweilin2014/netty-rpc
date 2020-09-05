package com.xu.rpc.cluster.support.wrapper;

import com.xu.rpc.cluster.Directory;
import com.xu.rpc.commons.URL;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.proxy.JDKProxyFactory;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class MockClusterInvoker<T> implements Invoker<T> {

    private final Directory directory;

    private final Invoker invoker;

    private final URL url;

    private static final Map<String, Invoker<?>> mocks = new ConcurrentHashMap<>();

    public MockClusterInvoker(Directory directory, Invoker invoker) {
        this.directory = directory;
        this.invoker = invoker;
        this.url = directory.getUrl();
    }

    @Override
    public Class<?> getInterface() {
        return directory.getInterface();
    }

    /**
     * mock 的值有 5 种情况，一种是 true/false/force/fail/Mock类名
     *
     * 当 mock 的值为 false 或者为空的时候，表明不进行服务降级操作，就会直接返回调用 invoke 方法的调用结果
     * 当 mock 的值为 true 的时候，当远程调用发生错误的时候，就去调用同一路径下的 Mock 类，返回这个 Mock 类的结果
     * 当 mock 的值为 Mock 类名的时候，基本上也是相同的做法
     * 当 mock 的值为 force 的时候，相当于强制屏蔽，也就是不会走远程调用逻辑，直接返回 null 结果
     * 当 mock 的值为 fail 的时候，相当于容错，也就是当消费方对该服务调用失败之后，会返回一个 null 结果
     */
    @SuppressWarnings("unchecked")
    @Override
    public RpcResult invoke(RpcInvocation invocation) throws RpcException {
        String mock = directory.getUrl().getParameter(RpcConfig.MOCK_KEY, Boolean.FALSE.toString());
        RpcResult result;
        if (mock.length() == 0 || RpcConfig.FALSE.equalsIgnoreCase(mock)){
            result = invoker.invoke(invocation);
        } else if (RpcConfig.MOCK_FAIL_KEY.equalsIgnoreCase(mock)){
            try{
                result = invoker.invoke(invocation);
            }catch (RpcException e){
                result = new RpcResult();
                result.setResult(null);
            }
        } else if (RpcConfig.MOCK_FORCE_KEY.equalsIgnoreCase(mock)) {
            result = new RpcResult();
            result.setResult(null);
        } else {
            try {
                result = invoker.invoke(invocation);
            } catch (RpcException e) {
                if (RpcConfig.TRUE.equalsIgnoreCase(mock)){
                    mock = invocation.getServiceType().getName() + "Mock";
                }

                try {
                    Invoker<?> mockInvoker = mocks.get(mock);
                    if (mockInvoker == null){
                        Class<?> mockClass = Class.forName(mock);
                        if (!invocation.getServiceType().isAssignableFrom(mockClass)){
                            throw new IllegalStateException("mock class " + mockClass.getName() + " did not implement interface "
                                    + invocation.getServiceType().getName());
                        }

                        T mockObject = (T) mockClass.getConstructor().newInstance();
                        mockInvoker = JDKProxyFactory.getInvoker(mockObject, url);
                        mocks.put(mock, mockInvoker);
                    }

                    result = mockInvoker.invoke(invocation);
                } catch (ClassNotFoundException t) {
                    throw new RpcException("class " + mock + " not found, try to recheck it.");
                } catch (NoSuchMethodException t) {
                    throw new RpcException("no default constructor in class " + mock);
                }  catch (Throwable t) {
                    throw new RpcException("failed to invoke the method " + invocation.getMethodName() + " in mock class " +
                            invocation.getServiceType().getName() + ", caused by " + t.getMessage());
                }
            }
        }

        return result;
    }

    @Override
    public URL getUrl() {
        return directory.getUrl();
    }

    @Override
    public boolean isAvailable() {
        return directory.isAvailable();
    }

    @Override
    public void destroy() {
        invoker.destroy();
        directory.destroy();
    }
}
