package com.xu.rpc.cluster.support.wrapper;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.proxy.JDKProxyFactory;
import org.apache.commons.lang3.StringUtils;

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
        RpcResult result = null;
        if (StringUtils.isEmpty(mock) || RpcConfig.FALSE.equalsIgnoreCase(mock)){
            result = invoker.invoke(invocation);
        // 当 mock 为 fail 时，调用失败之后，直接返回一个 null 结果
        } else if (RpcConfig.MOCK_FAIL_KEY.equalsIgnoreCase(mock)){
            try{
                // 正常返回的 result 也有可能是包含了异常，也就是调用发生了错误
                result = invoker.invoke(invocation);
                if (result.getException() != null){
                    result.setResult(null);
                    // 把 exception 置为 null，因此 ProxyWrapper 就不会把异常抛出
                    result.setException(null);
                }
            }catch (Throwable e){
                result = new RpcResult();
                result.setResult(null);
            }
        // 当 mock 为 force 时，直接返回 null 结果，不会进行远程调用
        } else if (RpcConfig.MOCK_FORCE_KEY.equalsIgnoreCase(mock)) {
            result = new RpcResult();
            result.setResult(null);
        // 当 mock 为 true 或者类名时
        } else {
            Throwable throwable;
            try {
                result = invoker.invoke(invocation);
                throwable = result.getException();
            } catch (Throwable e) {
                throwable = e;
            }

            if (throwable != null){
                // 调用失败之后，调用用户自己定义的 Mock 类
                if (RpcConfig.TRUE.equalsIgnoreCase(mock)){
                    mock = invocation.getServiceType().getName() + "Mock";
                }

                try {
                    Invoker<?> mockInvoker = mocks.get(mock);
                    if (mockInvoker == null){
                        Class<?> mockClass = Thread.currentThread().getContextClassLoader().loadClass(mock);
                        if (!invocation.getServiceType().isAssignableFrom(mockClass)){
                            throw new IllegalStateException("mock class " + mockClass.getName() + " did not implement interface "
                                    + invocation.getServiceType().getName());
                        }

                        T mockObject = (T) mockClass.getConstructor().newInstance();
                        mockInvoker = JDKProxyFactory.getInvoker(mockObject, url, getInterface());
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
