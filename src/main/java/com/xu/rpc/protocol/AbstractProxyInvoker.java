package com.xu.rpc.protocol;

import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.commons.URL;
import org.apache.log4j.Logger;

// 真正执行本地方法的 Invoker
public abstract class AbstractProxyInvoker<T> implements Invoker<T>{

    private Object serviceBean;

    private static final Logger logger = Logger.getLogger(AbstractProxyInvoker.class);

    private URL url;

    private Class<?> type;

    public URL getUrl() {
        return url;
    }

    public void setUrl(URL url) {
        this.url = url;
    }

    public AbstractProxyInvoker(Object serviceBean, URL url, Class<?> type) {
        this.serviceBean = serviceBean;
        this.url = url;
        this.type = type;
    }

    public Object getServiceBean() {
        return serviceBean;
    }

    public void setServiceBean(Object serviceBean) {
        this.serviceBean = serviceBean;
    }

    @Override
    public RpcResult invoke(RpcInvocation invocation) throws RpcException {
        try{
            return new RpcResult(doInvoke(serviceBean, invocation.getMethodName(), invocation.getParameters()));
        } catch (Throwable e) {
            logger.error("failed to invoke remote method " + invocation.getMethodName() + " to url " +
                    url.toFullString() + " " + e.getMessage());
            return new RpcResult(e);
        }
    }

    @Override
    public void destroy() {
        // do nothing
    }

    @Override
    public Class<?> getInterface() {
        return type;
    }

    @Override
    public boolean isAvailable() {
        return true;
    }

    protected abstract Object doInvoke(Object serviceBean, String methodName, Object[] parametersVal) throws Throwable;
}
