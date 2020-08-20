package com.xu.rpc.protocol;

import com.xu.rpc.core.RpcResult;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.util.URL;
import org.apache.log4j.Logger;

// 真正执行本地方法的 Invoker
public abstract class AbstractProxyInvoker implements Invoker{

    private Object serviceBean;

    private static final Logger logger = Logger.getLogger(AbstractProxyInvoker.class);

    private URL url;

    public URL getUrl() {
        return url;
    }

    public void setUrl(URL url) {
        this.url = url;
    }

    @Override
    public URL getURL() {
        return url;
    }

    public AbstractProxyInvoker(Object serviceBean, URL url) {
        this.serviceBean = serviceBean;
        this.url = url;
    }

    public Object getServiceBean() {
        return serviceBean;
    }

    public void setServiceBean(Object serviceBean) {
        this.serviceBean = serviceBean;
    }

    @Override
    public Object invoke(MessageRequest request) {
        try{
             return new RpcResult(doInvoke(serviceBean, request.getMethodName(), request.getParametersVal()));
        } catch (Throwable e) {
            logger.error("failed to invoke remote method " + request.getMethodName() + " to url " +
                    url.toFullString() + " " + e.getMessage());
            return new RpcResult(e);
        }
    }

    @Override
    public void destroy() {
        // TODO: 2020/8/19  
    }

    @Override
    public Class<?> getInterface() {
        // TODO: 2020/8/19  
        return null;
    }

    @Override
    public boolean isAvailable() {
        // TODO: 2020/8/19  
        return false;
    }

    protected abstract Object doInvoke(Object serviceBean, String methodName, Object[] parametersVal) throws Throwable;
}
