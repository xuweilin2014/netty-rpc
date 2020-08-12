package com.newlandframework.rpc.protocol;

import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.util.URL;
import org.apache.commons.lang3.time.StopWatch;

// 真正执行本地方法的 Invoker
public abstract class AbstractProxyInvoker implements Invoker{

    private Object serviceBean;
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

    private StopWatch sw = new StopWatch();

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

    // 获取方法执行消耗的时间
    public long getInvokeTimespan() {
        return sw.getTime();
    }

    @Override
    public Object invoke(MessageRequest request) throws RpcException {
        sw.reset();
        sw.start();
        try{
            return doInvoke(serviceBean, request.getMethodName(), request.getParametersVal());
        } catch (Throwable e) {
            throw new RpcException("failed to invoke remote method " + request.getMethodName() + " to url " +
                    url.toFullString() + " " + e.getMessage(), e);
        }finally {
            sw.stop();
        }
    }

    protected abstract Object doInvoke(Object serviceBean, String methodName, Object[] parametersVal) throws Throwable;
}
