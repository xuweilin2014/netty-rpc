package com.newlandframework.rpc.netty.server;

import com.newlandframework.rpc.model.MessageRequest;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.apache.commons.lang3.time.StopWatch;

/**
 * MethodInvoker#invoke方法通过反射来真正执行客户端要求调用的方法
 */
public class MethodInvoker {
    private Object serviceBean;
    private StopWatch sw = new StopWatch();

    public Object getServiceBean() {
        return serviceBean;
    }

    public void setServiceBean(Object serviceBean) {
        this.serviceBean = serviceBean;
    }

    public Object invoke(MessageRequest request) throws Throwable {
        String methodName = request.getMethodName();
        Object[] parameters = request.getParametersVal();
        sw.reset();
        sw.start();
        Object result = MethodUtils.invokeMethod(serviceBean, methodName, parameters);
        sw.stop();
        return result;
    }

    public long getInvokeTimespan() {
        return sw.getTime();
    }
}

