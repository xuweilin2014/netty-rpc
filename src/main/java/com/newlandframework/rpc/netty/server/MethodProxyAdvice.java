package com.newlandframework.rpc.netty.server;

import com.newlandframework.rpc.filter.Filter;
import com.newlandframework.rpc.filter.ServiceFilterBinder;
import com.newlandframework.rpc.model.MessageRequest;
import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.reflect.MethodUtils;

import java.lang.reflect.Method;
import java.util.Map;

/**
 * 用来对MethodInvoker类中的invoke方法进行增强，也就是增加一个是否需要调用拦截器Filter，
 * 来对客户端的RPC服务请求进行过滤
 */
public class MethodProxyAdvice implements MethodInterceptor {
    private Map<String, Object> handlerMap;
    private boolean returnNotNull = true;

    public boolean isReturnNotNull() {
        return returnNotNull;
    }

    public void setReturnNotNull(boolean returnNotNull) {
        this.returnNotNull = returnNotNull;
    }

    public MethodProxyAdvice(Map<String, Object> handlerMap) {
        this.handlerMap = handlerMap;
    }

    @Override
    public Object invoke(MethodInvocation invocation) throws Throwable {
        Object[] params = invocation.getArguments();
        if (params.length <= 0) {
            return null;
        }

        MessageRequest request = (MessageRequest) params[0];

        String className = request.getClassName();
        Object serviceBean = handlerMap.get(className);
        String methodName = request.getMethodName();
        Object[] parameters = request.getParametersVal();

        ServiceFilterBinder binder = (ServiceFilterBinder) serviceBean;
        ((MethodInvoker) invocation.getThis()).setServiceBean(binder.getObject());

        if (binder.getFilter() != null) {
            Filter filter = binder.getFilter();
            Object[] args = ArrayUtils.nullToEmpty(parameters);
            Class<?>[] parameterTypes = ClassUtils.toClass(args);
            Method method = MethodUtils.getMatchingAccessibleMethod(binder.getObject().getClass(), methodName, parameterTypes);
            //Filter接口定义的before方法是在对应RPC服务方法运行之前执行，所以如果before方法返回false，RPC服务端会拒绝执行对应的RPC服务方法。
            //如果before返回true，则会执行RPC服务方法，执行成功之后，再执行Filter定义的after方法。
            if (filter.before(method, binder.getObject(), parameters)) {
                Object result = invocation.proceed();
                filter.after(method, binder.getObject(), parameters);
                setReturnNotNull(result != null);
                return result;
            } else {
                return null;
            }
        }

        Object result = invocation.proceed();
        setReturnNotNull(result != null);
        return result;
    }
}


