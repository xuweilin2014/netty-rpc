package com.xu.rpc.filter.support;

import com.xu.rpc.commons.util.ReflectionUtils;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.extension.Activate;
import com.xu.rpc.filter.ChainFilter;
import com.xu.rpc.jmx.MetricsListener;
import com.xu.rpc.jmx.MetricsVisitor;
import com.xu.rpc.jmx.MetricsVisitorHandler;
import com.xu.rpc.jmx.MonitorEvent;
import com.xu.rpc.protocol.Invoker;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Activate(group = {RpcConfig.PROVIDER}, order = 4, value = RpcConfig.MONITOR_KEY)
public class MonitorChainFilter implements ChainFilter {

    private static final Map<String, Map<String, MetricsVisitor>> cachedVisitors = new ConcurrentHashMap<>();

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
    public RpcResult intercept(Invoker invoker, RpcInvocation invocation) {
        RpcResult result = invoker.invoke(invocation);

        if (invoker.getUrl().getParameter(RpcConfig.METRICS_KEY, true)
                && invoker.getUrl().getParameter(RpcConfig.MONITOR_KEY, true)){

            Class<?> cls = invocation.getServiceType();
            ReflectionUtils utils = new ReflectionUtils();
            MetricsVisitor visitor;
            try{
                // 通过反射获取客户端要调用的方法
                Method method = invocation.getMethod();
                // 通过反射获取方法的签名，保存到ReflectionUtils类的provider对象中
                utils.listMethod(method, false);
                // 获取客户端要调用的方法的方法签名的字符串
                String signatureMethod = utils.getProvider().toString().trim();
                String className = cls.getName().trim();
                Map<String, MetricsVisitor> visitors = cachedVisitors.get(className);
                if (visitors == null){
                    cachedVisitors.put(className, new ConcurrentHashMap<>());
                    visitors = cachedVisitors.get(className);
                }

                visitor = visitors.get(signatureMethod);

                if (visitor == null){
                    // 获取和此方法对应的ModuleMetricsVisitor对象，用来记录这个方法的调用情况，每一个特定的方法，都只和一个ModuleMetricsVisitor对象对应
                    visitor = MetricsVisitorHandler.getINSTANCE().getVisitor(className, signatureMethod);
                    visitors.put(signatureMethod, visitor);
                }
            } finally {
                utils.clearProvider();
            }

            // 增加方法的调用次数
            injectInvoke(visitor);
            if (result.getException() != null){
                // 修改方法调用的失败次数、方法调用失败的时间以及失败的堆栈明细
                injectFailInvoke(result.getException(), visitor);
            }else{
                // 增加方法调用成功的次数
                injectSuccInvoke(result.getInvokeTimespan(), visitor);
            }
        }

        return result;
    }

    protected void injectInvoke(MetricsVisitor visitor) {
        MetricsListener.handleNotification(visitor, MonitorEvent.INVOKE_EVENT, null);
    }

    protected void injectSuccInvoke(long invokeTimespan, MetricsVisitor visitor) {
        Map<String, Object> attachments = new HashMap<>();
        attachments.put(RpcConfig.INVOKE_TIMESPAN_KEY, invokeTimespan);
        MetricsListener.handleNotification(visitor, MonitorEvent.INVOKE_SUCC_EVENT, null);
        MetricsListener.handleNotification(visitor, MonitorEvent.INVOKE_TIMESPAN_EVENT, attachments);
        MetricsListener.handleNotification(visitor, MonitorEvent.INVOKE_MAX_TIMESPAN_EVENT, attachments);
        MetricsListener.handleNotification(visitor, MonitorEvent.INVOKE_MIN_TIMESPAN_EVENT, attachments);
    }

    // 当方法调用失败之后，就会调用此方法，作用是更新方法调用失败的次数、方法调用最后一次失败的时间以及最后一次失败的堆栈明细这三个参数。
    protected void injectFailInvoke(Throwable error, MetricsVisitor visitor) {
        Map<String, Object> attachments = new HashMap<>();
        attachments.put(RpcConfig.STACK_TRACE_KEY, error);
        MetricsListener.handleNotification(visitor, MonitorEvent.INVOKE_FAIL_EVENT, null);
        MetricsListener.handleNotification(visitor, MonitorEvent.INVOKE_FAIL_STACKTRACE_EVENT, attachments);
    }
}
