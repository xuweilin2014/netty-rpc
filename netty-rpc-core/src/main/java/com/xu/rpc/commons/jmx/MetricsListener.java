package com.xu.rpc.commons.jmx;

import com.xu.rpc.core.RpcConfig;

import java.util.Map;

public class MetricsListener {

    public static void handleNotification(MetricsVisitor visitor, MonitorEvent event, Map<String, Object> attachments) {

        long invokeTimespan = 0L;
        Throwable error = null;

        if (attachments != null){
            if (attachments.containsKey(RpcConfig.INVOKE_TIMESPAN_KEY)){
                invokeTimespan = (long) attachments.get(RpcConfig.INVOKE_TIMESPAN_KEY);
            }
            if (attachments.containsKey(RpcConfig.STACK_TRACE_KEY)){
                error = (Throwable) attachments.get(RpcConfig.STACK_TRACE_KEY);
            }
        }

        switch (event) {
            case INVOKE_EVENT:
                visitor.incrementInvokeCount();
                break;
            case INVOKE_SUCC_EVENT:
                visitor.incrementInvokeSuccCount();
                break;
            case INVOKE_FAIL_EVENT:
                visitor.incrementInvokeFailCount();
                break;
            case INVOKE_FILTER_EVENT:
                visitor.incrementInvokeFilterCount();
                break;
            case INVOKE_TIMESPAN_EVENT:
                visitor.accumulateTimespan(invokeTimespan);
                break;
            case INVOKE_MAX_TIMESPAN_EVENT:
                visitor.setInvokeMaxTimespan(invokeTimespan);
                break;
            case INVOKE_MIN_TIMESPAN_EVENT:
                visitor.setInvokeMinTimespan(invokeTimespan);
                break;
            case INVOKE_FAIL_STACKTRACE_EVENT:
                //acn.getNewValue返回的是方法调用发生错误时抛出的异常error，通过此error，设置好
                //发生异常的时间，以及异常的详细堆栈信息
                visitor.setLastStackTrace((Exception) error);
                break;
            default:
                break;
        }
    }
}

