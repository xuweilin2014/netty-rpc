package com.newlandframework.rpc.jmx;

import com.newlandframework.rpc.event.ModuleEvent;
import javax.management.AttributeChangeNotification;
import javax.management.JMException;
import javax.management.Notification;
import javax.management.NotificationListener;

public class ModuleMetricsListener implements NotificationListener {
    @Override
    public void handleNotification(Notification notification, Object handback) {
        if (!(notification instanceof AttributeChangeNotification)) {
            return;
        }

        AttributeChangeNotification acn = (AttributeChangeNotification) notification;
        // acn.getAttributeType获取到的是事件，比如ModuleEvent.INVOKE_EVENT
        ModuleEvent event = Enum.valueOf(ModuleEvent.class, acn.getAttributeType());
        // acn.getMessage获取到的是className，acn.getAttributeName获取到的是methodName，根据这两个来取得对应的visitor
        ModuleMetricsVisitor visitor = ModuleMetricsHandler.getInstance().getVisitor(acn.getMessage(), acn.getAttributeName());

        switch (event) {
            case INVOKE_EVENT:
                visitor.setInvokeCount((Long) acn.getNewValue());
                break;
            case INVOKE_SUCC_EVENT:
                visitor.setInvokeSuccCount((Long) acn.getNewValue());
                break;
            case INVOKE_FAIL_EVENT:
                visitor.setInvokeFailCount((Long) acn.getNewValue());
                break;
            case INVOKE_FILTER_EVENT:
                visitor.setInvokeFilterCount((Long) acn.getNewValue());
                break;
            case INVOKE_TIMESPAN_EVENT:
                visitor.setInvokeTimespan((Long) acn.getNewValue());
                break;
            case INVOKE_MAX_TIMESPAN_EVENT:
                if ((Long) acn.getNewValue() > (Long) acn.getOldValue()) {
                    visitor.setInvokeMaxTimespan((Long) acn.getNewValue());
                }
                break;
            case INVOKE_MIN_TIMESPAN_EVENT:
                if ((Long) acn.getNewValue() < (Long) acn.getOldValue()) {
                    visitor.setInvokeMinTimespan((Long) acn.getNewValue());
                }
                break;
            case INVOKE_FAIL_STACKTRACE_EVENT:
                try {
                    //acn.getNewValue返回的是方法调用发生错误时抛出的异常error，通过此error，设置好
                    //发生异常的时间，以及异常的详细堆栈信息
                    visitor.setLastStackTrace((Exception) acn.getNewValue());
                    visitor.buildErrorCompositeData((Exception) acn.getNewValue());
                } catch (JMException e) {
                    e.printStackTrace();
                }
                break;
            default:
                break;
        }
    }
}

