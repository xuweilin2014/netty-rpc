package com.newlandframework.rpc.jmx;

import com.newlandframework.rpc.event.AbstractInvokeEvent;
import com.newlandframework.rpc.event.InvokeEventFacade;
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
        //acn.getAttributeType获取到的是事件类型的字符串，比如ModuleEvent.INVOKE_EVENT
        ModuleEvent event = Enum.valueOf(ModuleEvent.class, acn.getAttributeType());
        //acn.getOldValue获取到的是InvokeEventFacade类型的对象，然后根据事件的类型来取得对应的visitor
        ModuleMetricsVisitor visitor = ((InvokeEventFacade) acn.getOldValue()).fetchEvent(event).getVisitor();

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
                visitor.accumulateTimespan((Long) acn.getNewValue());
                break;
            case INVOKE_MAX_TIMESPAN_EVENT:
                visitor.setInvokeMaxTimespan((Long) acn.getNewValue());
                break;
            case INVOKE_MIN_TIMESPAN_EVENT:
                visitor.setInvokeMinTimespan((Long) acn.getNewValue());
                break;
            case INVOKE_FAIL_STACKTRACE_EVENT:
                //acn.getNewValue返回的是方法调用发生错误时抛出的异常error，通过此error，设置好
                //发生异常的时间，以及异常的详细堆栈信息
                visitor.setLastStackTrace((Exception) acn.getNewValue());
                break;
            default:
                break;
        }
    }
}

