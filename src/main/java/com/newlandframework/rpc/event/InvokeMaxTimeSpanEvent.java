package com.newlandframework.rpc.event;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;


public class InvokeMaxTimeSpanEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeMaxTimeSpanNumber = new AtomicLong(0L);

    public InvokeMaxTimeSpanEvent() {
        super();
    }

    public InvokeMaxTimeSpanEvent(String moduleName, String methodName) {
        super(moduleName, methodName);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this, sequenceInvokeMaxTimeSpanNumber.incrementAndGet(), System.currentTimeMillis(),
                super.className, super.methodName, ModuleEvent.INVOKE_MAX_TIMESPAN_EVENT.toString(), oldValue, newValue);
    }
}
