package com.newlandframework.rpc.event;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;

public class InvokeMinTimeSpanEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeMinTimeSpanNumber = new AtomicLong(0L);

    public InvokeMinTimeSpanEvent() {
        super();
    }

    public InvokeMinTimeSpanEvent(String moduleName, String methodName) {
        super(moduleName, methodName);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this, sequenceInvokeMinTimeSpanNumber.incrementAndGet(), System.currentTimeMillis(),
                super.className, super.methodName, ModuleEvent.INVOKE_MIN_TIMESPAN_EVENT.toString(), oldValue, newValue);
    }
}
