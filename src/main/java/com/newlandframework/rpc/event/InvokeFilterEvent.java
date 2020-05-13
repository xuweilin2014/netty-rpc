package com.newlandframework.rpc.event;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;

public class InvokeFilterEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeFilterNumber = new AtomicLong(0L);

    public InvokeFilterEvent() {
        super();
    }

    public InvokeFilterEvent(String moduleName, String methodName) {
        super(moduleName, methodName);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this, sequenceInvokeFilterNumber.incrementAndGet(), System.currentTimeMillis(),
                super.className, super.methodName, ModuleEvent.INVOKE_FILTER_EVENT.toString(), oldValue, newValue);
    }
}
