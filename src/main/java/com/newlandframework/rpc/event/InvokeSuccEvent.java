package com.newlandframework.rpc.event;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;


public class InvokeSuccEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeSuccNumber = new AtomicLong(0L);

    public InvokeSuccEvent() {
        super();
    }

    public InvokeSuccEvent(String moduleName, String methodName) {
        super(moduleName, methodName);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this, sequenceInvokeSuccNumber.incrementAndGet(), System.currentTimeMillis(),
                super.className, super.methodName, ModuleEvent.INVOKE_SUCC_EVENT.toString(), oldValue, newValue);
    }
}

