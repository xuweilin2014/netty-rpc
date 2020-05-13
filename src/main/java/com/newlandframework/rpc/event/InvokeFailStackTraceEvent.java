package com.newlandframework.rpc.event;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;

public class InvokeFailStackTraceEvent extends AbstractInvokeEvent {

    private AtomicLong sequenceInvokeFailStackTraceNumber = new AtomicLong(0L);

    public InvokeFailStackTraceEvent() {
        super();
    }

    public InvokeFailStackTraceEvent(String className, String methodName) {
        super(className, methodName);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this,
                sequenceInvokeFailStackTraceNumber.incrementAndGet(), System.currentTimeMillis(),
                super.className, super.methodName, ModuleEvent.INVOKE_FAIL_STACKTRACE_EVENT.toString(), oldValue, newValue);
    }
}


