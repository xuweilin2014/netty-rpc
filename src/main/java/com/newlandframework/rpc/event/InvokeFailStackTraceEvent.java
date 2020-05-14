package com.newlandframework.rpc.event;

import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;

public class InvokeFailStackTraceEvent extends AbstractInvokeEvent {

    private AtomicLong sequenceInvokeFailStackTraceNumber = new AtomicLong(0L);

    public InvokeFailStackTraceEvent() {
        super();
    }

    public InvokeFailStackTraceEvent(ModuleMetricsVisitor visitor) {
        super(visitor);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this,
                sequenceInvokeFailStackTraceNumber.incrementAndGet(), System.currentTimeMillis(),
                super.visitor.getClassName(), super.visitor.getMethodName(), ModuleEvent.INVOKE_FAIL_STACKTRACE_EVENT.toString(),
                oldValue, newValue);
    }
}


