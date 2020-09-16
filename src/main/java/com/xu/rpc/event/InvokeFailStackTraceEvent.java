package com.xu.rpc.event;

import com.xu.rpc.jmx.MetricsVisitor;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;

public class InvokeFailStackTraceEvent extends AbstractInvokeEvent {

    private AtomicLong sequenceInvokeFailStackTraceNumber = new AtomicLong(0L);

    public InvokeFailStackTraceEvent() {
        super();
    }

    public InvokeFailStackTraceEvent(MetricsVisitor visitor) {
        super(visitor);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this,
                sequenceInvokeFailStackTraceNumber.incrementAndGet(), System.currentTimeMillis(),
                super.visitor.getClassName(), super.visitor.getMethodName(), MonitorEvent.INVOKE_FAIL_STACKTRACE_EVENT.toString(),
                oldValue, newValue);
    }
}


