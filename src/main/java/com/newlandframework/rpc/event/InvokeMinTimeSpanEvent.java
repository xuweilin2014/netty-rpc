package com.newlandframework.rpc.event;

import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;

public class InvokeMinTimeSpanEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeMinTimeSpanNumber = new AtomicLong(0L);

    public InvokeMinTimeSpanEvent() {
        super();
    }

    public InvokeMinTimeSpanEvent(ModuleMetricsVisitor visitor) {
        super(visitor);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this, sequenceInvokeMinTimeSpanNumber.incrementAndGet(), System.currentTimeMillis(),
                super.visitor.getClassName(), super.visitor.getMethodName(), ModuleEvent.INVOKE_MIN_TIMESPAN_EVENT.toString(),
                oldValue, newValue);
    }
}
