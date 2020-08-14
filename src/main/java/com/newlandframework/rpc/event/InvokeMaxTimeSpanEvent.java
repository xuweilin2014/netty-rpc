package com.newlandframework.rpc.event;

import com.newlandframework.rpc.jmx.MetricsVisitor;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;


public class InvokeMaxTimeSpanEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeMaxTimeSpanNumber = new AtomicLong(0L);

    public InvokeMaxTimeSpanEvent() {
        super();
    }

    public InvokeMaxTimeSpanEvent(MetricsVisitor visitor) {
        super(visitor);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this, sequenceInvokeMaxTimeSpanNumber.incrementAndGet(), System.currentTimeMillis(),
                super.visitor.getClassName(), super.visitor.getMethodName(), ModuleEvent.INVOKE_MAX_TIMESPAN_EVENT.toString(),
                oldValue, newValue);
    }
}
