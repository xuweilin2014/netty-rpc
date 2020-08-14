package com.newlandframework.rpc.event;

import com.newlandframework.rpc.jmx.MetricsVisitor;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;

public class InvokeFilterEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeFilterNumber = new AtomicLong(0L);

    public InvokeFilterEvent() {
        super();
    }

    public InvokeFilterEvent(MetricsVisitor visitor) {
        super(visitor);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this, sequenceInvokeFilterNumber.incrementAndGet(), System.currentTimeMillis(),
                super.visitor.getClassName(), super.visitor.getMethodName(), ModuleEvent.INVOKE_FILTER_EVENT.toString(),
                oldValue, newValue);
    }
}
