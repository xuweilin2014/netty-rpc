package com.newlandframework.rpc.event;

import com.newlandframework.rpc.jmx.MetricsVisitor;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;


public class InvokeSuccEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeSuccNumber = new AtomicLong(0L);

    public InvokeSuccEvent() {
        super();
    }

    public InvokeSuccEvent(MetricsVisitor visitor) {
        super(visitor);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this, sequenceInvokeSuccNumber.incrementAndGet(), System.currentTimeMillis(),
                super.visitor.getClassName(), super.visitor.getMethodName(), ModuleEvent.INVOKE_SUCC_EVENT.toString(),
                oldValue, newValue);
    }
}

