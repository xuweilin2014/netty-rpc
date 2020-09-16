package com.xu.rpc.event;

import com.xu.rpc.jmx.MetricsVisitor;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;

public class InvokeFailEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeFailNumber = new AtomicLong(0L);

    public InvokeFailEvent() {
        super();
    }

    public InvokeFailEvent(MetricsVisitor visitor) {
        super(visitor);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this, sequenceInvokeFailNumber.incrementAndGet(), System.currentTimeMillis(),
                super.visitor.getClassName(), super.visitor.getMethodName(), MonitorEvent.INVOKE_FAIL_EVENT.toString(),
                oldValue, newValue);
    }
}
