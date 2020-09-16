package com.xu.rpc.event;

import com.xu.rpc.jmx.MetricsVisitor;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;

public class InvokeTimeSpanEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeTimeSpanNumber = new AtomicLong(0L);

    public InvokeTimeSpanEvent() {
        super();
    }

    public InvokeTimeSpanEvent(MetricsVisitor visitor) {
        super(visitor);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        return new AttributeChangeNotification(this, sequenceInvokeTimeSpanNumber.incrementAndGet(), System.currentTimeMillis(),
                super.visitor.getClassName(), super.visitor.getClassName(), MonitorEvent.INVOKE_TIMESPAN_EVENT.toString(),
                oldValue, newValue);
    }
}

