package com.newlandframework.rpc.event;

import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;

import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import java.util.concurrent.atomic.AtomicLong;

/**
 * InvokeEvent
 */
public class InvokeEvent extends AbstractInvokeEvent {
    private AtomicLong sequenceInvokeNumber = new AtomicLong(0L);

    public InvokeEvent() {
        super();
    }

    public InvokeEvent(ModuleMetricsVisitor visitor) {
        super(visitor);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        // 参数列表中的各个参数与AttributeChangeNotification类中的属性对应关系如下：
        // className -> msg
        // methodName -> attributeName
        // ModuleEvent.INVOKE_EVENT.toString() -> attributeType
        return new AttributeChangeNotification(this, sequenceInvokeNumber.incrementAndGet(), System.currentTimeMillis(),
                super.visitor.getClassName(), super.visitor.getMethodName(), ModuleEvent.INVOKE_EVENT.toString(),
                oldValue, newValue);
    }
}

