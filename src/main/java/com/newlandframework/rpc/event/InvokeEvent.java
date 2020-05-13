package com.newlandframework.rpc.event;

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

    public InvokeEvent(String className, String methodName) {
        super(className, methodName);
    }

    @Override
    public Notification buildNotification(Object oldValue, Object newValue) {
        // 参数列表中的各个参数与AttributeChangeNotification类中的属性对应关系如下：
        // className -> msg
        // methodName -> attributeName
        // ModuleEvent.INVOKE_EVENT -> attributeType
        return new AttributeChangeNotification(this, sequenceInvokeNumber.incrementAndGet(), System.currentTimeMillis(),
                super.className, super.methodName, ModuleEvent.INVOKE_EVENT.toString(), oldValue, newValue);
    }
}

