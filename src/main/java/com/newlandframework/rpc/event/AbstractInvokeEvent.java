package com.newlandframework.rpc.event;

import com.newlandframework.rpc.jmx.ModuleMetricsHandler;
import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;
import lombok.Data;
import javax.management.Notification;

@Data
public abstract class AbstractInvokeEvent {

    protected ModuleMetricsVisitor visitor;
    protected ModuleMetricsHandler handler;
    protected InvokeEventFacade facade;

    public AbstractInvokeEvent() {
    }

    public AbstractInvokeEvent(ModuleMetricsVisitor visitor) {
        this.visitor = visitor;
    }

    public abstract Notification buildNotification(Object oldValue, Object newValue);

    public void notify(Object oldValue, Object newValue) {
        //buildNotification方法会根据不同的子类来创建不同的notification
        Notification notification = buildNotification(oldValue, newValue);
        //这个notification最终会被发送到ModuleMetricsListener中
        handler.sendNotification(notification);
    }
}
