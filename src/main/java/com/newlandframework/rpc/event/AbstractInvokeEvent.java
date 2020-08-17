package com.newlandframework.rpc.event;

import com.newlandframework.rpc.jmx.MetricsServer;
import com.newlandframework.rpc.jmx.MetricsVisitor;
import com.newlandframework.rpc.jmx.MetricsVisitorHandler;

import javax.management.Notification;


public abstract class AbstractInvokeEvent {

    protected MetricsVisitor visitor;
    protected MetricsVisitorHandler handler;
    protected InvokeEventFacade facade;

    public AbstractInvokeEvent() {
    }

    public AbstractInvokeEvent(MetricsVisitor visitor) {
        this.visitor = visitor;
    }

    public abstract Notification buildNotification(Object oldValue, Object newValue);

    public void notify(Object oldValue, Object newValue) {
        //buildNotification方法会根据不同的子类来创建不同的notification
        Notification notification = buildNotification(oldValue, newValue);
        //这个notification最终会被发送到ModuleMetricsListener中
        handler.sendNotification(notification);
    }

    public MetricsVisitor getVisitor() {
        return visitor;
    }

    public void setVisitor(MetricsVisitor visitor) {
        this.visitor = visitor;
    }

    public MetricsVisitorHandler getHandler() {
        return handler;
    }

    public void setHandler(MetricsVisitorHandler handler) {
        this.handler = handler;
    }

    public InvokeEventFacade getFacade() {
        return facade;
    }

    public void setFacade(InvokeEventFacade facade) {
        this.facade = facade;
    }
}

