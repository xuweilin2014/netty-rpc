package com.newlandframework.rpc.observer;

import com.newlandframework.rpc.event.InvokeEventFacade;
import com.newlandframework.rpc.jmx.MetricsVisitor;

import java.util.Observer;


public abstract class AbstractInvokeObserver implements Observer {

    private InvokeEventFacade facade;
    private MetricsVisitor visitor;

    public InvokeEventFacade getFacade() {
        return facade;
    }

    public void setFacade(InvokeEventFacade facade) {
        this.facade = facade;
    }

    public MetricsVisitor getVisitor() {
        return visitor;
    }

    public void setVisitor(MetricsVisitor visitor) {
        this.visitor = visitor;
    }

    public AbstractInvokeObserver(InvokeEventFacade facade, MetricsVisitor visitor) {
        this.facade = facade;
        this.visitor = visitor;
    }
}

