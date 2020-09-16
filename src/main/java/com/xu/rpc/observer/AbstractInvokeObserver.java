package com.xu.rpc.observer;

import com.xu.rpc.event.InvokeEventFacade;
import com.xu.rpc.event.MonitorEvent;
import com.xu.rpc.event.MonitorNotification;
import com.xu.rpc.jmx.MetricsVisitor;
import lombok.Getter;
import lombok.Setter;

import java.util.Observable;
import java.util.Observer;

@Getter
@Setter
public abstract class AbstractInvokeObserver implements Observer {

    private InvokeEventFacade facade;

    private MetricsVisitor visitor;

    private MonitorEvent event;

    public AbstractInvokeObserver(InvokeEventFacade facade, MetricsVisitor visitor, MonitorEvent event) {
        this.facade = facade;
        this.visitor = visitor;
        this.event = event;
    }

    @Override
    public void update(Observable o, Object arg) {
        if (arg instanceof MonitorNotification){
            MonitorNotification mn = (MonitorNotification) arg;
            if (mn.getEvent() == event && mn.getVisitor() == visitor){
                doUpdate();
            }
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AbstractInvokeObserver){
            AbstractInvokeObserver aio = (AbstractInvokeObserver) obj;
            return this.visitor == aio.visitor;
        }
        return false;
    }

    protected abstract void doUpdate();
}

