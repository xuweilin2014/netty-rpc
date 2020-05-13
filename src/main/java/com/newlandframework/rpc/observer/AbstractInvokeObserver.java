package com.newlandframework.rpc.observer;

import com.newlandframework.rpc.event.InvokeEventFacade;
import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;

import java.util.Observer;

@Getter
@Setter
public abstract class AbstractInvokeObserver implements Observer {
    private InvokeEventFacade facade;
    private ModuleMetricsVisitor visitor;

    public AbstractInvokeObserver(InvokeEventFacade facade, ModuleMetricsVisitor visitor) {
        this.facade = facade;
        this.visitor = visitor;
    }
}

