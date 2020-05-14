package com.newlandframework.rpc.observer;

import com.newlandframework.rpc.event.InvokeEventFacade;
import com.newlandframework.rpc.event.ModuleEvent;
import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;

import java.util.Observable;

/**
 * 当某个方法被调用时被过滤器过滤了，此观察者中的update方法被回调，用来更新此方法被过滤的次数
 */
public class InvokeFilterObserver extends AbstractInvokeObserver {

    public InvokeFilterObserver(InvokeEventFacade facade, ModuleMetricsVisitor visitor) {
        super(facade, visitor);
    }

    @Override
    public void update(Observable o, Object arg) {
        if (arg == ModuleEvent.INVOKE_FILTER_EVENT) {
            // 更新方法被过滤的次数
            super.getFacade().fetchEvent(ModuleEvent.INVOKE_FILTER_EVENT)
                    .notify(super.getFacade(),
                            null);
        }
    }
}
