package com.xu.rpc.observer;

import com.xu.rpc.event.AbstractInvokeEvent;
import com.xu.rpc.event.InvokeEventFacade;
import com.xu.rpc.event.MonitorEvent;
import com.xu.rpc.jmx.MetricsVisitor;

import java.util.Observable;

/**
 * 使用了观察者模式，InvokeObserver是一个观察者，而被观察者是InvokeEventTarget，InvokeObserver对象
 * 在MessageRecvInitializeTask类的injectInvoke方法中被注册到target对象上（也就是InvokeEventTarget，即被观察者）。
 * 接着target调用notify方法，回调所有注册到其上的观察者（除了InvokeObserver类型外，还有可能其它类型的观察者）。最终调用到
 * 这些观察者的update方法，对于每一个观察者而言，它都只能被特定的事件触发，在InvokeObserver中就是INVOKE_EVENT。
 *
 * fetchEvent会返回InvokeEventFacade中保存的InvokeEvent对象，然后调用此对象的notify方法。调用链如下：
 * InvokeObserver#update --> AbstractInvokeEvent#notify --> ModuleMetricsListener#handleNotification --> ModuleMetricsVisitor#setInvokeCount
 * 最后setInvokeCount来设置某个方法被调用的特定次数
 */
public class InvokeObserver extends AbstractInvokeObserver {

    public InvokeObserver(InvokeEventFacade facade, MetricsVisitor visitor,MonitorEvent event) {
        super(facade, visitor, event);
    }

    @Override
    public void doUpdate() {
        AbstractInvokeEvent event = super.getFacade().fetchEvent(MonitorEvent.INVOKE_EVENT);
        event.notify(super.getFacade(), null);
    }
}

