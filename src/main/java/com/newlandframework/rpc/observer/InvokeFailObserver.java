package com.newlandframework.rpc.observer;

import com.newlandframework.rpc.event.InvokeEventFacade;
import com.newlandframework.rpc.event.ModuleEvent;
import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;

import java.util.Observable;

/**
 * 当某个方法被调用失败之后，此观察者中的update方法被回调，用来更新此方法调用的失败次数、方法调用最后一次失败的时间
 * 以及最后一次失败的堆栈明细这三个参数
 */
@Getter
@Setter
public class InvokeFailObserver extends AbstractInvokeObserver {
    private Throwable error;

    public InvokeFailObserver(InvokeEventFacade facade, ModuleMetricsVisitor visitor, Throwable error) {
        super(facade, visitor);
        this.error = error;
    }

    @Override
    public void update(Observable o, Object arg) {
        if (arg == ModuleEvent.INVOKE_FAIL_EVENT) {
            //更新方法调用失败的次数
            super.getFacade().fetchEvent(ModuleEvent.INVOKE_FAIL_EVENT)
                    .notify(super.getFacade(), null);
            //更新方法调用最后一次失败的时间以及最后一次失败的堆栈明细
            super.getFacade().fetchEvent(ModuleEvent.INVOKE_FAIL_STACKTRACE_EVENT)
                    .notify(super.getFacade(), error);
        }
    }
}