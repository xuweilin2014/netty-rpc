package com.newlandframework.rpc.observer;

import com.newlandframework.rpc.event.InvokeEventFacade;
import com.newlandframework.rpc.event.ModuleEvent;
import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;

import java.util.Observable;

/**
 * 当某个方法被调用成功之后，此观察者中的update方法被回调，用来更新此方法调用成功的次数、耗时、最大耗时、最小耗时这四个参数
 */
@Getter
@Setter
public class InvokeSuccObserver extends AbstractInvokeObserver {

    private long invokeTimespan;

    public InvokeSuccObserver(InvokeEventFacade facade, ModuleMetricsVisitor visitor, long invokeTimespan) {
        super(facade, visitor);
        this.invokeTimespan = invokeTimespan;
    }


    @Override
    public void update(Observable o, Object arg) {
        if (arg == ModuleEvent.INVOKE_SUCC_EVENT) {
            // 更新调用成功的次数
            super.getFacade().fetchEvent(ModuleEvent.INVOKE_SUCC_EVENT)
                    .notify(super.getFacade(), null);
            // 更新方法调用总的耗时，也就是此方法所有调用时间之和，这个数据被用来计算方法调用的平均时间
            super.getFacade().fetchEvent(ModuleEvent.INVOKE_TIMESPAN_EVENT)
                    .notify(super.getFacade(), invokeTimespan);
            // 更新方法调用最大耗时
            super.getFacade().fetchEvent(ModuleEvent.INVOKE_MAX_TIMESPAN_EVENT)
                    .notify(super.getFacade(), invokeTimespan);
            // 更新方法调用最小耗时
            super.getFacade().fetchEvent(ModuleEvent.INVOKE_MIN_TIMESPAN_EVENT)
                    .notify(super.getFacade(), invokeTimespan);
        }
    }
}
