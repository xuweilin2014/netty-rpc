package com.newlandframework.rpc.event;

import com.newlandframework.rpc.jmx.ModuleMetricsHandler;
import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;

import java.util.EnumMap;
import java.util.Map;


public class InvokeEventFacade {
    private static Map<ModuleEvent, AbstractInvokeEvent> enumMap = new EnumMap<>(ModuleEvent.class);

    static {
        enumMap.put(ModuleEvent.INVOKE_EVENT, new InvokeEvent());
        enumMap.put(ModuleEvent.INVOKE_SUCC_EVENT, new InvokeSuccEvent());
        enumMap.put(ModuleEvent.INVOKE_FAIL_EVENT, new InvokeFailEvent());
        enumMap.put(ModuleEvent.INVOKE_FILTER_EVENT, new InvokeFilterEvent());
        enumMap.put(ModuleEvent.INVOKE_TIMESPAN_EVENT, new InvokeTimeSpanEvent());
        enumMap.put(ModuleEvent.INVOKE_MAX_TIMESPAN_EVENT, new InvokeMaxTimeSpanEvent());
        enumMap.put(ModuleEvent.INVOKE_MIN_TIMESPAN_EVENT, new InvokeMinTimeSpanEvent());
        enumMap.put(ModuleEvent.INVOKE_FAIL_STACKTRACE_EVENT, new InvokeFailStackTraceEvent());
    }

    public InvokeEventFacade(ModuleMetricsHandler handler, ModuleMetricsVisitor visitor) {
        for (AbstractInvokeEvent event : enumMap.values()) {
            event.setHandler(handler);
            event.setVisitor(visitor);
            event.setFacade(this);
        }
    }

    public AbstractInvokeEvent fetchEvent(ModuleEvent event) {
        if (enumMap.containsKey(event)) {
            return enumMap.get(event);
        } else {
            return null;
        }
    }
}

