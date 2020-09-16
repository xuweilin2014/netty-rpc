package com.xu.rpc.event;

import com.xu.rpc.jmx.MetricsVisitor;
import com.xu.rpc.jmx.MetricsVisitorHandler;

import java.util.EnumMap;
import java.util.Map;


public class InvokeEventFacade {

    private static Map<MonitorEvent, AbstractInvokeEvent> enumMap = new EnumMap<>(MonitorEvent.class);

    static {
        enumMap.put(MonitorEvent.INVOKE_EVENT, new InvokeEvent());
        enumMap.put(MonitorEvent.INVOKE_SUCC_EVENT, new InvokeSuccEvent());
        enumMap.put(MonitorEvent.INVOKE_FAIL_EVENT, new InvokeFailEvent());
        enumMap.put(MonitorEvent.INVOKE_TIMESPAN_EVENT, new InvokeTimeSpanEvent());
        enumMap.put(MonitorEvent.INVOKE_MAX_TIMESPAN_EVENT, new InvokeMaxTimeSpanEvent());
        enumMap.put(MonitorEvent.INVOKE_MIN_TIMESPAN_EVENT, new InvokeMinTimeSpanEvent());
        enumMap.put(MonitorEvent.INVOKE_FAIL_STACKTRACE_EVENT, new InvokeFailStackTraceEvent());
    }

    public InvokeEventFacade(MetricsVisitorHandler handler, MetricsVisitor visitor) {
        for (AbstractInvokeEvent event : enumMap.values()) {
            event.setHandler(handler);
            event.setVisitor(visitor);
            event.setFacade(this);
        }
    }

    @SuppressWarnings("Java8MapApi")
    public AbstractInvokeEvent fetchEvent(MonitorEvent event) {
        if (enumMap.containsKey(event)) {
            return enumMap.get(event);
        } else {
            return null;
        }
    }
}

