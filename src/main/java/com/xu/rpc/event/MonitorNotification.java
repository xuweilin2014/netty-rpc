package com.xu.rpc.event;

import com.xu.rpc.jmx.MetricsVisitor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MonitorNotification {

    private final MetricsVisitor visitor;

    private final MonitorEvent event;

    public MonitorNotification(MetricsVisitor visitor, MonitorEvent event){
        this.visitor = visitor;
        this.event = event;
    }

}
