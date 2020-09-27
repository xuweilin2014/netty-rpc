package com.xu.rpc.protocol.injvm;

import com.xu.rpc.protocol.AbstractExporter;
import com.xu.rpc.protocol.Exporter;
import com.xu.rpc.protocol.Invoker;

import java.util.Map;

public class InjvmExporter<T> extends AbstractExporter<T> {

    private final String serviceKey;

    private final Map<String, Exporter<?>> exporters;

    public InjvmExporter(Invoker<T> invoker, String serviceKey, Map<String, Exporter<?>> exporters) {
        super(invoker);
        this.serviceKey = serviceKey;
        this.exporters = exporters;
        exporters.put(serviceKey, this);
    }

    public void unexport(){
        super.unexport();
        exporters.remove(serviceKey);
    }

}
