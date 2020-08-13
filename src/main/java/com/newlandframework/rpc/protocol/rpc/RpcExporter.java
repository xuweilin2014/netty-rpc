package com.newlandframework.rpc.protocol.rpc;

import com.newlandframework.rpc.protocol.AbstractExporter;
import com.newlandframework.rpc.protocol.Exporter;
import com.newlandframework.rpc.protocol.Invoker;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class RpcExporter extends AbstractExporter {

    private String key;

    private Map<String, Exporter> exporterMap;

    public RpcExporter(Invoker invoker, String key, Map<String, Exporter> exporterMap) {
        super(invoker);
        this.key = key;
        this.exporterMap = exporterMap;
    }

    @Override
    public void unexport(){
        super.unexport();
        exporterMap.remove(key);
    }

}
