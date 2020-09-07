package com.xu.rpc.protocol.rpc;

import com.xu.rpc.protocol.AbstractExporter;
import com.xu.rpc.protocol.Exporter;
import com.xu.rpc.protocol.Invoker;

import java.util.Map;

public class RpcExporter extends AbstractExporter {

    private String key;

    private Map<String, Exporter<?>> exporterMap;

    public RpcExporter(Invoker invoker, String key, Map<String, Exporter<?>> exporterMap) {
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
