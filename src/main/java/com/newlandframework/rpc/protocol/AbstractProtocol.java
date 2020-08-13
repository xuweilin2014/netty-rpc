package com.newlandframework.rpc.protocol;

import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.util.URL;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public abstract class AbstractProtocol implements Protocol {

    protected static final Map<String, Exporter> exporters = new ConcurrentHashMap<>();

    public String getServiceKey(String serviceName, int port){
        return serviceName + ":" + port;
    }

    @Override
    public void destroy() {
        // TODO: 2020/8/9
        // 销毁掉 exporters 中所有的 exporter
    }
}
