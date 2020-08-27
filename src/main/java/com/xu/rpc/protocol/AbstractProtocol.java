package com.xu.rpc.protocol;

import io.netty.util.internal.ConcurrentSet;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public abstract class AbstractProtocol implements Protocol {

    protected final Map<String, Exporter> exporters = new ConcurrentHashMap<>();

    protected final Set<Invoker> invokers = new ConcurrentSet<>();

    public String getServiceKey(String serviceName, int port){
        return serviceName + ":" + port;
    }

    @Override
    public void destroy() {
        // TODO: 2020/8/9
        // 销毁掉 exporters 中所有的 exporter
    }
}
