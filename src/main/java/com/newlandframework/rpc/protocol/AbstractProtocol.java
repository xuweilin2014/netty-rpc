package com.newlandframework.rpc.protocol;

import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.util.URL;

import java.util.concurrent.ConcurrentHashMap;

public class AbstractProtocol implements Protocol {

    protected ConcurrentHashMap<String, Exporter> exporters = new ConcurrentHashMap<>();

    @Override
    public Exporter export(Invoker invoker) throws RpcException {
        return null;
    }

    @Override
    public Invoker refer(URL url) throws RpcException {
        return null;
    }

    @Override
    public void destroy() {
        // TODO: 2020/8/9
        // 销毁掉 exporters 中所有的 exporter
    }
}
