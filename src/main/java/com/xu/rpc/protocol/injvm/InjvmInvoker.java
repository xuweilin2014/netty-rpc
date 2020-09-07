package com.xu.rpc.protocol.injvm;

import com.xu.rpc.commons.URL;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.AbstractInvoker;
import com.xu.rpc.protocol.Exporter;

import java.util.Map;

public class InjvmInvoker<T> extends AbstractInvoker<T> {

    private final Map<String, Exporter<?>> exporters;

    public InjvmInvoker(Class<?> type, URL url, Map<String, Exporter<?>> exporters) {
        super(type, url);
        this.exporters = exporters;
    }

    @Override
    public RpcResult doInvoke(RpcInvocation invocation) throws RpcException {
        Exporter<?> exporter = exporters.get(getUrl().getServiceName());
        if (exporter == null)
            throw new RpcException("there is no satisfied exporter found.");
        return exporter.getInvoker().invoke(invocation);
    }

    @Override
    public boolean isAvailable() {
        Exporter<?> exporter = exporters.get(getUrl().getServiceName());
        if (exporter == null) {
            return false;
        }
        return isDestroyed();
    }
}
