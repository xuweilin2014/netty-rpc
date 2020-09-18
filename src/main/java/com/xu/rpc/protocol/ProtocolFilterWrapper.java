package com.xu.rpc.protocol;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.extension.ExtensionLoader;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.filter.ChainFilter;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.commons.Assert;
import com.xu.rpc.commons.URL;

import java.util.List;

public class ProtocolFilterWrapper implements Protocol {

    private Protocol protocol;

    public ProtocolFilterWrapper(Protocol protocol){
        Assert.notNull(protocol, "protocol == null");
        this.protocol = protocol;
    }

    private <T> Invoker<T> buildInvokerChain(Invoker<T> invoker, String group){
        URL url = invoker.getUrl();
        List<ChainFilter> filters = ExtensionLoader.getExtensionLoader(ChainFilter.class).getActivateExtension(url, RpcConfig.FILTER, group);
        for (int i = filters.size() - 1; i >= 0; i--) {
            ChainFilter filter = filters.get(i);
            Invoker<T> next = invoker;
            invoker = new Invoker<T>() {
                @Override
                public URL getUrl() {
                    return next.getUrl();
                }

                @Override
                public boolean isAvailable() {
                    return next.isAvailable();
                }

                @Override
                public void destroy() {
                    next.destroy();
                }

                @Override
                public Class<?> getInterface() {
                    return next.getInterface();
                }

                @Override
                public RpcResult invoke(RpcInvocation invocation) throws RpcException {
                    return filter.intercept(next, invocation);
                }
            };
        }

        return invoker;
    }

    @Override
    public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException {
        if (RpcConfig.REGISTRY_PROTOCOL.equals(invoker.getUrl().getProtocol())){
            return protocol.export(invoker);
        }
        return protocol.export(buildInvokerChain(invoker, RpcConfig.PROVIDER));
    }

    @Override
    public <T> Invoker<T> refer(URL url, Class<?> type) throws RpcException {
        if (RpcConfig.REGISTRY_PROTOCOL.equals(url.getProtocol())){
            return protocol.refer(url, type);
        }
        return buildInvokerChain(protocol.refer(url, type), RpcConfig.CONSUMER);
    }

    @Override
    public void destroy() {
        protocol.destroy();
    }

    public Protocol getProtocol() {
        return protocol;
    }
}
