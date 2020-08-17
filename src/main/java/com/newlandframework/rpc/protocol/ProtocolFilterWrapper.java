package com.newlandframework.rpc.protocol;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.core.extension.ExtensionLoader;
import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.filter.ChainFilter;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.util.Assert;
import com.newlandframework.rpc.util.URL;

import java.util.List;

public class ProtocolFilterWrapper implements Protocol {

    private Protocol protocol;

    public ProtocolFilterWrapper(Protocol protocol){
        Assert.notNull(protocol, "protocol == null");
        this.protocol = protocol;
    }

    private Invoker buildInvokerChain(Invoker invoker, String group){
        URL url = invoker.getURL();
        List<ChainFilter> filters = ExtensionLoader.getExtensionLoader(ChainFilter.class)
                            .getActivateExtension(url, RpcSystemConfig.FILTER, group);
        for (ChainFilter filter : filters) {
            Invoker next = invoker;
            invoker = new Invoker() {
                @Override
                public Object invoke(MessageRequest request){
                    return filter.intercept(next, request);
                }

                @Override
                public URL getURL() {
                    return next.getURL();
                }

                @Override
                public Class<?> getInterface() {
                    return next.getInterface();
                }
            };
        }

        return invoker;
    }

    @Override
    public Exporter export(Invoker invoker) throws RpcException {
        if (RpcSystemConfig.REGISTRY_PROTOCOL.equals(invoker.getURL().getProtocol())){
            return protocol.export(invoker);
        }
        return protocol.export(buildInvokerChain(invoker, RpcSystemConfig.PROVIDER));
    }

    @Override
    public Invoker refer(URL url) throws RpcException {
        if (RpcSystemConfig.REGISTRY_PROTOCOL.equals(url.getProtocol())){
            return protocol.refer(url);
        }
        return buildInvokerChain(protocol.refer(url), RpcSystemConfig.CONSUMER);
    }

    @Override
    public void destroy() {
        // TODO: 2020/8/14
    }

}
