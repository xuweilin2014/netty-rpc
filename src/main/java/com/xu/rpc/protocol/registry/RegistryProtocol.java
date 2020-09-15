package com.xu.rpc.protocol.registry;

import com.xu.rpc.cluster.Cluster;
import com.xu.rpc.cluster.directory.RegistryDirectory;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.AbstractProtocol;
import com.xu.rpc.protocol.Exporter;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.registry.Registry;
import com.xu.rpc.registry.RegistryFactory;
import com.xu.rpc.commons.util.AdaptiveExtensionUtils;
import com.xu.rpc.commons.URL;

import java.util.HashMap;
import java.util.Map;

public class RegistryProtocol extends AbstractProtocol {

    public static final String NAME = "registry";

    @Override
    public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException {
        if (invoker == null)
            throw new IllegalArgumentException("invoker cannot be null.");
        if (invoker.getUrl() == null)
            throw new IllegalArgumentException("url in invoker cannot be null.");

        // 1.获取 providerURL
        URL providerUrl =  getProviderUrl(invoker);
        // 2.使用 providerURL 进行真正的导出
        Protocol protocol = AdaptiveExtensionUtils.getProtocol(providerUrl);
        Exporter<T> exporter = protocol.export(new InvokerWrapper<>(invoker, providerUrl));
        // 3.获取 registry 对象的类型
        Registry registry = getRegistry(invoker);
        // 4.将 providerURL 注册到 registryURL 上
        registry.register(providerUrl);
        return exporter;
    }

    private Registry getRegistry(Invoker invoker) {
        // 在 invoker 的 registryURL 中的 registry 属性配置了要使用的注册中心的类型
        RegistryFactory registryFactory = AdaptiveExtensionUtils.getRegistryFactory(invoker.getUrl());
        return registryFactory.getRegistry(invoker.getUrl());
    }

    private URL getProviderUrl(Invoker invoker) {
        String providerUrl = invoker.getUrl().getParameterAndDecoded(RpcConfig.EXPORT_KEY);
        return URL.valueOf(providerUrl);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> Invoker<T> refer(URL url, Class<?> type) throws RpcException {
        url = url.setProtocol(url.getParameter(RpcConfig.REGISTRY_KEY, RpcConfig.DEFAULT_REGISTRY))
                .removeParameter(RpcConfig.REGISTRY_KEY);
        RegistryFactory registryFactory = AdaptiveExtensionUtils.getRegistryFactory(url);
        Registry registry = registryFactory.getRegistry(url);

        // 重新生成一个 consumer 端使用的 url，上面的 url 为注册中心的 url
        Map<String, String> queryMap = URL.parseQueryString(url.getParameterAndDecoded(RpcConfig.REFER_KEY));
        URL consumerUrl = new URL(RpcConfig.CONSUMER, queryMap.remove(RpcConfig.IP_ADDRESS), 0, type.getName(), queryMap);

        // RegistryDirectory 本身既可以看成是 invoker 的集合，同时也可以看成是一个监听器，用于从注册中心获取信息
        RegistryDirectory directory = new RegistryDirectory(type, url, registry);

        directory.subscribe(consumerUrl);

        Cluster cluster = AdaptiveExtensionUtils.getCluster(directory.getUrl());
        return cluster.join(directory);
    }

    static class InvokerWrapper<T> implements Invoker<T>{

        private final Invoker<T> invoker;

        private final URL url;

        public InvokerWrapper(Invoker<T> invoker, URL url){
            this.invoker = invoker;
            this.url = url;
        }

        @Override
        public Class<?> getInterface() {
            return invoker.getInterface();
        }

        @Override
        public RpcResult invoke(RpcInvocation invocation) throws RpcException {
            return invoker.invoke(invocation);
        }

        @Override
        public URL getUrl() {
            return url;
        }

        @Override
        public boolean isAvailable() {
            return invoker.isAvailable();
        }

        @Override
        public void destroy() {
            invoker.destroy();
        }
    }

    @Override
    public String doGetName() {
        return NAME;
    }
}
