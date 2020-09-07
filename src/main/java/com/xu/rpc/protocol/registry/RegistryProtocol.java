package com.xu.rpc.protocol.registry;

import com.xu.rpc.cluster.Cluster;
import com.xu.rpc.cluster.directory.RegistryDirectory;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.AbstractProtocol;
import com.xu.rpc.protocol.Exporter;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.registry.Registry;
import com.xu.rpc.registry.RegistryFactory;
import com.xu.rpc.commons.util.AdaptiveExtensionUtil;
import com.xu.rpc.commons.URL;

import java.util.HashMap;
import java.util.Map;

public class RegistryProtocol extends AbstractProtocol {

    @Override
    public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException {
        if (invoker == null)
            throw new IllegalArgumentException("invoker cannot be null.");
        if (invoker.getUrl() == null)
            throw new IllegalArgumentException("url in invoker cannot be null.");

        // 1.获取 providerURL
        URL providerURL =  getProviderURL(invoker);
        // 2.使用 providerURL 进行真正的导出
        Protocol protocol = AdaptiveExtensionUtil.getProtocol(providerURL);
        Exporter<T> exporter = protocol.export(invoker);
        // 3.获取 registry 对象的类型
        Registry registry = getRegistry(invoker);
        // 4.将 providerURL 注册到 registryURL 上
        registry.register(providerURL);
        return exporter;
    }

    private Registry getRegistry(Invoker invoker) {
        // 在 invoker 的 registryURL 中的 registry 属性配置了要使用的注册中心的类型
        RegistryFactory registryFactory = AdaptiveExtensionUtil.getRegistryFactory(invoker.getUrl());
        return registryFactory.getRegistry(invoker.getUrl());
    }

    private URL getProviderURL(Invoker invoker) {
        String providerURL = invoker.getUrl().getParameterAndDecoded(RpcConfig.EXPORT_KEY);
        return URL.valueOf(providerURL);
    }

    @Override
    public <T> Invoker<T> refer(URL url, Class<?> type) throws RpcException {
        url = url.setProtocol(url.getParameter(RpcConfig.REGISTRY_KEY, RpcConfig.DEFAULT_REGISTRY))
                .removeParameter(RpcConfig.REGISTRY_KEY);
        RegistryFactory registryFactory = AdaptiveExtensionUtil.getRegistryFactory(url);
        Registry registry = registryFactory.getRegistry(url);

        // RegistryDirectory 本身既可以看成是 invoker 的集合，同时也可以看成是一个监听器，用于从注册中心获取信息
        RegistryDirectory directory = new RegistryDirectory(type, url, registry);
        // 重新生成一个 consumer 端使用的 url
        Map<String, String> map = new HashMap<>(directory.getURL().getParameters());
        URL consumerURL = new URL(RpcConfig.CONSUMER, map.get(RpcConfig.CONSUMER_HOST), 0, type.getName(), map);

        directory.subscribe(consumerURL);

        Cluster cluster = AdaptiveExtensionUtil.getCluster(directory.getURL());
        return cluster.join(directory);
    }
}
