package com.xu.rpc.util;

import com.xu.rpc.cluster.Cluster;
import com.xu.rpc.cluster.LoadBalance;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.extension.ExtensionLoader;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.registry.RegistryFactory;

public class AdaptiveExtensionUtil {

    public static Protocol getProtocol(URL url){
        if (url == null)
            throw new IllegalArgumentException("url cannot be null.");

        String name = url.getProtocol();
        ExtensionLoader<Protocol> loader = ExtensionLoader.getExtensionLoader(Protocol.class);
        // 如果获取不到协议，则使用 Protocol 默认的扩展
        if (name == null || name.length() == 0)
            return loader.getDefaultExtension();
        else
            return loader.getExtension(name);
    }

    public static RegistryFactory getRegistryFactory(URL url){
        if (url == null)
            throw new IllegalArgumentException("url cannot be null.");

        String name = url.getParameter(RpcConfig.REGISTRY_FACTORY);
        ExtensionLoader<RegistryFactory> loader = ExtensionLoader.getExtensionLoader(RegistryFactory.class);
        // 如果获取不到用户配置的 registry 的值，则使用 registry 默认的扩展
        if (name == null || name.length() == 0)
            return loader.getDefaultExtension();
        else
            return loader.getExtension(name);
    }

    public static Cluster getCluster(URL url){
        if (url == null)
            throw new IllegalArgumentException("url cannot be null.");

        String cluster = url.getParameter(RpcConfig.CLUSTER_KEY);
        ExtensionLoader<Cluster> loader = ExtensionLoader.getExtensionLoader(Cluster.class);
        if (cluster == null || cluster.length() == 0)
            return loader.getDefaultExtension();
        else
            return loader.getExtension(cluster);
    }

    public static LoadBalance getLoadBalance(URL url){
        if (url == null)
            throw new IllegalArgumentException("url cannot be null.");

        String loadbalance = url.getParameter(RpcConfig.LOADBALANCE_KEY);
        ExtensionLoader<LoadBalance> loader = ExtensionLoader.getExtensionLoader(LoadBalance.class);
        if (loadbalance == null || loadbalance.length() == 0)
            return loader.getExtension(loadbalance);
        else
            return loader.getDefaultExtension();
    }
}
