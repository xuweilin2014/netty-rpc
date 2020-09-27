package com.xu.rpc.commons.util;

import com.xu.rpc.commons.cache.CacheFactory;
import com.xu.rpc.commons.limiter.RateLimiterFactory;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.registry.RegistryFactory;
import com.xu.rpc.cluster.Cluster;
import com.xu.rpc.cluster.LoadBalancer;
import com.xu.rpc.commons.URL;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.extension.ExtensionLoader;

public class AdaptiveExtensionUtils {

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

    public static LoadBalancer getLoadBalance(URL url){
        if (url == null)
            throw new IllegalArgumentException("url cannot be null.");

        String loadbalance = url.getParameter(RpcConfig.LOADBALANCE_KEY);
        ExtensionLoader<LoadBalancer> loader = ExtensionLoader.getExtensionLoader(LoadBalancer.class);
        if (loadbalance == null || loadbalance.length() == 0)
            return loader.getDefaultExtension();
        else
            return loader.getExtension(loadbalance);
    }

    public static <K,V> CacheFactory<K,V> getCacheFactory(URL url){
        if (url == null)
            throw new IllegalArgumentException("url cannot be null.");

        String cache = url.getParameter(RpcConfig.CACHE_KEY);
        ExtensionLoader<CacheFactory> loader = ExtensionLoader.getExtensionLoader(CacheFactory.class);
        if (cache == null || cache.length() == 0)
            return loader.getDefaultExtension();
        else
            return loader.getExtension(cache);
    }

    public static RateLimiterFactory getLimiterFactory(URL url){
        if (url == null)
            throw new IllegalArgumentException("url cannot be null.");

        String limiter = url.getParameter(RpcConfig.LIMITER_KEY);
        ExtensionLoader<RateLimiterFactory> loader = ExtensionLoader.getExtensionLoader(RateLimiterFactory.class);
        if (limiter == null || limiter.length() == 0)
            return loader.getExtension(limiter);
        else
            return loader.getDefaultExtension();
    }
}
