package com.newlandframework.rpc.util;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.core.extension.ExtensionLoader;
import com.newlandframework.rpc.protocol.Protocol;
import com.newlandframework.rpc.registry.RegistryFactory;

public class AdaptiveExtensionUtil {

    public static Protocol getProtocolExtension(URL url){
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

    public static RegistryFactory getRegistryFactoryExtension(URL url){
        if (url == null)
            throw new IllegalArgumentException("url cannot be null.");

        String name = url.getParameter(RpcSystemConfig.REGISTRY_FACTORY);
        ExtensionLoader<RegistryFactory> loader = ExtensionLoader.getExtensionLoader(RegistryFactory.class);
        // 如果获取不到用户配置的 registry 的值，则使用 registry 默认的扩展
        if (name == null || name.length() == 0)
            return loader.getDefaultExtension();
        else
            return loader.getExtension(name);
    }
}
