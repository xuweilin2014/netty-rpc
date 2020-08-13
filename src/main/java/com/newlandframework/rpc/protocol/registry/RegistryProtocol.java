package com.newlandframework.rpc.protocol.registry;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.protocol.AbstractProtocol;
import com.newlandframework.rpc.protocol.Exporter;
import com.newlandframework.rpc.protocol.Invoker;
import com.newlandframework.rpc.protocol.Protocol;
import com.newlandframework.rpc.registry.Registry;
import com.newlandframework.rpc.registry.RegistryFactory;
import com.newlandframework.rpc.util.AdaptiveExtensionUtil;
import com.newlandframework.rpc.util.URL;

public class RegistryProtocol extends AbstractProtocol {

    @Override
    public Exporter export(Invoker invoker) throws RpcException {
        if (invoker == null)
            throw new IllegalArgumentException("invoker cannot be null.");
        if (invoker.getURL() == null)
            throw new IllegalArgumentException("url in invoker cannot be null.");

        // 1.获取 providerURL
        URL providerURL =  getProviderURL(invoker);
        // 2.使用 providerURL 进行真正的导出
        Protocol protocol = AdaptiveExtensionUtil.getProtocolExtension(providerURL);
        Exporter exporter = protocol.export(invoker);
        // 3.获取 registry 对象的类型
        Registry registry = getRegistry(invoker);
        // 4.将 providerURL 注册到 registryURL 上
        registry.register(providerURL);
        return exporter;
    }

    private Registry getRegistry(Invoker invoker) {
        // 在 invoker 的 registryURL 中的 registry 属性配置了要使用的注册中心的类型
        RegistryFactory registryFactory = AdaptiveExtensionUtil.getRegistryFactoryExtension(invoker.getURL());
        return registryFactory.getRegistry(invoker.getURL());
    }

    private URL getProviderURL(Invoker invoker) {
        String providerURL = invoker.getURL().getParameterAndDecoded(RpcSystemConfig.EXPORT_KEY);
        return URL.valueOf(providerURL);
    }

    @Override
    public Invoker refer(URL url) throws RpcException {
        return null;
    }

    @Override
    public void destroy() {
        // TODO: 2020/8/9
    }

    public void subscribe(){
        // TODO: 2020/8/9
    }
}
