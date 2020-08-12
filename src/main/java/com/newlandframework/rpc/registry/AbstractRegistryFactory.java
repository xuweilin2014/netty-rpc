package com.newlandframework.rpc.registry;

import com.newlandframework.rpc.util.URL;

public abstract class AbstractRegistryFactory implements RegistryFactory{

    @Override
    public Registry getRegistry(URL url) {
        // TODO: 2020/8/11 对注册中心进行缓存 ，以免重复创建
        return null;
    }

    public abstract Registry createRegistry(URL url);
}
