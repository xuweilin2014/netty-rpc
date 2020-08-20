package com.xu.rpc.registry.zookeeper;

import com.xu.rpc.registry.AbstractRegistryFactory;
import com.xu.rpc.registry.Registry;
import com.xu.rpc.util.URL;

public class ZookeeperRegistryFactory extends AbstractRegistryFactory {
    @Override
    public Registry createRegistry(URL url) {
        return new ZookeeperRegistry(url);
    }

}
