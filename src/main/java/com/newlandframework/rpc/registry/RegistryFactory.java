package com.newlandframework.rpc.registry;

import com.newlandframework.rpc.core.extension.Extension;
import com.newlandframework.rpc.util.URL;

@Extension("zookeeper")
public interface RegistryFactory {

    public Registry getRegistry(URL url);

}
