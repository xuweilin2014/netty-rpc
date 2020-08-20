package com.xu.rpc.registry;

import com.xu.rpc.core.extension.Extension;
import com.xu.rpc.util.URL;

@Extension("zookeeper")
public interface RegistryFactory {

    public Registry getRegistry(URL url);

}
