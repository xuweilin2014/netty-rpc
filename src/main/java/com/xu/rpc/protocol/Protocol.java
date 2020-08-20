package com.xu.rpc.protocol;

import com.xu.rpc.core.extension.Extension;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.util.URL;

@Extension("rpc")
public interface Protocol {
    public Exporter export(Invoker invoker) throws RpcException;

    public Invoker refer(URL url, Class<?> type) throws RpcException;

    public void destroy();
}
