package com.newlandframework.rpc.protocol;

import com.newlandframework.rpc.core.extension.Extension;
import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.util.URL;

@Extension("rpc")
public interface Protocol {
    public Exporter export(Invoker invoker) throws RpcException;

    public Invoker refer(URL url) throws RpcException;

    public void destroy();
}
