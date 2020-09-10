package com.xu.rpc.protocol;

import com.xu.rpc.core.extension.Extension;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.commons.URL;

@Extension("rpc")
public interface Protocol {

    public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException;

    public <T> Invoker<T> refer(URL url, Class<?> type) throws RpcException;

    public void destroy();
}
