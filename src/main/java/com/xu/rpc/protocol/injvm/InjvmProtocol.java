package com.xu.rpc.protocol.injvm;

import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.AbstractProtocol;
import com.xu.rpc.protocol.Exporter;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.util.URL;

public class InjvmProtocol extends AbstractProtocol {

    @Override
    public <T> Exporter<T> export(Invoker<T> invoker) throws RpcException {
        return new InjvmExporter<T>(invoker, invoker.getUrl().getServiceName(), exporters);
    }

    @Override
    public <T> Invoker<T> refer(URL url, Class<?> type) throws RpcException {
        return new InjvmInvoker<T>(type, url, exporters);
    }

}
