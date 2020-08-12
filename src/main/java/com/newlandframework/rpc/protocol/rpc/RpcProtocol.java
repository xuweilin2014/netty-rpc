package com.newlandframework.rpc.protocol.rpc;

import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.protocol.Exporter;
import com.newlandframework.rpc.protocol.Invoker;
import com.newlandframework.rpc.protocol.Protocol;
import com.newlandframework.rpc.util.URL;

public class RpcProtocol implements Protocol {
    @Override
    public Exporter export(Invoker invoker) throws RpcException {
        return null;
    }

    @Override
    public Invoker refer(URL url) throws RpcException {
        return null;
    }

    @Override
    public void destroy() {

    }
}
