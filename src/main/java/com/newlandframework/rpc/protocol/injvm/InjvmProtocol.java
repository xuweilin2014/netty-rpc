package com.newlandframework.rpc.protocol.injvm;

import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.protocol.AbstractProtocol;
import com.newlandframework.rpc.protocol.Exporter;
import com.newlandframework.rpc.protocol.Invoker;
import com.newlandframework.rpc.util.URL;

public class InjvmProtocol extends AbstractProtocol {
    @Override
    public Exporter export(Invoker invoker) throws RpcException {
        return null;
    }

    @Override
    public Invoker refer(URL url) throws RpcException {
        return null;
    }
}
