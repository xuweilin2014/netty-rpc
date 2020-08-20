package com.xu.rpc.protocol.injvm;

import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.AbstractProtocol;
import com.xu.rpc.protocol.Exporter;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.util.URL;

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
