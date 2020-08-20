package com.xu.rpc.cluster;

import com.xu.rpc.core.extension.Extension;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;

@Extension("failover")
public interface Cluster {

    public  Invoker join(Directory directory) throws RpcException;

}
