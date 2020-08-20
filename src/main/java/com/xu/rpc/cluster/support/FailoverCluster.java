package com.xu.rpc.cluster.support;

import com.xu.rpc.cluster.Cluster;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;

public class FailoverCluster implements Cluster {

    public static final String NAME = "failover";

    @Override
    public Invoker join(Directory directory) throws RpcException {
        return null;
    }
}
