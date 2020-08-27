package com.xu.rpc.cluster.support;

import com.xu.rpc.cluster.Cluster;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;

public class FailfastCluster implements Cluster {

    public static final String NAME = "failfast";

    @Override
    public Invoker join(Directory directory) throws RpcException {
        return new FailfastClusterInvoker(directory);
    }

}
