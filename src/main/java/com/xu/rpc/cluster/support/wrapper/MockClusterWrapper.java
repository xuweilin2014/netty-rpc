package com.xu.rpc.cluster.support.wrapper;

import com.xu.rpc.cluster.Cluster;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;

public class MockClusterWrapper implements Cluster {

    private Cluster cluster;

    public MockClusterWrapper(Cluster cluster){
        this.cluster = cluster;
    }

    @Override
    public Invoker join(Directory directory) throws RpcException {
        return new MockClusterInvoker(directory, cluster.join(directory));
    }
}
