package com.xu.rpc.cluster;

import com.xu.rpc.cluster.support.FailsafeCluster;
import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.core.extension.Extension;

@Extension(FailsafeCluster.NAME)
public interface Cluster {

    public Invoker join(Directory directory) throws RpcException;

}
