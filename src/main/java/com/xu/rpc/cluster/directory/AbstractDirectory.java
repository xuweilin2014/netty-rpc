package com.xu.rpc.cluster.directory;

import com.xu.rpc.cluster.Directory;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.util.URL;

import java.util.List;

public abstract class AbstractDirectory implements Directory {

    private URL url;

    private boolean destroyed = false;

    private URL consumerUrl;

    public AbstractDirectory(URL url) {
        this.url = url;
    }

    @Override
    public List<Invoker> getInvokers(RpcInvocation invocation) throws RpcException {
        if (destroyed){
            throw new IllegalStateException("already destroyed.");
        }

        return doGetInvokers(invocation);
    }

    public void setConsumerUrl(URL consumerUrl){
        this.consumerUrl = consumerUrl;
    }

    public URL getConsumerUrl() {
        return consumerUrl;
    }

    @Override
    public URL getURL() {
        return url;
    }


    @Override
    public void destroy() {
        destroyed = true;
    }

    public boolean isDestroyed(){
        return destroyed;
    }

    public abstract List<Invoker> doGetInvokers(RpcInvocation invocation);
}
