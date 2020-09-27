package com.xu.rpc.cluster.directory;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;

import java.util.List;
import java.util.Map;

public abstract class AbstractDirectory implements Directory {

    private URL url;

    private boolean destroyed = false;

    private URL consumerUrl;

    public AbstractDirectory(URL url) {
        if (url.getParameter(RpcConfig.REFER_KEY) != null) {
            Map<String, String> queryMap = URL.parseQueryString(url.getParameterAndDecoded(RpcConfig.REFER_KEY));
            this.url = new URL(url.getProtocol(), url.getHost(), url.getPort(), url.getPath(), queryMap);
        }else{
            this.url = url;
        }
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
    public URL getUrl() {
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
