package com.xu.rpc.cluster.directory;

import com.xu.rpc.commons.URL;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.core.RpcInvocation;

import java.util.List;

public class StaticDirectory extends AbstractDirectory {

    private List<Invoker> invokers;

    public StaticDirectory(List<Invoker> invokers, URL url){
        super(url);
        if (invokers == null || invokers.size() == 0)
            throw new IllegalArgumentException("no invokers.");
        this.invokers = invokers;
    }

    @Override
    public Class<?> getInterface() {
        if (invokers == null || invokers.size() == 0)
            throw new IllegalStateException("no invokers.");
        return invokers.get(0).getInterface();
    }

    @Override
    public boolean isAvailable() {
        if (isDestroyed())
            return false;
        for (Invoker invoker : invokers) {
            if (invoker.isAvailable())
                return true;
        }
        return false;
    }

    @Override
    public void destroy() {
        if (isDestroyed())
            return;
        super.destroy();
        for (Invoker invoker : invokers) {
            invoker.destroy();
        }
        invokers.clear();
    }

    @Override
    public List<Invoker> doGetInvokers(RpcInvocation invocation) {
        return invokers;
    }
}
