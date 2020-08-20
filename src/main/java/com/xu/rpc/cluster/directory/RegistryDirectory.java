package com.xu.rpc.cluster.directory;

import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.registry.NotifyListener;
import com.xu.rpc.registry.Registry;
import com.xu.rpc.util.URL;
import io.netty.util.internal.ConcurrentSet;
import org.apache.log4j.Logger;
import sun.dc.pr.PRError;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class RegistryDirectory extends AbstractDirectory implements NotifyListener {

    private static final Logger logger = Logger.getLogger(RegistryDirectory.class);

    private Registry registry;

    private Protocol protocol;

    private volatile Map<String, List<Invoker>> methodToInvokers = new ConcurrentHashMap<>();

    private volatile Set<URL> cachedInvokerUrls = new ConcurrentSet<>();

    private volatile Map<String, Invoker> urlToInvokers = new ConcurrentHashMap<>();

    public RegistryDirectory(Class<?> type, URL url, Registry registry){
        super(url);
        this.registry = registry;
    }

    @Override
    public List<Invoker> doGetInvokers(String method) {
        return null;
    }

    @Override
    public Class<?> getInterface() {
        return null;
    }

    @Override
    public boolean isAvailable() {
        return false;
    }

    @Override
    public void notify(List<URL> urls) throws RpcException {

    }

    public void subscribe(URL url){
        registry.subscribe(url, this);
    }
}
