package com.xu.rpc.protocol;

import io.netty.util.internal.ConcurrentSet;
import org.apache.log4j.Logger;

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public abstract class AbstractProtocol implements Protocol {

    private static final Logger logger = Logger.getLogger(AbstractProtocol.class);

    protected final Map<String, Exporter<?>> exporters = new ConcurrentHashMap<>();

    protected final Set<Invoker> invokers = new ConcurrentSet<>();

    public String getServiceKey(String serviceName, int port){
        return serviceName + ":" + port;
    }

    @Override
    public void destroy() {
        // 销毁掉 invokers 中所有的 invoker
        for (Invoker invoker : invokers) {
            try {
                invoker.destroy();
                invokers.remove(invoker);
            } catch (Exception e) {
                logger.warn("failed to destroy the invoker " + invoker.getInterface());
            }
        }

        // 销毁掉 exporters 中所有的 exporter
        for (String key : exporters.keySet()) {
            Exporter<?> exporter = exporters.remove(key);
            try {
                exporter.unexport();
            } catch (Exception e) {
                logger.warn("failed to unexport the exporter " + key);
            }
        }
    }

    public String getName(){
        return doGetName();
    }

    public Set<Invoker> getInvokers() {
        return Collections.unmodifiableSet(invokers);
    }

    public Map<String, Exporter<?>> getExporters() {
        return Collections.unmodifiableMap(exporters);
    }

    public abstract String doGetName();
}
