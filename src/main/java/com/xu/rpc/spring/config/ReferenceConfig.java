package com.xu.rpc.spring.config;

import com.xu.rpc.cluster.Cluster;
import com.xu.rpc.cluster.directory.StaticDirectory;
import com.xu.rpc.cluster.support.AvailableCluster;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.proxy.JDKProxyFactory;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.util.AdaptiveExtensionUtil;
import com.xu.rpc.util.URL;
import lombok.Getter;
import lombok.Setter;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Getter
@Setter
public class ReferenceConfig<T> extends AbstractConfig {

    private static final Logger logger = Logger.getLogger(ReferenceConfig.class);

    protected String timeout;

    protected String retries;

    protected String loadbalance;

    protected String async;

    protected String cluster;

    protected String heartbeat;

    protected String heartbeatTimeout;

    protected String stub;

    protected String scope;

    protected String filter;

    // 进行服务直连
    protected String url;

    protected T ref;

    protected String protocol;

    private List<URL> urls;

    private volatile boolean destroyed;

    private volatile Invoker invoker;

    private Class<?> interfaceClass;

    public synchronized T get(){
        if (destroyed)
            throw new IllegalStateException("already destroyed.");

        if (ref == null) {
            init();
        }

        return ref;
    }

    public void init(){
        // TODO: 2020/8/19  进行参数的配置
        Map<String, String> map = new HashMap<>();
        map.put("interface", interfaceName);

        try {
            interfaceClass = Class.forName(interfaceName, true, Thread.currentThread().getContextClassLoader());
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException("interface class not found.");
        }

        ref = createProxy(map);
    }

    @SuppressWarnings("unchecked")
    public T createProxy(Map<String, String> map){
        // TODO: 2020/8/19
        // 暂时起作用的协议，只是用来判断是否进行本地引用
        URL tmpUrl = new URL("temp", "localhost", 0, map);
        // 判断是否进行本地引用 do local reference
        boolean isJvmRefer = isJvm(tmpUrl);

        // 进行本地引用
        if (isJvmRefer){

        // 进行远程引用
        }else{
            // url 不等于 null 的话，意味着进行服务直连
            if (url != null && url.length() != 0){

            // 使用注册中心
            }else{
                List<URL> us = getRegistries();
                if (us != null && us.size() > 0){
                    for (URL u : us) {
                        urls.add(u.addParameterAndEncoded(RpcConfig.REFER, URL.toQueryString(map)));
                    }
                }
            }

            if (urls == null || urls.size() == 0){
                throw new IllegalStateException("no registry to subscribe.");
            }

            // 只有单个注册中心或者单个服务直连 url
            if (urls.size() == 1){
                URL url = urls.get(0);
                invoker = AdaptiveExtensionUtil.getProtocol(url).refer(url, interfaceClass);

            // 有多个注册中心或者多个服务直连 url
            }else {
                List<Invoker> invokers = new ArrayList<>();
                URL registryURL = null;
                for (URL u : urls) {
                    Protocol protocol = AdaptiveExtensionUtil.getProtocol(u);
                    invokers.add(protocol.refer(u, interfaceClass));
                    if (RpcConfig.REGISTRY_PROTOCOL.equals(u.getProtocol())){
                        registryURL = u;
                    }
                }

                // 有多个注册中心，使用 AvailableCluster 和 StaticDirectory
                if (registryURL != null){
                    URL url = registryURL.addParameter(RpcConfig.CLUSTER, AvailableCluster.NAME);
                    Cluster cluster = AdaptiveExtensionUtil.getCluster(url);
                    invoker = cluster.join(new StaticDirectory(invokers, url));

                // 有多个服务直连 url，使用 url 中指定的 Cluster，如果没有指定的话，使用 FailoverCluster，并且使用 StaticDirectory 保存 invokers
                }else{
                    if (invokers.size() == 0){
                        throw new IllegalStateException("no invoker available.");
                    }
                    URL url = invokers.get(0).getURL();
                    Cluster cluster = AdaptiveExtensionUtil.getCluster(url);
                    invoker = cluster.join(new StaticDirectory(invokers, url));
                }
            }
        }

        if (!invoker.isAvailable()){
            throw new IllegalStateException("no provider available for the service " + interfaceName);
        }

        logger.info("refer service " + interfaceName + " from url " +invoker.getURL());

        return (T) JDKProxyFactory.getProxy(invoker);
    }

    private boolean isJvm(URL url) {
        // TODO: 2020/8/19
        return false;
    }

    public synchronized void destroy(){
        // TODO: 2020/8/18  
    }

}
