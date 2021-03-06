package com.xu.rpc.cluster.directory;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.commons.util.AdaptiveExtensionUtils;
import com.xu.rpc.commons.util.ReflectionUtils;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.registry.NotifyListener;
import com.xu.rpc.registry.Registry;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.extension.ExtensionLoader;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArraySet;

public class RegistryDirectory extends AbstractDirectory implements NotifyListener {

    private static final Logger logger = Logger.getLogger(RegistryDirectory.class);

    private Registry registry;

    private Class<?> type;

    private Map<String, String> parameters = new ConcurrentHashMap<>();

    private volatile Map<String, List<Invoker>> methodToInvokers = new ConcurrentHashMap<>();

    private volatile Set<URL> cachedInvokerUrls = new CopyOnWriteArraySet<>();

    private volatile Map<String, Invoker> urlToInvokers = new ConcurrentHashMap<>();

    public RegistryDirectory(Class<?> type, URL url, Registry registry){
        super(url);
        this.registry = registry;
        this.type = type;
    }

    // 根据消费方要求调用的方法的名字，找出对应的 invoker 集合，并且返回
    @Override
    public List<Invoker> doGetInvokers(RpcInvocation invocation) {
        if (isDestroyed()){
            logger.error("registry directory for [" + getUrl() + "already destroyed.");
            return new ArrayList<>(0);
        }

        // 获取到方法的签名
        String methodName = new ReflectionUtils().getMethodSignature(invocation.getMethod());
        if (methodName == null || methodName.length() == 0)
            throw new IllegalStateException("method name must be configured.");

        List<Invoker> invokers = null;
        Map<String, List<Invoker>> localMethodToInvokers = methodToInvokers;
        if (localMethodToInvokers != null && localMethodToInvokers.size() > 0){
            invokers = localMethodToInvokers.get(methodName);
        }

        return invokers == null ? new ArrayList<>(0) : invokers;
    }

    @Override
    public Class<?> getInterface() {
        return type;
    }

    @Override
    public URL getUrl() {
        return super.getUrl();
    }

    @Override
    public boolean isAvailable() {
        if (isDestroyed()){
            return false;
        }

        Map<String, Invoker> localUrlToInvokers = new HashMap<>(urlToInvokers);
        if (localUrlToInvokers.size() > 0){
            for (Invoker invoker : localUrlToInvokers.values()) {
                if (invoker.isAvailable())
                    return true;
            }
        }

        return false;
    }

    /**
     * 1.设置父类的 destroyed 为 true
     * 2.从注册中心上取消注册监听器
     * 3.销毁所有的 invoker
     */
    @Override
    public void destroy() {
        if (isDestroyed())
            return;

        super.destroy();

        try{
            if (getConsumerUrl() != null && registry != null && registry.isAvailable()){
                registry.unsubscribe(getConsumerUrl(), this);
            }
        }catch (Throwable t){
            logger.error("failed to unsubscribe the listener.");
        }

        destroyAllInvokers();
    }

    // 销毁掉 urlToInvokers 中保存的所有 invoker 映射
    private void destroyAllInvokers() {
        Map<String, Invoker> localUrlToInvokers = urlToInvokers;
        if (urlToInvokers.size() > 0){
            // 在对全局对象进行遍历时，一般要进行复制到另外一个容器中。因为可能一个线程在进行遍历，而另外一个线程在进行修改，
            List<Invoker> invokers = new ArrayList<>(localUrlToInvokers.values());
            for (Invoker invoker : invokers) {
                try {
                    invoker.destroy();
                    logger.info("destroy invoker [" + invoker + "] successfully!");
                } catch (Exception e) {
                    logger.error("failed to destroy the invoker for " + invoker.getUrl());
                }
            }
        }
        localUrlToInvokers.clear();
    }

    @Override
    public void notify(List<URL> invokerUrls) throws RpcException {
        if (invokerUrls == null || invokerUrls.size() == 0) {
            destroyAllInvokers();
            return;
        }

        Map<String, Invoker> oldUrlToInvokers = this.urlToInvokers;

        if (cachedInvokerUrls.size() != 0){
            invokerUrls.addAll(cachedInvokerUrls);
        }else{
            // 更新缓存 Invoker 的 url
            cachedInvokerUrls = new HashSet<>();
            cachedInvokerUrls.addAll(invokerUrls);
        }

        if (invokerUrls.size() == 0)
            return;

        Map<String, Invoker> newUrltoInvokers = toInvokers(invokerUrls);
        Map<String, List<Invoker>> newMethodToInvokers = toMethodInvokers(newUrltoInvokers);
        this.urlToInvokers = newUrltoInvokers;
        this.methodToInvokers = newMethodToInvokers;

        try{
            // 关闭掉已经下线的 invoker
            destroyInvokers(newUrltoInvokers, oldUrlToInvokers);
        }catch (Throwable t){
            logger.error("destroy unused invokers' error.");
        }
    }

    private void destroyInvokers(Map<String, Invoker> newUrltoInvokers, Map<String, Invoker> oldUrlToInvokers) {
        // 通过 toInvokers 得到的 newUrlToInvokers 可能大小为 0，也就是说可能提供者的协议，消费者端都不支持
        if (newUrltoInvokers == null || newUrltoInvokers.size() == 0){
            destroyAllInvokers();
            return;
        }

        List<String> unused = new ArrayList<>();
        Set<String> invokers = newUrltoInvokers.keySet();
        for (Map.Entry<String, Invoker> entry : oldUrlToInvokers.entrySet()) {
            if (!invokers.contains(entry.getKey())){
                unused.add(entry.getKey());
            }
        }

        for (String url : unused) {
            Invoker invoker = oldUrlToInvokers.remove(url);
            if (invoker != null){
                try {
                    invoker.destroy();
                    logger.info("destroy the invoker [" + invoker.getUrl() + "] successfully.");
                } catch (Exception e) {
                    logger.error("destroy the invoker [" + invoker.getUrl() + "] fail.");
                }
            }
        }

    }

    @SuppressWarnings("Java8MapApi")
    private Map<String, List<Invoker>> toMethodInvokers(Map<String, Invoker> urlToInvokers) {
        Map<String, List<Invoker>> newMethodToInvokers = new HashMap<>();
        if (urlToInvokers != null && urlToInvokers.size() > 0){
            for (Invoker invoker : urlToInvokers.values()) {
                String methodsKey = invoker.getUrl().getParameter(RpcConfig.METHODS_KEY);
                if (methodsKey == null || methodsKey.length() == 0){
                    throw new IllegalStateException("methods attribute is null.");
                }
                String[] methods = methodsKey.split(RpcConfig.SEMICOLON);
                for (String method : methods) {
                    List<Invoker> invokers = newMethodToInvokers.get(method);
                    if (invokers == null){
                        invokers = new ArrayList<>();
                        newMethodToInvokers.put(method, invokers);
                    }
                    invokers.add(invoker);
                }
            }
        }

        return newMethodToInvokers;
    }

    private Map<String, Invoker> toInvokers(List<URL> invokerUrls) {
        Map<String, Invoker> newInvokerToUrls = new HashMap<>();
        if (invokerUrls == null || invokerUrls.size() == 0)
            return newInvokerToUrls;

        // 用户在 <nettyrpc:reference/> 的 protocol 属性中进行配置，只调用指定协议的服务提供方，其它协议忽略
        String protocols = parameters.get(RpcConfig.PROTOCOL_KEY);

        for (URL providerUrl : invokerUrls) {

            if (protocols != null && protocols.length() != 0){
                String[] acceptProtocols = protocols.split(RpcConfig.COMMA_SEPARATOR);
                boolean accept = false;
                // 遍历检查提供者 invoker 的 url 是否在用户指定的协议之中
                for (String protocol : acceptProtocols) {
                    if (StringUtils.equals(protocol, providerUrl.getProtocol())){
                        accept = true;
                        break;
                    }
                }

                if (!accept)
                    continue;
            }

            if (!ExtensionLoader.getExtensionLoader(Protocol.class).hasExtension(providerUrl.getProtocol())){
                logger.error("consumer doesn't support " + providerUrl.getProtocol() + " protocol.");
                continue;
            }

            String key = providerUrl.toFullString();
            Invoker invoker = urlToInvokers.get(key);

            // 缓存没有命中
            if (invoker == null){
                // consumerUrl 是客户端的配置，而 providerUrl 为服务端的配置，这两个配置可能有冲突，所以要进行一些合并或者覆盖操作
                providerUrl = mergeUrl(providerUrl, getConsumerUrl());

                Protocol protocol = AdaptiveExtensionUtils.getProtocol(providerUrl);
                invoker = protocol.refer(providerUrl, this.type);
            }

            newInvokerToUrls.put(key, invoker);
        }

        return newInvokerToUrls;
    }
    
    private URL mergeUrl(URL providerUrl, URL consumerUrl){
        if (providerUrl == null || consumerUrl == null)
            throw new IllegalArgumentException("invalid argument for provioderUrl or consumerUrl");

        String filter = consumerUrl.getParameter(RpcConfig.FILTER_KEY);
        String sticky = consumerUrl.getParameter(RpcConfig.STICKY_KEY);
        int heartbeat = consumerUrl.getParameter(RpcConfig.HEARTBEAT_KEY, RpcConfig.DEFAULT_HEARTBEAT);
        int heartbeatTimeout = consumerUrl.getParameter(RpcConfig.HEARTBEAT_TIMEOUT_KEY, 3 * heartbeat);
        String cache = consumerUrl.getParameter(RpcConfig.CACHE_KEY);
        String capacity = consumerUrl.getParameter(RpcConfig.CACHE_CAPACITY_KEY);
        String segments = consumerUrl.getParameter(RpcConfig.SEGMENTS_KEY);

        // providerUrl 为从注册中心上获取到的 url，也就是服务提供者的配置信息，所以要加上或者覆盖消费者自己的配置信息，
        // 比如 filter、sticky、heartbeat、heartbeatTimeout以及cache、capacity
        providerUrl = providerUrl.replaceParameter(RpcConfig.FILTER_KEY, filter);
        providerUrl = providerUrl.replaceParameter(RpcConfig.STICKY_KEY, sticky);
        providerUrl = providerUrl.replaceParameter(RpcConfig.HEARTBEAT_KEY, String.valueOf(heartbeat));
        providerUrl = providerUrl.replaceParameter(RpcConfig.HEARTBEAT_TIMEOUT_KEY, String.valueOf(heartbeatTimeout));
        providerUrl = providerUrl.replaceParameter(RpcConfig.CACHE_KEY, cache);
        providerUrl = providerUrl.replaceParameter(RpcConfig.CACHE_CAPACITY_KEY, capacity);
        providerUrl = providerUrl.replaceParameter(RpcConfig.SEGMENTS_KEY, segments);
        
        return providerUrl;
    }


    public void subscribe(URL url){
        // 在 AbstractDirectory 中保存消费者端的 url，用于销毁时从注册中心上取消监听器
        // AbstractDirectory 保存了两个属性：url 和 consumerUrl，分别表示注册中心的 url 和 消费端的 url
        // 注册中心 url：zookeeper://host:port/registryService?key1=value1&key2=value2
        // 消费者 consumerUrl：consumer://host:port/UserServiceName?key1=value1&key2=value2
        setConsumerUrl(url);
        this.parameters = url.getParameters();
        registry.subscribe(url, this);
    }
}
