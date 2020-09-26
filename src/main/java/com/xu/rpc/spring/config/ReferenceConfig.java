package com.xu.rpc.spring.config;

import com.xu.rpc.cluster.Cluster;
import com.xu.rpc.cluster.directory.StaticDirectory;
import com.xu.rpc.cluster.support.AvailableCluster;
import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.util.AdaptiveExtensionUtils;
import com.xu.rpc.commons.util.ReflectionUtils;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.extension.Activate;
import com.xu.rpc.core.extension.Attribute;
import com.xu.rpc.core.proxy.JDKProxyFactory;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.spring.bean.NettyRpcParameter;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Getter
@Setter
public class ReferenceConfig<T> extends AbstractConfig {

    private static final Logger logger = Logger.getLogger(ReferenceConfig.class);
    // 重试次数
    @Attribute
    protected String retries;
    // 负载均衡的策略，包括：hash、roundrobin、random 三种
    @Attribute
    protected String loadbalance;
    // 是否开启异步，默认值为 false
    @Attribute
    protected String async;
    // 集群容错方式：failback, failover, failsafe, failfast
    @Attribute
    protected String cluster;
    // 桩
    @Attribute
    protected String stub;
    // 是否开启粘滞连接
    @Attribute
    protected String sticky;
    // 直连 url
    @Attribute
    protected String url;
    // 服务降级(本地伪装)
    @Attribute
    protected String mock;

    protected T ref;

    private List<URL> urls = new ArrayList<>();

    protected volatile boolean destroyed;

    protected volatile Invoker invoker;

    protected Class<?> interfaceClass;

    public synchronized T get(){
        if (destroyed)
            throw new IllegalStateException("already destroyed.");

        if (ref == null) {
            init();
        }

        return ref;
    }

    public void init(){
        Map<String, String> parameters = new HashMap<>();
        // 添加 interface 的值
        parameters.put(RpcConfig.INTERFACE_KEY, interfaceName);
        // 添加 ip 的值，也就是本机的 ip 地址
        parameters.put(RpcConfig.IP_ADDRESS, getHostAddress());
        // 添加 methods 的值，也就是 interfaceClass 这个类中所有的方法名（不包括父类），并且方法名之间使用逗号分隔
        parameters.put(RpcConfig.METHODS_KEY, StringUtils.join(new ReflectionUtils().getClassMethodSignature(interfaceClass), RpcConfig.SEMICOLON));
        // 添加 application 的值，也就是应用名
        parameters.put(RpcConfig.APPLICATION_KEY, application.getName());

        // 将服务端中所有 <nettyrpc:parameter/> 标签中的 key、value 值保存到 parameters 中
        if (getParameters() != null && getParameters().size() > 0){
            for (NettyRpcParameter parameter : getParameters().values()) {
                if (!StringUtils.isEmpty(parameter.getKey()) && !StringUtils.isEmpty(parameter.getValue())){
                    parameters.put(parameter.getKey(), parameter.getValue());
                }
            }
        }

        // 将此 ReferenceConfig 中的不为空的成员属性添加到 map 中
        appendParameters(this, parameters);

        try {
            if (interfaceName != null && interfaceName.length() > 0)
                interfaceClass = Thread.currentThread().getContextClassLoader().loadClass(interfaceName);
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException("error occurs when loading " + interfaceName + ", caused by " + e.getMessage());
        }

        ref = createProxy(parameters);
    }

    @SuppressWarnings("unchecked")
    public T createProxy(Map<String, String> map){
        // 暂时起作用的协议，只是用来判断是否进行本地引用
        URL tmpUrl = new URL("temp", RpcConfig.LOCALHOST, 0, map.get(RpcConfig.INTERFACE_KEY), map);
        // 判断是否进行本地引用 do local reference
        boolean isJvmRefer = isJvm(tmpUrl);

        // 进行本地引用
        if (isJvmRefer){
            URL injvmLocal = tmpUrl.setProtocol(RpcConfig.INJVM_PROTOCOL);
            Protocol protocol = AdaptiveExtensionUtils.getProtocol(injvmLocal);
            invoker = protocol.refer(injvmLocal, interfaceClass);
            logger.info("using injvm service " + interfaceClass.getName());

        // 进行远程引用
        }else{
            // url 属性不为空，说明使用 url 直连
            if (!StringUtils.isEmpty(url)){
                String[] us = url.split(RpcConfig.SEMICOLON);
                if (us.length > 0){
                    for (String u : us) {
                        // 检查配置的直连 url 的格式是否合法
                        if (!URL.isUrlInvalid(u)){
                            throw new IllegalStateException("url format is invalid, url " + u);
                        }
                        URL url = URL.valueOf(u);
                        url = new URL(url.getProtocol(), url.getHost(), url.getPort(), interfaceName, map);
                        urls.add(url);
                    }
                }
            // 使用注册中心
            } else {
                List<URL> us = getRegistries();
                if (us != null && us.size() > 0) {
                    for (URL u : us) {
                        urls.add(u.addParameterAndEncoded(RpcConfig.REFER_KEY, URL.toQueryString(map)));
                    }
                }
            }

            if (urls == null || urls.size() == 0){
                throw new IllegalStateException("no registry to subscribe");
            }

            // 只有单个注册中心或者单个服务直连 url
            if (urls.size() == 1){
                URL url = urls.get(0);
                invoker = AdaptiveExtensionUtils.getProtocol(url).refer(url, interfaceClass);
            // 有多个注册中心或者多个服务直连 url
            }else {
                List<Invoker> invokers = new ArrayList<>();
                URL registryURL = null;
                for (URL u : urls) {
                    Protocol protocol = AdaptiveExtensionUtils.getProtocol(u);
                    invokers.add(protocol.refer(u, interfaceClass));
                    if (RpcConfig.REGISTRY_PROTOCOL.equals(u.getProtocol())){
                        registryURL = u;
                    }
                }

                // 有多个注册中心，使用 AvailableCluster 和 StaticDirectory
                if (registryURL != null){
                    URL url = registryURL.addParameter(RpcConfig.CLUSTER_KEY, AvailableCluster.NAME);
                    Cluster cluster = AdaptiveExtensionUtils.getCluster(url);
                    invoker = cluster.join(new StaticDirectory(invokers, url));

                // 有多个服务直连 url，使用 url 中指定的 Cluster，如果没有指定的话，使用 FailsafeCluster，并且使用 StaticDirectory 保存 invokers
                }else{
                    if (invokers.size() == 0){
                        throw new IllegalStateException("no invoker available.");
                    }
                    URL url = invokers.get(0).getUrl();
                    Cluster cluster = AdaptiveExtensionUtils.getCluster(url);
                    invoker = cluster.join(new StaticDirectory(invokers, url));
                }
            }
        }

        if (!invoker.isAvailable()){
            throw new IllegalStateException("no provider available for the service " + interfaceName);
        }

        logger.info("refer service " + interfaceName + " from url " +invoker.getUrl());

        return (T) JDKProxyFactory.getProxy(invoker);
    }

    private boolean isJvm(URL tmpUrl) {
        // 如果指定了使用 url 直连的功能，那么就不能使用本地调用
        if (!StringUtils.isEmpty(url))
            return false;
        // 如果用户指定使用本地的方法，就进行本地调用
        return RpcConfig.SCOPE_LOCAL.equals(tmpUrl.getParameter(RpcConfig.SCOPE_KEY));
    }

    public synchronized void destroy(){
        if (destroyed){
            return;
        }

        if (invoker != null){
            try {
                invoker.destroy();
            } catch (Exception e) {
                logger.error("failed to destroy the invoker, caused by " + e.getMessage());
            }
        }

        destroyed = true;
        invoker = null;
    }



}
