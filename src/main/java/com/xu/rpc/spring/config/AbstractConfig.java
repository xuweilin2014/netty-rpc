package com.xu.rpc.spring.config;

import com.sun.org.apache.regexp.internal.RE;
import com.xu.rpc.core.AbilityDetail;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.extension.Attribute;
import com.xu.rpc.core.extension.ExtensionLoader;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.registry.AbstractRegistryFactory;
import com.xu.rpc.registry.Registry;
import com.xu.rpc.spring.bean.NettyRpcApplication;
import com.xu.rpc.spring.bean.NettyRpcProtocol;
import com.xu.rpc.commons.URL;
import com.xu.rpc.spring.bean.NettyRpcRegistry;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.BeanFactoryUtils;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;

import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

@Getter
@Setter
public abstract class AbstractConfig implements InitializingBean {

    private static final Logger logger = Logger.getLogger(AbstractConfig.class);
    @Attribute(excluded = true)
    protected String id;
    // 服务接口名
    @Attribute(excluded = true)
    protected String interfaceName;
    // 使用的注册中心的id值指明各个注册中心，不指定则将服务注册在所有注册中心上，或者向所有的注册中心进行订阅
    @Attribute
    protected String registry;
    // 缓存种类：lru、cache
    @Attribute
    protected String cache;
    // 缓存的大小容量
    @Attribute
    protected String capacity;
    // 在提供者端：服务使用哪种/哪些协议进行导出,指定协议
    // 在消费者端：消费者只会调用指定协议的服务，其它协议忽略
    @Attribute
    protected String protocol;
    // 超时时间
    @Attribute
    protected String timeout;

    protected List<NettyRpcRegistry> registries;

    protected List<NettyRpcProtocol> protocols;

    protected NettyRpcApplication application;

    protected boolean consumerSide;

    private boolean providerSide;

    private ApplicationContext applicationContext;

    private static final int WAIT_AFTER_REGISTRY = 5000;

    private static final AtomicBoolean destroyed = new AtomicBoolean(false);

    static {
        // graceful shutdown
        Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
            @Override
            public void run() {
                logger.info("gracefully shutdown begins.");
                AbstractConfig.shutdownRpc();
            }
        }));
    }

    // 获取配置的注册中心的地址，在 ServiceConfig 和 ReferenceConfig 中都有可能被引用，所以放到抽象类里面
    public List<URL> getRegistries(){
        List<URL> registryUrls = new ArrayList<>();
        if (registries != null && registries.size() > 0){
            // registries 要么是所有的注册中心，要么是用户在 <nettyrpc:service/> 或者 <nettyrpc:reference/> 中 registry 属性中指明的注册中心
            for (NettyRpcRegistry registry : registries) {
                Map<String, String> map = new HashMap<>();
                // 获取到注册中心的名称，比如：zookeeper、nacos
                map.put(RpcConfig.REGISTRY_KEY, registry.getName());
                // 获取到应用的名字，比如：consumer-of-hello-world-app
                map.put(RpcConfig.APPLICATION_KEY, application.getName());

                String[] address = registry.getAddress().split(RpcConfig.ADDRESS_DELIMITER);
                if (address.length != 2)
                    throw new IllegalStateException("address " + registry.getAddress() + " in <nettyrpc:registry/> is invalid, please check it.");

                URL url = new URL(RpcConfig.REGISTRY_PROTOCOL, address[0], Integer.parseInt(address[1]), Registry.class.getName(), map);
                registryUrls.add(url);
            }
        }

        return registryUrls;
    }

    // 检查 <nettyrpc:protocol/> 标签中的各个属性是否为空，以及是否支持配置的协议类型，
    // 同时检测端口号 port 是否和 jmx port 以及 echo port 一样
    public void checkProtocol(NettyRpcProtocol protocol){
        if (protocol == null){
            throw new IllegalStateException("tag <nettyrpc:protocol/> must be configured.");
        }

        String name = protocol.getName();
        if (StringUtils.isEmpty(name)){
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, name attribute cannot be empty.");
        }

        Protocol ext = ExtensionLoader.getExtensionLoader(Protocol.class).getExtension(name);
        if (ext == null)
            throw new IllegalStateException("protocol " + name + " is not supported yet.");

        String port = protocol.getPort();
        if (StringUtils.isEmpty(port))
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, port attribute cannot be empty.");

        if (String.valueOf(RpcConfig.METRICS_PORT).equals(port)
                || String.valueOf(RpcConfig.ECHO_PORT).equals(port))
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, port cannot be the same as echo port or jmx port.");

        if (protocol.getSerialize() == null || protocol.getSerialize().length() == 0)
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, serialize attribute cannot be empty.");
    }

    public void appendParameters(Object object, Map<String, String> parameters){
        Class<?> cls = object.getClass();
        while (cls != Object.class){
            Field[] fields = cls.getDeclaredFields();
            for (Field field : fields) {
                if (field.isAnnotationPresent(Attribute.class) && !parameters.containsKey(field.getName())){
                    Attribute attribute = field.getAnnotation(Attribute.class);
                    if (!attribute.excluded()){
                        field.setAccessible(true);
                        try {
                            String o = (String) field.get(object);
                            if (o != null && o.length() > 0 && !RpcConfig.FALSE.equals(o)
                                    && !RpcConfig.RPC_DEFAULT.equals(o)){
                                parameters.put(field.getName(), o);
                            }
                        } catch (IllegalAccessException e) {
                            logger.warn("failed to get value " + field.getName() + " in object " + cls.getName());
                        }
                    }
                }
            }
            cls = cls.getSuperclass();
        }
    }

    @Override
    public void afterPropertiesSet(){
        // 根据 <nettyrpc:application/> 设置 application
        if (getApplication() == null && applicationContext != null){
            Map<String, NettyRpcApplication> applicationMap = BeanFactoryUtils.beansOfTypeIncludingAncestors(applicationContext,
                    NettyRpcApplication.class, false, false);
            if (applicationMap.size() > 1)
                throw new IllegalStateException("there is only one application tag allowed in consumer or provider side.");
            if (applicationMap.size() > 0) {
                for (Map.Entry<String, NettyRpcApplication> entry : applicationMap.entrySet()) {
                    setApplication(entry.getValue());
                }
            }
        }

        // 设置服务端或者消费者端的注册中心集合
        if ((getRegistries() == null || getRegistries().size() == 0)
                && applicationContext != null){
            Map<String, NettyRpcRegistry> registryMap = BeanFactoryUtils.beansOfTypeIncludingAncestors(applicationContext,
                    NettyRpcRegistry.class, false, false);
            boolean isRegistryConfigured = false;
            List<String> userRegistries = null;
            // registry 是 <nettyrpc:reference/> 标签中的成员值，可以用来指定从哪些注册中心上订阅或者注册服务，registry 的值为各个注册中心的 id 值，并且用逗号分隔
            // 若用户没有配置，那么就表明从所有的注册中心上订阅服务，也就是所有的 <nettyrpc:registry/> 标签
            if (registry != null && registry.length() > 0) {
                isRegistryConfigured = true;
                userRegistries = Arrays.asList(registry.split(RpcConfig.COMMA_SEPARATOR));
            }

            if (registryMap.size() > 0){
                List<NettyRpcRegistry> registries = new ArrayList<>();
                for (Map.Entry<String, NettyRpcRegistry> entry : registryMap.entrySet()) {
                    String key = entry.getKey();
                    // 如果用户配置了 registry 属性
                    if (isRegistryConfigured){
                        // 只会去从 registry 中指明了的注册中心订阅或者注册服务
                        if (userRegistries.contains(key)){
                            registries.add(entry.getValue());
                        }
                    }else
                        registries.add(entry.getValue());
                }

                setRegistries(registries);
            }
        }

        // 对于服务提供者端，如果在 <nettyrpc:service/> 中配置了 protocol 属性，那么就会使用其中指定协议进行服务导出操作
        // 如果没有配置 protocol 属性，则默认使用所有配置的协议进行服务导出操作
        if (providerSide && (getProtocols() == null || getProtocols().size() == 0)
                && applicationContext != null){
            Map<String, NettyRpcProtocol> protocolMap = BeanFactoryUtils.beansOfTypeIncludingAncestors(applicationContext,
                    NettyRpcProtocol.class, false, false);

            boolean isProtocolConfigured = false;
            List<String> userProtocols = null;
            // protocol 为各个协议的 id 值，用逗号分隔
            if (protocol != null && protocol.length() > 0){
                isProtocolConfigured = true;
                userProtocols = Arrays.asList(protocol.split(RpcConfig.COMMA_SEPARATOR));
            }

            List<NettyRpcProtocol> protocols = new ArrayList<>();
            for (Map.Entry<String, NettyRpcProtocol> entry : protocolMap.entrySet()) {
                String key = entry.getKey();
                // 检测 protocol 的各项属性是否合法
                checkProtocol(entry.getValue());
                if (isProtocolConfigured){
                    if (userProtocols.contains(key)){
                        protocols.add(entry.getValue());
                    }
                }else{
                    protocols.add(entry.getValue());
                }
            }

            setProtocols(protocols);
        }


    }

    public String getHostAddress(){
        return getHostAddress(null);
    }

    // 如果用户配置了合法的 IP 地址，则直接返回，否则，默认使用本机 IP 地址
    public String getHostAddress(String host){
        if (StringUtils.isEmpty(host)){
            try {
                return InetAddress.getLocalHost().getHostAddress();
            } catch (UnknownHostException e) {
                throw new IllegalStateException(e.getMessage(), e);
            }
        }else {
            if (URL.getIpPatter().matcher(host).matches()){
                return host;
            }else{
                throw new IllegalStateException("host address " + host + " is invalid in tag <nettyrpc:protocol/>");
            }
        }
    }

    // 优雅停机
    public static void shutdownRpc(){
        if (destroyed.compareAndSet(false, true)){
            // 销毁 rpc 服务器和所有的注册中心的连接，以及取消注册在注册中心上的节点和监听器
            AbstractRegistryFactory.destroyAll();

            try {
                // 等待消费者接收到注册中心发送的通知，感知到服务提供者的下线
                Thread.sleep(WAIT_AFTER_REGISTRY);
            } catch (InterruptedException e) {
                logger.warn(e.getMessage());
            }

            List<Protocol> protocols = ExtensionLoader.getExtensionLoader(Protocol.class).getExtensions();
            // 对协议流进行注销
            for (Protocol protocol : protocols) {
                if (protocol != null){
                    protocol.destroy();
                }
            }
        }
    }

}
