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
import com.xu.rpc.spring.bean.NettyRpcParameter;
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
import java.util.concurrent.ConcurrentHashMap;
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
    // 使用的注册中心的id值指明各个注册中心，不指定则服务提供者将服务注册在所有注册中心上，或者消费者向所有的注册中心进行订阅
    @Attribute
    protected String registry;
    // 缓存种类：lru、cache
    @Attribute
    protected String cache;
    // 在提供者端：服务使用哪种/哪些协议进行导出,指定协议的 id
    // 在消费者端：消费者只会调用指定协议的服务，其它协议忽略，指定允许的协议的名字
    @Attribute
    protected String protocol;
    // 超时时间
    @Attribute
    protected String timeout;
    // 1.引用服务的范围
    // 2.服务导出的范围：remote/local/空值
    //      remote:只导出到远程
    //      local:只导出到本地
    //      空值：既导出到远程，又导出到本地
    @Attribute
    protected String scope;
    // 过滤器
    @Attribute
    protected String filter;

    protected List<NettyRpcRegistry> registries;

    protected List<NettyRpcProtocol> protocols;

    // <nettyrpc:parameter key="" value="" /> 标签的值
    // 键值对为：key$value -> NettyRpcParameter
    // 比如：heartbeat$3000 -> NettyRpcParameter
    protected final Map<String, NettyRpcParameter> parameters = new ConcurrentHashMap<>();

    protected NettyRpcApplication application;
    // 是否位于消费者端
    protected boolean consumerSide;
    // 是否位于提供者端
    protected boolean providerSide;

    private ApplicationContext applicationContext;

    private static final int WAIT_AFTER_REGISTRY = 5000;

    private static final AtomicBoolean destroyed = new AtomicBoolean(false);

    static {
        // graceful shutdown
        Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
            @Override
            public void run() {
                logger.info("gracefully shutdown begins");
                AbstractConfig.shutdownRpc();
            }
        }));
    }

    // 获取配置的注册中心的地址，在 ServiceConfig 和 ReferenceConfig 中都有可能被引用，所以放到抽象类里面
    // 获取到注册中心的所有 url
    protected List<URL> getRegistries(){
        List<URL> registryUrls = new ArrayList<>();
        if (registries != null && registries.size() > 0){
            // registries 要么是所有的注册中心，要么是用户在 <nettyrpc:service/> 或者 <nettyrpc:reference/>
            // 中 registry 属性中指明的注册中心
            for (NettyRpcRegistry registry : registries) {
                Map<String, String> map = new HashMap<>();
                // 获取到注册中心的名称，比如：zookeeper、nacos
                map.put(RpcConfig.REGISTRY_KEY, registry.getName());
                // 获取到应用的名字，比如：consumer-of-hello-world-app
                map.put(RpcConfig.APPLICATION_KEY, application.getName());

                String[] address = registry.getAddress().split(RpcConfig.ADDRESS_DELIMITER);
                if (address.length != 2)
                    throw new IllegalStateException("address " + registry.getAddress() + " in <nettyrpc:registry/> is invalid, please check it");

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
            throw new IllegalStateException("tag <nettyrpc:protocol/> must be configured");
        }

        String name = protocol.getName();
        if (StringUtils.isEmpty(name)){
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, name attribute cannot be empty");
        }

        Protocol ext = ExtensionLoader.getExtensionLoader(Protocol.class).getExtension(name);
        if (ext == null)
            throw new IllegalStateException("protocol " + name + " is not supported yet.");

        String port = protocol.getPort();
        if (StringUtils.isEmpty(port))
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, port attribute cannot be empty.");

        // 检查协议的端口号是否和 metricsPorty 与 echoPort 配置的一样
        String metricsPort = StringUtils.isEmpty(application.getMetricsPort()) ?
                String.valueOf(RpcConfig.METRICS_PORT) : application.getMetricsPort();
        String echoPort = StringUtils.isEmpty(application.getEchoPort()) ?
                String.valueOf(RpcConfig.ECHO_PORT) : application.getEchoPort();
        if (metricsPort.equals(port)
                || echoPort.equals(port))
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, port cannot be the same as echo port or jmx port");

        if (StringUtils.isEmpty(protocol.getSerialize()))
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, serialize attribute cannot be empty.");
    }

    // 将对象 object 及其父类中，所有被 @Attribute 标注的不为空的属性保存到 parameters 中
    public void appendParameters(Object object, Map<String, String> parameters){
        Class<?> cls = object.getClass();
        while (cls != Object.class){
            Field[] fields = cls.getDeclaredFields();
            for (Field field : fields) {
                if (field.isAnnotationPresent(Attribute.class) && !parameters.containsKey(field.getName())){
                    Attribute attribute = field.getAnnotation(Attribute.class);
                    // @Attribute 标签中的 excluded 属性为 true 的话，表示这个属性值不应该保存到 parameters 中
                    if (!attribute.excluded()){
                        field.setAccessible(true);
                        try {
                            String o = (String) field.get(object);
                            // field 的值不为空或者 false/default 的话
                            if (o != null && o.length() > 0 && !RpcConfig.FALSE.equals(o)
                                    && !RpcConfig.RPC_DEFAULT.equals(o)){
                                parameters.put(field.getName(), o);
                            }
                        } catch (IllegalAccessException e) {
                            logger.warn("failed to get value " + field.getName() + " in class " + cls.getName());
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
            // registryMap 中的键值就是配置 <nettyrpc:registry/> 时指定的注册中心的 id 值
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
                        // 只会去从 registry 中指明了的注册中心订阅或者注册服务，其它的注册中心 registry 会被忽略
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

        // 获取服务或者消费者端，在 <nettyrpc:parameter/> 标签中配置的参数值
        if ((getParameters() == null || getParameters().size() == 0) && applicationContext != null){
            Map<String, NettyRpcParameter> map = BeanFactoryUtils.beansOfTypeIncludingAncestors(applicationContext,
                    NettyRpcParameter.class, false, false);
            if (!map.isEmpty()){
                for (Map.Entry<String, NettyRpcParameter> entry : map.entrySet()) {
                    NettyRpcParameter parameter = entry.getValue();
                    if (StringUtils.isEmpty(parameter.getKey()) || StringUtils.isEmpty(parameter.getValue())){
                        throw new IllegalStateException("key or value cannot be empty in tag <nettyrpc:parameter/>");
                    }

                    String key = parameter.getKey() + RpcConfig.HEX_SEPARATOR + parameter.getValue();
                    if (parameters.containsKey(key)){
                        throw new IllegalStateException("value of tag <nettyrpc:parameter/> is duplicated/>");
                    }
                    // parameters 中的键值对为: key + # + value
                    parameters.put(key, parameter);
                }
            }
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
