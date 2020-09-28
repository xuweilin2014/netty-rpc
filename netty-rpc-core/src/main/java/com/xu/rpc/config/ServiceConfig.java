package com.xu.rpc.config;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.util.AdaptiveExtensionUtils;
import com.xu.rpc.commons.util.ReflectionUtils;
import com.xu.rpc.config.bean.NettyRpcParameter;
import com.xu.rpc.config.bean.NettyRpcProtocol;
import com.xu.rpc.protocol.Exporter;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.extension.Attribute;
import com.xu.rpc.core.proxy.JDKProxyFactory;
import com.xu.rpc.commons.*;
import com.xu.rpc.commons.util.*;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;

@Getter
@Setter
public class ServiceConfig<T> extends AbstractConfig{

    private static final Logger logger = Logger.getLogger(ServiceConfig.class);
    // 服务对象实现引用
    @Attribute
    protected String ref;
    // 服务是否开启监控，值为 true/false，默认值为 true，即表示这个服务会开启监控
    @Attribute
    protected String monitor;
    // 服务验证，可接受的值为：true/false/自定义密码, true表示开启服务验证，反之不开启，
    // 默认使用UUID生成密码，用户也可以直接配置指定值作为密码
    @Attribute
    protected String token;
    // 限流器的种类：bucket、token、guava
    @Attribute
    private String limiter;
    // 限流速率，也就是每一秒可以通过的流量
    @Attribute
    private String rate;
    // 此服务提供者的权重，用于负载均衡的计算
    @Attribute
    private String weight;

    protected T bean;

    protected volatile boolean exported;

    private String path;

    private Class<?> interfaceClass;
    // 在 unexport 的过程中，对 exporters 中的每一个 exporter 都进行 unexport 操作
    private List<Exporter<?>> exporters = new CopyOnWriteArrayList<>();

    public synchronized void export(){
        // 如果已经导出过了直接返回
        if (exported){
            return;
        }
        exported = true;
        if (interfaceName == null || interfaceName.length() == 0)
            throw new IllegalStateException("in tag <nettyrpc:service/> interfaceName cannot be null");

        try {
            // 框架的类加载器加载不了 interfaceName 对应的接口，因此使用用户线程中的 ContextClassLoader（违反了双亲委派模型）
            // 这种方式实际上使用到的是 AppClassLoader，也就是系统类加载器
            interfaceClass = Thread.currentThread().getContextClassLoader().loadClass(interfaceName);
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException(e.getMessage(), e);
        }

        if (StringUtils.isEmpty(path))
            path = interfaceName;

        doExportUrls();
    }

    // 将服务按照每一个协议注册到所有的注册中心上去
    private void doExportUrls() {
        // 获取注册中心的 url 地址
        List<URL> registries = getRegistries();
        // 获取到所有的协议
        List<NettyRpcProtocol> protocols = getProtocols();

        for (NettyRpcProtocol protocol : protocols) {
            // 检查协议中的各项属性
            checkProtocol(protocol);
            // 将这个服务使用某个协议注册到所有的或者用户配置的注册中心上去
            doExportUrlsFor1Protocol(protocol, registries);
        }
    }

    private void doExportUrlsFor1Protocol(NettyRpcProtocol protocol, List<URL> registries) {
        String name = protocol.getName();
        Map<String, String> parameters = new HashMap<>();

        // 获取到接口 interfaceClass 类中所有的方法签名集合，并且使用分号 ； 进行分隔
        List<String> methodNames = new ReflectionUtils().getClassMethodSignature(interfaceClass);
        if (methodNames.size() == 0){
            logger.warn(interfaceName + " does not have any method.");
            parameters.put(RpcConfig.METHODS_KEY, "");
        }else {
            parameters.put(RpcConfig.METHODS_KEY, StringUtils.join(methodNames, RpcConfig.SEMICOLON));
        }

        // 获取协议所使用的序列化方式
        String serialize = protocol.getSerialize();
        parameters.put(RpcConfig.SERIALIZE, serialize);

        if (!StringUtils.isEmpty(token) && !RpcConfig.FALSE.equals(token)) {
            // token 的值为 true 的话，使用随机 token 令牌，即使用 UUID 生成
            if (RpcConfig.TRUE.equals(token)){
                parameters.put(RpcConfig.TOKEN_KEY, UUID.randomUUID().toString());
            // token 不为 true 的话，即 token 值本身相当于密码
            }else{
                parameters.put(RpcConfig.TOKEN_KEY, token);
            }
        }

        // 将服务端中所有 <nettyrpc:parameter/> 标签中的 key、value 值保存到 parameters 中
        if (getParameters() != null && getParameters().size() > 0){
            for (NettyRpcParameter parameter : getParameters().values()) {
                if (!StringUtils.isEmpty(parameter.getKey()) && !StringUtils.isEmpty(parameter.getValue())){
                    parameters.put(parameter.getKey(), parameter.getValue());
                }
            }
        }

        // 如果用户没有配置 monitor，就默认设置 monitor 为 true，也就是默认开启监控
        if (StringUtils.isEmpty(monitor)) {
            monitor = RpcConfig.TRUE;
        }
        parameters.put(RpcConfig.MONITOR_KEY, monitor);

        // 将 ServiceConfig 中的属性（也就是 <nettyrpc:service/> 中配置的属性）添加到 parameters 中
        appendParameters(this, parameters);
        // 将 NettyRpcApplication 中的属性（也就是 <nettyrpc:application/> 中配置的属性）添加到 parameters 中
        appendParameters(application, parameters);

        String host = getHostAddress(protocol.getHost());
        int port = Integer.parseInt(protocol.getPort());
        URL url = new URL(name, host, port, path, parameters);

        String scope = url.getParameter(RpcConfig.SCOPE_KEY);

        // 当 scope 为 remote 的时候，只导出到远程，不导出到本地
        // 当 scope 为 local 的时候，只导出到本地，不导出到远程
        // 当用户没有配置 scope 的时候，既导出到本地，又导出到远程，这是默认的选项
        if (!RpcConfig.SCOPE_REMOTE.equals(scope))
            doExportLocal(protocol, url);

        if (!RpcConfig.SCOPE_LOCAL.equals(scope))
            doExportRemote(protocol, url, registries);
    }

    // 使用 protocol 协议将服务导出到地址为 registries 中所有的注册中心上去
    private void doExportRemote(NettyRpcProtocol protocol, URL url, List<URL> registries) {
        if (protocol.getName() == null)
            throw new IllegalStateException("protocol name cannot be null");
        // 导出服务到远程的时候，<nettyrpc:service/> 中配置的协议里面不能包含 injvm 协议
        if (RpcConfig.INJVM_PROTOCOL.equals(protocol.getName())) {
            return;
        }

        if (logger.isInfoEnabled())
            logger.info("export service " + interfaceName + " to " + url.toFullString());

        for (URL registryUrl : registries) {
            Protocol regProtocol = AdaptiveExtensionUtils.getProtocol(registryUrl);
            try {
                // 先导出 AbstractProxyInvoker 子类的对象，用于具体的执行客户端要调用的方法
                // 将 url 和 registryUrl 编码在一起，以便后面获取到 providerUrl 和 registryUrl
                Invoker<T> invoker = JDKProxyFactory.getInvoker(bean,
                        registryUrl.addParameterAndEncoded(RpcConfig.EXPORT_KEY, url.toFullString()), interfaceClass);

                // 将 invoker 导出成为 Exporter，其实就是封装到 Exporter 中。导出操作在 RegistryProtocol 和 RpcProtocol 中完成。
                // 在 RegistryProtocol 其实只完成注册到注册中心的工作，而在 RpcProtocol 中，导出为 Exporter，并且启动服务器监听
                Exporter<?> exporter = regProtocol.export(invoker);
                exporters.add(exporter);
            } catch (Throwable t) {
                throw new IllegalStateException("cannot instantiate " + interfaceClass.getName() + " object, caused by " + t.getMessage());
            }
        }
    }

    private void doExportLocal(NettyRpcProtocol protocol, URL url) {
        // 导出服务到本地的时候，<nettyrpc:service/> 中配置的协议如果为 injvm，则不做处理，如果为其它协议，则将其转换为 injvm 协议
        URL localUrl = url;
        if (!RpcConfig.INJVM_PROTOCOL.equals(protocol.getName())){
            localUrl = URL.valueOf(url.toFullString()).setProtocol(RpcConfig.INJVM_PROTOCOL);
        }

        Protocol protocolLocal = AdaptiveExtensionUtils.getProtocol(localUrl);
        // 通过 JDKProxyFactory 生成一个 invoker，用来真正执行具体的方法，再调用 export 方法将其导出成为一个 exporter
        Exporter<?> exporter = protocolLocal.export(JDKProxyFactory.getInvoker(bean, localUrl, interfaceClass));

        logger.info("export service " + interfaceName + " to " + localUrl);
        exporters.add(exporter);
    }
    
    protected synchronized void unexport(){
        if (!exported){
            return;
        }

        if (exporters.size() > 0){
            for (Exporter<?> exporter : exporters) {
                try {
                    exporter.unexport();
                } catch (Throwable e) {
                    logger.warn("error occurs when trying to unexport exporter " + exporter);
                }
            }
            exporters.clear();
        }
    }

}
