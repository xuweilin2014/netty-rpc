package com.newlandframework.rpc.spring.config;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.core.proxy.JDKProxyFactory;
import com.newlandframework.rpc.protocol.Exporter;
import com.newlandframework.rpc.protocol.Invoker;
import com.newlandframework.rpc.protocol.Protocol;
import com.newlandframework.rpc.util.AdaptiveExtensionUtil;
import com.newlandframework.rpc.util.ReflectionUtil;
import com.newlandframework.rpc.spring.bean.NettyRpcProtocol;
import com.newlandframework.rpc.util.URL;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.context.ApplicationContext;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Getter
@Setter
public class ServiceConfig extends AbstractConfig{

    private static final Logger logger = Logger.getLogger(ServiceConfig.class);

    protected String id;
    protected String interfaceName;
    protected String protocol;
    protected String ref;
    protected String filter;
    protected String scope;
    protected String url;
    protected String registry;

    protected boolean exported;

    private String path;

    public ApplicationContext applicationContext;

    private Class<?> interfaceClass;
    
    // 在 unexport 的过程中，对 exporters 中的每一个 exporter 都进行 unexport 操作
    private List<Exporter> exporters;

    public synchronized void export(){
        // 如果已经导出过了直接返回
        if (exported){
            return;
        }
        exported = true;
        if (interfaceName == null || interfaceName.length() == 0)
            throw new IllegalStateException("in tag <nettyrpc:service/> interfaceName cannot be null");

        try {
            interfaceClass = Class.forName(interfaceName);
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException(e.getMessage(), e);
        }

        if (path == null || path.length() == 0)
            path = interfaceName;

        doExportUrls();
    }

    // 将服务按照每一个协议注册到所有的注册中心上去
    private void doExportUrls() {
        // 获取注册中心的地址
        List<URL> registries = getRegistries(registry);
        String[] protocolIds = protocol.split(",");

        for (String id : protocolIds) {
            NettyRpcProtocol protocol = (NettyRpcProtocol) applicationContext.getBean(id);
            if (protocol == null)
                throw new IllegalStateException("protocol with id " + id + " is not configured.");
            checkProtocol(protocol);
            // 将这个服务使用某个协议注册到所有的注册中心上去
            doExportUrlsFor1Protocol(protocol, registries);
        }
    }

    private void doExportUrlsFor1Protocol(NettyRpcProtocol protocol, List<URL> registries) {
        String name = protocol.getName();
        Map<String, String> map = new HashMap<>();

        String[] methodNames = ReflectionUtil.getMethodNames(interfaceClass);
        if (methodNames.length == 0){
            logger.warn(interfaceName + " does not have any method.");
            map.put("methods", "");
        }else {
            map.put("methods", StringUtils.join(methodNames, ','));
        }

        // 获取协议所使用的序列化方式
        String serialize = protocol.getSerialize();
        map.put(RpcSystemConfig.SERIALIZE, serialize);

        String host = this.getHostAddress(protocol);
        int port = Integer.parseInt(protocol.getPort());
        URL url = new URL(name, host, port, path, map);

        String scope = url.getParameter("scope");

        // 当 scope 为 remote 的时候，只导出到远程，不导出到本地
        if (!RpcSystemConfig.SCOPE_REMOTE.equals(scope))
            doExportLocal(protocol, url);

        // 当 scope 为 local 的时候，只导出到本地，不导出到远程
        if (!RpcSystemConfig.SCOPE_LOCAL.equals(scope))
            doExportRemote(protocol, url, registries);
    }

    // 使用 protocol 协议将服务导出到地址为 registries 中所有的注册中心上去
    private void doExportRemote(NettyRpcProtocol protocol, URL url, List<URL> registries) {
        if (protocol.getName() == null)
            throw new IllegalStateException("protocol name cannot be null");
        // 导出服务到远程的时候，<nettyrpc:service/> 中配置的协议里面不能包含 injvm 协议
        if (RpcSystemConfig.INJVM.equals(protocol.getName()))
            throw new IllegalStateException("cannot configure injvm protocol when scope attribute is "
                    + scope + ", in " + id + " <nettyrpc:service/>");

        if (logger.isInfoEnabled())
            logger.info("export service " + interfaceName + " to " + url.toFullString());

        for (URL registryURL : registries) {
            Protocol regProtocol = AdaptiveExtensionUtil.getProtocolExtension(registryURL);
            try {
                Object proxy = interfaceClass.newInstance();
                // 先导出 AbstractProxyInvoker 子类的对象，用于具体的执行客户端要调用的方法
                // 将 url 和 registryURL 编码在一起，以便后面获取到 providerURL 和 registryURL
                Invoker invoker = JDKProxyFactory.getInvoker(proxy, registryURL.addParameterAndEncoded(RpcSystemConfig.EXPORT_KEY, url.toFullString()));

                // 将 invoker 导出成为 Exporter，其实就是封装到 Exporter 中。
                // 导出操作在 RegistryProtocol 和 RpcProtocol 中完成。
                // 在 RegistryProtocol 其实只完成 注册到注册中心的工作，而在 RpcProtocol 中，导出为 Exporter，并且启动服务器监听
                Exporter exporter = regProtocol.export(invoker);
            } catch (InstantiationException | IllegalAccessException e) {
                throw new IllegalStateException("cannot instantiate " + interfaceClass.getName() + " object.");
            }
        }
    }

    private void doExportLocal(NettyRpcProtocol protocol, URL url) {
        // 导出服务到本地的时候，<nettyrpc:service/> 中配置的协议只能为 injvm
        // TODO: 2020/8/9  
    }

    // 如果用户配置了合法的 IP 地址，则直接返回，否则，默认使用本机 IP 地址
    public String getHostAddress(NettyRpcProtocol protocol){
        String host = protocol.getHost();

        if (host == null || host.length() == 0){
            try {
                return InetAddress.getLocalHost().toString();
            } catch (UnknownHostException e) {
                throw new IllegalStateException(e.getMessage(), e);
            }
        }else {
            if (URL.IP_PATTER.matcher(host).matches()){
                return host;
            }else{
                throw new IllegalStateException("host address " + host + " is invalid in tag <nettyrpc:protocol/>");
            }
        }
    }
    
    public void unexport(){
        // TODO: 2020/8/9
    }

}
