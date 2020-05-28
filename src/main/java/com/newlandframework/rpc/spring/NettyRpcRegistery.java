package com.newlandframework.rpc.spring;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.jmx.ModuleMetricsHandler;
import com.newlandframework.rpc.serialize.RpcSerializeProtocol;
import com.newlandframework.rpc.netty.MessageRecvExecutor;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * InitializingBean接口：
 * 在Bean所有的属性都被注入之后会去调用这个afterPropertiesSet()方法，其实在依赖注入完成的时候，
 * spring会去检查这个类是否实现了InitializingBean接口，如果实现了InitializingBean接口，就会去调用这个类的
 * afterPropertiesSet()方法。
 *
 * DisposableBean接口：
 * 在对象销毁的时候，会去调用DisposableBean的destroy方法。
 */
public class NettyRpcRegistery implements InitializingBean, DisposableBean {
    private String ipAddr;
    private String protocol;
    private String echoApiPort;
    private AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();

    @Override
    public void destroy() throws Exception {
        MessageRecvExecutor.getInstance().stop();

        if (RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_SUPPORT) {
            ModuleMetricsHandler handler = ModuleMetricsHandler.getInstance();
            handler.stop();
        }
    }

    @Override
    public void afterPropertiesSet() throws Exception {
        MessageRecvExecutor ref = MessageRecvExecutor.getInstance();
        // 设置MessageRecvExecutor中的ip地址和端口
        ref.setServerAddress(ipAddr);
        // 设置服务端服务能力展示开放端口
        ref.setEchoApiPort(Integer.parseInt(echoApiPort));
        // 设置序列化协议
        ref.setSerializeProtocol(Enum.valueOf(RpcSerializeProtocol.class, protocol));
        // 启动Rpc服务器，使其运行起来
        ref.start();

        // 如果用户开启了对JMX监控的支持的话，那么就通过ModuleMetricsHandler启动JMX
        if (RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_SUPPORT) {
            ModuleMetricsHandler handler = ModuleMetricsHandler.getInstance();
            handler.start();
        }
    }

    public String getIpAddr() {
        return ipAddr;
    }

    public void setIpAddr(String ipAddr) {
        this.ipAddr = ipAddr;
    }

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    public String getEchoApiPort() {
        return echoApiPort;
    }

    public void setEchoApiPort(String echoApiPort) {
        this.echoApiPort = echoApiPort;
    }
}


