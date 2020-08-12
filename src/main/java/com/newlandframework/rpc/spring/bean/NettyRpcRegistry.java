package com.newlandframework.rpc.spring.bean;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.jmx.ModuleMetricsHandler;
import com.newlandframework.rpc.serialize.RpcSerializeProtocol;
import com.newlandframework.rpc.netty.server.MessageRecvExecutor;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;

/**
 * InitializingBean接口：
 * 在Bean所有的属性都被注入之后会去调用这个afterPropertiesSet()方法，其实在依赖注入完成的时候，
 * spring会去检查这个类是否实现了InitializingBean接口，如果实现了InitializingBean接口，就会去调用这个类的
 * afterPropertiesSet()方法。
 *
 * DisposableBean接口：
 * 在对象销毁的时候，会去调用DisposableBean的destroy方法。
 *
 * <nettyrpc:registry/>标签用来配置Rpc服务器启动所必需的一些信息，比如监听的IP地址和端口，服务能力开放的端口，传输数据所使用的
 * 序列化协议，最后会启动JMX服务器
 */
public class NettyRpcRegistry implements InitializingBean, DisposableBean {

    private String id;
    private String address;
    private String name;

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
//        MessageRecvExecutor ref = MessageRecvExecutor.getInstance();
//        // 设置MessageRecvExecutor中的ip地址和端口，默认为 127.0.0.1:18887
//        ref.setServerAddress(ipAddr);
//        // 设置服务端服务能力展示开放的端口：默认为 18886
//        ref.setEchoApiPort(Integer.parseInt(echoApiPort));
//        // 设置序列化协议
//        ref.setSerializeProtocol(Enum.valueOf(RpcSerializeProtocol.class, protocol));
//        // 启动Rpc服务器，使其运行起来
//        ref.start();
//
//        // 如果用户开启了对JMX监控的支持的话，那么就通过ModuleMetricsHandler启动JMX服务器
//        if (RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_SUPPORT) {
//            ModuleMetricsHandler handler = ModuleMetricsHandler.getInstance();
//            handler.start();
//        }
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}


