package com.xu.rpc.spring.config;

import com.xu.rpc.core.extension.ExtensionLoader;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.registry.AbstractRegistryFactory;
import com.xu.rpc.spring.bean.NettyRpcApplication;
import com.xu.rpc.spring.bean.NettyRpcProtocol;
import com.xu.rpc.commons.URL;
import lombok.Getter;
import lombok.Setter;
import org.apache.log4j.Logger;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

@Getter
@Setter
public abstract class AbstractConfig {

    private static final Logger logger = Logger.getLogger(AbstractConfig.class);

    protected String id;

    protected NettyRpcApplication application;
    // 服务接口名
    protected String interfaceName;
    // 使用的注册中心的id值，不指定则将服务注册在所有注册中心上，或者向所有的注册中心进行订阅
    protected String registry;
    // 缓存种类：lru、cache
    protected String cache;
    // 缓存的大小容量
    protected String capacity;

    private static final int WAIT_AFTER_REGISTRY = 5000;

    private static final AtomicBoolean destroyed = new AtomicBoolean(false);

    static {
        Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
            @Override
            public void run() {
                logger.info("gracefully shutdown begins.");
                AbstractConfig.shutdownRpc();
            }
        }));
    }

    // 获取注册中心，在 ServiceConfig 和 ReferenceConfig 中都有可能被引用，所以放到抽象类里面
    public List<URL> getRegistries(){
        // TODO: 2020/8/17
        return null;
    }

    // 检查 <nettyrpc:protocol/> 标签中的各个属性是否为空，以及是否支持配置的协议类型
    public void checkProtocol(NettyRpcProtocol protocol){
        if (protocol == null){
            throw new IllegalStateException("tag <nettyrpc:protocol/> must be configured.");
        }

        String name = protocol.getName();
        if (name == null || name.length() == 0){
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, name attribute cannot be empty.");
        }

        Protocol ext = ExtensionLoader.getExtensionLoader(Protocol.class).getExtension(name);
        if (ext == null)
            throw new IllegalStateException("protocol " + name + " is not supported yet.");

        if (protocol.getPort() == null || protocol.getPort().length() == 0)
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, port attribute cannot be empty.");

        if (protocol.getSerialize() == null || protocol.getSerialize().length() == 0)
            throw new IllegalStateException("in tag <nettyrpc:protocol/>, serialize attribute cannot be empty.");
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
