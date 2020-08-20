package com.xu.rpc.spring.config;

import com.xu.rpc.core.extension.ExtensionLoader;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.spring.bean.NettyRpcProtocol;
import com.xu.rpc.util.URL;
import org.apache.log4j.Logger;

import java.util.List;

public abstract class AbstractConfig {

    private static final Logger logger = Logger.getLogger(AbstractConfig.class);

    protected String id;

    protected String interfaceName;

    protected String registry;

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

}
