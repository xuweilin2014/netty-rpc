package com.xu.rpc.spring.bean;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.extension.Attribute;
import com.xu.rpc.jmx.MetricsServer;
import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;

/**
 * <nettyrpc:registry/>标签用来配置注册中心的信息
 */
@Getter
@Setter
public class NettyRpcRegistry {

    // 注册中心的id
    @Attribute
    private String id;
    // 注册中心的地址：host:port
    @Attribute
    private String address;
    // 注册中心的名字
    @Attribute
    private String name;
    // 本地缓存文件的地址
    @Attribute
    private String file;

}


