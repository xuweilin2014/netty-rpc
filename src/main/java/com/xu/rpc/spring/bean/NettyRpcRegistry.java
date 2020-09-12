package com.xu.rpc.spring.bean;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.jmx.MetricsServer;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;

/**
 * <nettyrpc:registry/>标签用来配置注册中心的信息
 */
public class NettyRpcRegistry {

    // 注册中心的id
    private String id;
    // 注册中心的地址：host:port
    private String address;
    // 注册中心的名字
    private String name;
    // 本地缓存文件的地址
    private String file;

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


