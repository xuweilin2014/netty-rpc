package com.xu.rpc.spring.bean;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.jmx.MetricsServer;
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
 * <nettyrpc:registry/>标签用来配置注册中心的信息
 */
public class NettyRpcRegistry implements InitializingBean, DisposableBean {

    // 注册中心的id
    private String id;
    // 注册中心的地址：host:port
    private String address;
    // 注册中心的名字
    private String name;
    // 本地缓存文件的地址
    private String file;

    @Override
    public void destroy() throws Exception {
    }

    @Override
    public void afterPropertiesSet() throws Exception {

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


