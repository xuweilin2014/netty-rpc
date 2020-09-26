package com.xu.rpc.spring.bean;

import com.xu.rpc.core.extension.Attribute;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class NettyRpcApplication {
    // 是否开启监控：true/false，默认为true
    @Attribute
    private String metrics;
    // 应用的名称
    @Attribute
    private String name;
    // jmx 的启动端口
    @Attribute
    private String metricsPort;
    // 服务能力展示的端口
    @Attribute
    private String echoPort;
}
