package com.xu.rpc.config.bean;

import com.xu.rpc.core.extension.Attribute;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class NettyRpcProtocol {
    // 协议 id
    @Attribute
    private String id;
    // 协议名称
    @Attribute
    private String name;
    // 端口号
    @Attribute
    private String port;
    // 序列化方式
    @Attribute
    private String serialize;
    // 服务导出的ip地址，可选，不进行配置的话，默认为本机地址
    @Attribute
    private String host;
}
