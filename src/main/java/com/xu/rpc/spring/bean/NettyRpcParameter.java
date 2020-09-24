package com.xu.rpc.spring.bean;

import com.xu.rpc.core.extension.Attribute;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class NettyRpcParameter {

    // 参数的键
    @Attribute
    private String key;
    // 参数的值
    @Attribute
    private String value;

}
