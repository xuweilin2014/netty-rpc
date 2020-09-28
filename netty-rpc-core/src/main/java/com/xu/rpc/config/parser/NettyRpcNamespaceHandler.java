package com.xu.rpc.config.parser;

import com.xu.rpc.config.bean.*;
import com.xu.rpc.config.bean.*;
import org.springframework.beans.factory.xml.NamespaceHandlerSupport;


public class NettyRpcNamespaceHandler extends NamespaceHandlerSupport {

    @Override
    public void init() {
        registerBeanDefinitionParser("service", new NettyRpcBeanDefinitionParser(NettyRpcService.class));
        registerBeanDefinitionParser("registry", new NettyRpcBeanDefinitionParser(NettyRpcRegistry.class));
        registerBeanDefinitionParser("reference", new NettyRpcBeanDefinitionParser(NettyRpcReference.class));
        registerBeanDefinitionParser("protocol", new NettyRpcBeanDefinitionParser(NettyRpcProtocol.class));
        registerBeanDefinitionParser("application", new NettyRpcBeanDefinitionParser(NettyRpcApplication.class));
        registerBeanDefinitionParser("parameter", new NettyRpcBeanDefinitionParser(NettyRpcParameter.class));
    }
}

