package com.xu.rpc.spring.parser;

import com.xu.rpc.spring.bean.*;
import org.springframework.beans.factory.xml.NamespaceHandlerSupport;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import com.google.common.io.CharStreams;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;


public class NettyRpcNamespaceHandler extends NamespaceHandlerSupport {

    @Override
    public void init() {
        registerBeanDefinitionParser("service", new NettyRpcBeanDefinitionParser(NettyRpcService.class));
        registerBeanDefinitionParser("registry", new NettyRpcBeanDefinitionParser(NettyRpcRegistry.class));
        registerBeanDefinitionParser("reference", new NettyRpcBeanDefinitionParser(NettyRpcReference.class));
        registerBeanDefinitionParser("protocol", new NettyRpcBeanDefinitionParser(NettyRpcProtocol.class));
        registerBeanDefinitionParser("application", new NettyRpcBeanDefinitionParser(NettyRpcApplication.class));
    }
}

