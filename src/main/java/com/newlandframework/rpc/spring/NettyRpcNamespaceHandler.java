package com.newlandframework.rpc.spring;

import org.springframework.beans.factory.xml.NamespaceHandlerSupport;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import com.google.common.io.CharStreams;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;


public class NettyRpcNamespaceHandler extends NamespaceHandlerSupport {
    static {
        Resource resource = new ClassPathResource("NettyRPC-logo.txt");
        if (resource.exists()) {
            try {
                Reader reader = new InputStreamReader(resource.getInputStream(), "UTF-8");
                String text = CharStreams.toString(reader);
                System.out.println(text);
                reader.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        } else {
            System.out.println("");
            System.out.println(" _      _____ _____ _____ ___  _ ____  ____  ____ ");
            System.out.println("/ \\  /|/  __//__ __Y__ __\\\\  \\///  __\\/  __\\/   _\\");
            System.out.println("| |\\ |||  \\    / \\   / \\   \\  / |  \\/||  \\/||  /  ");
            System.out.println("| | \\|||  /_   | |   | |   / /  |    /|  __/|  \\_ ");
            System.out.println("\\_/  \\|\\____\\  \\_/   \\_/  /_/   \\_/\\_\\\\_/   \\____/");
            System.out.println("[NettyRPC 2.0,Build 2016/10/7,Author:tangjie http://www.cnblogs.com/jietang/]");
            System.out.println("");
        }
    }

    @Override
    public void init() {
        registerBeanDefinitionParser("service", new NettyRpcServiceParser());
        registerBeanDefinitionParser("registry", new NettyRpcRegisteryParser());
        registerBeanDefinitionParser("reference", new NettyRpcReferenceParser());
    }
}

