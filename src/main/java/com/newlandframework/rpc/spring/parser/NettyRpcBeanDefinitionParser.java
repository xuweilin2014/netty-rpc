package com.newlandframework.rpc.spring.parser;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.w3c.dom.Element;

import java.util.*;

public class NettyRpcBeanDefinitionParser implements BeanDefinitionParser {


    public static final Logger logger = Logger.getLogger(NettyRpcBeanDefinitionParser.class);

    private static final List<String> serviceAttributes = Arrays.asList("id", "interfaceName", "ref", "filter", "url", "scope", "registry", "protocol");

    private static final List<String> registryAttributes = Arrays.asList("id", "address", "name");

    private static final List<String> referenceAttributes = Arrays.asList("id", "registry", "interface", "timeout", "retries", "loadbalance", "async", "cluster",
            "heartbeat", "heartbeat.timeout", "stub", "scope");

    private static final List<String> protocolAttributes = Arrays.asList("id", "name", "port", "serialize", "host");

    private static final Map<String, List<String>> attrs = new HashMap<>();

    static{
        attrs.put("service", serviceAttributes);
        attrs.put("registry", registryAttributes);
        attrs.put("reference", referenceAttributes);
        attrs.put("protocol", protocolAttributes);
    }

    private Class<?> beanClass;

    public NettyRpcBeanDefinitionParser(Class<?> beanClass) {
        this.beanClass = beanClass;
    }

    private BeanDefinition createBeanDefinition(Element element, ParserContext parserContext, Class<?> beanDefClass){
        if (beanDefClass == null)
            throw new IllegalArgumentException("bean definition class should not be null");

        String tagName = beanDefClass.getSimpleName().substring(8).toLowerCase();
        List<String> attributes = attrs.get(tagName);

        if (attributes == null || attributes.size() == 0){
            logger.warn("tag " + tagName + " doesn't have any attributes");
            throw new IllegalStateException("tag " + tagName + " doesn't have any attributes");
        }

        RootBeanDefinition beanDefinition = new RootBeanDefinition();
        beanDefinition.setBeanClass(beanClass);
        beanDefinition.setLazyInit(false);

        String id = null;
        for (String name : attributes) {
            String value = element.getAttribute(name);
            beanDefinition.getPropertyValues().addPropertyValue(name, value);
            if ("id".equals(name)){
                id = value;
            }
        }

        parserContext.getRegistry().registerBeanDefinition(id, beanDefinition);

        return beanDefinition;
    }

    @Override
    public BeanDefinition parse(Element element, ParserContext parserContext) {
        return createBeanDefinition(element, parserContext, beanClass);
    }

}
