package com.xu.rpc.spring.parser;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.extension.Attribute;
import com.xu.rpc.spring.bean.NettyRpcParameter;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.w3c.dom.Element;

import java.lang.reflect.Field;
import java.util.*;

public class NettyRpcBeanDefinitionParser implements BeanDefinitionParser {

    private static final String PREFIX = "NettyRpc";

    private static long GLOBAL_ID = 1;

    private Class<?> beanClass;

    public NettyRpcBeanDefinitionParser(Class<?> beanClass) {
        this.beanClass = beanClass;
    }

    private BeanDefinition createBeanDefinition(Element element, ParserContext parserContext, Class<?> beanDefClass){
        if (beanDefClass == null)
            throw new IllegalArgumentException("bean definition class should not be null");

        // 获取到标签的名字，比如 NettyRpcApplication 的标签名为 application
        String tagName = beanDefClass.getSimpleName().substring(PREFIX.length()).toLowerCase();
        RootBeanDefinition beanDefinition = new RootBeanDefinition();
        beanDefinition.setBeanClass(beanClass);
        beanDefinition.setLazyInit(false);

        String id = null;
        // beanDefClass 类中的成员变量如果有 @Attribute 属性，那么表明其可以在 xml 文件中进行配置，
        // 所以自动的将其值装配到 beanDefinition 中
        while (beanDefClass!= Object.class){
            Field[] fields = beanDefClass.getDeclaredFields();
            for (Field field : fields) {
                if (field.isAnnotationPresent(Attribute.class)){
                    String name = field.getName();
                    String value = element.getAttribute(name);
                    beanDefinition.getPropertyValues().addPropertyValue(name, value);
                    // 如果属性值为 id，那么就将其保存到变量 id 中
                    if (RpcConfig.ID_KEY.equals(name)){
                        id = value;
                    }
                }
            }
            beanDefClass = beanDefClass.getSuperclass();
        }

        parserContext.getRegistry().registerBeanDefinition(StringUtils.isEmpty(id) ? String.valueOf(GLOBAL_ID++) : id, beanDefinition);

        return beanDefinition;
    }

    @Override
    public BeanDefinition parse(Element element, ParserContext parserContext) {
        return createBeanDefinition(element, parserContext, beanClass);
    }

}
