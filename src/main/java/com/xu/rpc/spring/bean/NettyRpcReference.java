package com.xu.rpc.spring.bean;

import com.sun.glass.ui.Application;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.spring.config.ReferenceConfig;
import org.apache.log4j.Logger;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactoryUtils;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * 这个自定义类实现了InitializingBean和FactoryBean两个接口。在Spring IoC容器中实例化了Bean之后，就会对Bean进行初始化操作，
 * 比如当一个bean实现了InitializingBean接口之后，就会回调afterProperties方法。在NettyRpcReference#afterProperties方法会
 * 和RPC服务器建立一个长连接，注意，在client.xml中使用了多少次<nettyrpc:reference>标签，就会调用多少次afterProperties方法，也就会
 * 与RPC服务器建立多少个长连接，并且把这个MessageSendHandler保存到RpcServerLoader的属性中。
 *
 * 实现FactoryBean接口的作用举例说明，比如此NettyRpcReference中保存的interfaceName为AddCalculate，那么当调用ctx.getBean()返回的时候，
 * 就会调用getObject方法，而不是返回NettyRpcReference这个Bean。在getObject方法中，返回的是实现了interfaceName的代理对象，当代理对象调用
 * 方法时，真正调用的是实现了InvocationHandler的MessageSendProxy对象的invoke方法
 */

public class NettyRpcReference extends ReferenceConfig implements FactoryBean, ApplicationContextAware {

    private static final Logger logger = Logger.getLogger(NettyRpcReference.class);

    @Override
    public Object getObject() throws Exception {
        return get();
    }

    @Override
    public Class<?> getObjectType() {
        if (interfaceClass != null)
            return interfaceClass;

        try {
            // 获取到 interfaceName 所指定的接口对应的类对象
            // 由于要加载的 interfaceName 类，此框架的类加载器中不存在，所以必须使用 ContextClassLoader，其实也就是用户的系统类加载器
            if (interfaceName != null && interfaceName.length() > 0)
                interfaceClass = Thread.currentThread().getContextClassLoader().loadClass(interfaceName);
        } catch (ClassNotFoundException e) {
            logger.error("spring analyze fail, cannot load class " + interfaceName);
        }

        return interfaceClass;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        setConsumerSide(true);
        super.setApplicationContext(applicationContext);
    }
}
