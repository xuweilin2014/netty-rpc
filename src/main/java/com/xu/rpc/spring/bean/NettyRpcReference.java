package com.xu.rpc.spring.bean;

import com.xu.rpc.exception.RpcException;
import com.xu.rpc.spring.config.ReferenceConfig;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;

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

public class NettyRpcReference extends ReferenceConfig implements FactoryBean, InitializingBean, DisposableBean {

    @Override
    public void destroy() throws RpcException {
        // TODO: 2020/8/17
    }

    @Override
    public void afterPropertiesSet() throws Exception {
        /*
         * 在客户端的Spring IoC容器启动的时候，每一个<nettyrpc:reference/>所定义的bean被初始化时，都会与服务器建立一个长连接。
         * 比如interfaceName属性为addCalculate和multiCalculate接口的标签，被初始化的时候，就会分别向RPC服务器发起一次长连接。
         *
         * 不过这样会有一个问题，向A服务器发起请求，要调用addCalculate中的add方法，同时向B服务器发起请求，调用multiCalculate中的multi方法，
         * 通过下面的setRpcServerLoader，最终会调用到MessageSendInitializeTask中的call方法，分别向服务器A、B建立长连接（每一个连接对应的pipeline中
         * 都有自己的MessageSendHandler对象）。但是由于RpcServerLoader是单例的，导致其中的属性MessageSendHandler是唯一的。因此，最后设置的
         * MessageSendHandler就是与A、B两台服务器连接中的一个handler。因此，发送的add方法请求和call方法请求都会通过这个handler发往同一台服务器。
         * 达不到我们原来的目的
         *
         * 但是这个项目设计的只支持一个客户端与一个服务器端的通信。
         */
        /*MessageSendExecutor.getInstance().setRpcServerLoader(ipAddr, Serialization.valueOf(protocol));*/
    }

    @Override
    public Object getObject() throws Exception {
        return null;
    }

    @Override
    public Class<?> getObjectType() {
        try {
            //获取到interfaceName所指定的接口对应的类对象，比如：
            //interfaceName为com.newlandframework.rpc.services.AddCalculate的话，就返回对应的Class对象
            return this.getClass().getClassLoader().loadClass(interfaceName);
        } catch (ClassNotFoundException e) {
            System.err.println("spring analyze fail!");
        }
        return null;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }
}
