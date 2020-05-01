package com.newlandframework.rpc.spring;

import com.google.common.eventbus.EventBus;
import com.newlandframework.rpc.event.ClientStopEvent;
import com.newlandframework.rpc.event.ClientStopEventListener;
import com.newlandframework.rpc.netty.MessageSendExecutor;
import com.newlandframework.rpc.serialize.RpcSerializeProtocol;
import lombok.Data;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;

@Data
public class NettyRpcReference implements FactoryBean, InitializingBean, DisposableBean {
    private String interfaceName;
    private String ipAddr;
    private String protocol;
    private EventBus eventBus = new EventBus();

    @Override
    public void destroy() throws Exception {
        eventBus.post(new ClientStopEvent(0));
    }

    @Override
    public void afterPropertiesSet() throws Exception {
        // 每一次客户端向服务器端建立一个连接时，都会发起一个长连接，比如调用RPC服务器端的addCalculate和multiCalculate方法，
        // 就会向RPC服务器发起两次连接。不过这样会有一个问题，假设在XML文件中，指定add方法向A服务器发起请求，multi方法向B服务器发起请求，
        // 接着调用下面这行代码setRpcServerLoader，最终会调用到MessageSendInitializeTask中的call方法，分别向服务器A、B建立长连接。
        // 但是由于RpcServerLoader是单例的，因此其中所包含的MessageSendHandler是唯一的。因此，最后设置的MessageSendHandler就是与A、B两台服务器
        // 连接中的一个handler。因此，发送的add方法请求和call方法请求都会通过这个handler发往同一台服务器。但是这个项目设计的只支持
        // 一个客户端与一个服务器端的通信。
        MessageSendExecutor.getInstance().setRpcServerLoader(ipAddr, RpcSerializeProtocol.valueOf(protocol));
        ClientStopEventListener listener = new ClientStopEventListener();
        eventBus.register(listener);
    }

    @Override
    public Object getObject() throws Exception {
        // 返回一个实现了interfaceName的代理对象，调用代理对象的方法时，真正调用的是实现了InvocationHandler的
        // MessageSendProxy对象的invoke方法
        return MessageSendExecutor.getInstance().execute(getObjectType());
    }

    @Override
    public Class<?> getObjectType() {
        try {
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
