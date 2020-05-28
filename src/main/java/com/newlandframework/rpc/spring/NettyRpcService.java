package com.newlandframework.rpc.spring;

import com.newlandframework.rpc.event.ServerStartEvent;
import com.newlandframework.rpc.filter.ServiceFilterBinder;
import com.newlandframework.rpc.filter.Filter;
import com.newlandframework.rpc.netty.MessageRecvExecutor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;

/**
 * 表示一个Rpc服务端所能够提供的服务，它具有三个最重要的属性：interfaceName、ref、filter。
 * 1.interfaceName表示这一个接口，这个接口中的方法表示RPC服务器可以提供的服务，由客户端调用，比如AddCalculate中的add方法
 * 2.ref为实现上面接口中方法的类，客户端的调用就由这个ref表示的类来完成，比如AddCalculateImpl
 * 3.filter表示拦截器
 *
 * 在Spring容器启动的时候，首先会对XML文件中的每个bean进行初始化，然后如果此bean实现了ApplicationContextAware接口
 * 的话，就会回调setApplicationContext方法，在NettyRpcService里面，就是获取到ApplicationContext，然后保存起来。
 * 接着，如果一个bean实现了InitializingBean接口，就会回调afterProperties方法，此方法就是获取到handlerMap，然后
 * 把interfaceName对应的binder（保存了可以提供服务的类以及拦截器）保存到handlerMap中。
 */
public class NettyRpcService implements ApplicationContextAware, InitializingBean {
    private String interfaceName;
    private String ref;
    private String filter;
    private ApplicationContext applicationContext;

    @Override
    public void afterPropertiesSet() throws Exception {
        ServiceFilterBinder binder = new ServiceFilterBinder();

        //在Spring IoC容器刚刚启动的时候，就会对scope为singleton且非懒加载的bean进行实例化，然后根据Bean的配置注入属性。
        //随后才是调用bean中的afterProperties方法，所以此时，所有的bean都存在于IoC容器中。而<nettyrpc:service/>标签中的
        //ref和filter值都是其它bean的id值，因此可以从IoC容器中获取，也就是applicationContext.getBean，然后设置到
        //ServiceFilterBinder中，最后保存进handlerMap里面
        if (StringUtils.isBlank(filter) || !(applicationContext.getBean(filter) instanceof Filter)) {
            binder.setObject(applicationContext.getBean(ref));
        } else {
            binder.setObject(applicationContext.getBean(ref));
            binder.setFilter((Filter) applicationContext.getBean(filter));
        }

        //获取到的MessageRecvExecutor在整个Rpc服务器端是唯一的，因此这个类中的handlerMap也是唯一的
        MessageRecvExecutor.getInstance().getHandlerMap().put(interfaceName, binder);
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext)
            throws BeansException {
        this.applicationContext = applicationContext;
    }

    public ApplicationContext getApplicationContext() {
        return applicationContext;
    }

    public String getFilter() {
        return filter;
    }

    public void setFilter(String filter) {
        this.filter = filter;
    }

    public String getRef() {
        return ref;
    }

    public void setRef(String ref) {
        this.ref = ref;
    }

    public String getInterfaceName() {
        return interfaceName;
    }

    public void setInterfaceName(String interfaceName) {
        this.interfaceName = interfaceName;
    }

}
