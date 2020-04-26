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
 * interfaceName表示这个服务所实现的接口的名字，比如AddCalculate
 * ref表示的是真正实现这个接口的类，比如AddCalculateImpl
 * filter表示拦截器
 *
 * 在Spring容器刚刚启动的时候，首先会对容器中的每个bean进行初始化，然后如果此bean实现了ApplicationContextAware接口
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

        if (StringUtils.isBlank(filter) || !(applicationContext.getBean(filter) instanceof Filter)) {
            binder.setObject(applicationContext.getBean(ref));
        } else {
            binder.setObject(applicationContext.getBean(ref));
            binder.setFilter((Filter) applicationContext.getBean(filter));
        }

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
