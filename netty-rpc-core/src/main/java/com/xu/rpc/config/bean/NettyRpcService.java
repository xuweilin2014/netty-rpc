package com.xu.rpc.config.bean;

import com.xu.rpc.config.ServiceConfig;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

import java.util.Date;


public class NettyRpcService extends ServiceConfig implements ApplicationListener<ContextRefreshedEvent>, DisposableBean,
        ApplicationContextAware, InitializingBean {

    private static final Logger logger = Logger.getLogger(NettyRpcService.class);

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) {
        setProviderSide(true);
        super.setApplicationContext(applicationContext);
    }

    @Override
    public void onApplicationEvent(ContextRefreshedEvent contextRefreshedEvent) {
        if (logger.isInfoEnabled()) {
            logger.info(this + " start up date [" + new Date() + "]");
        }

        if (!exported)
            export();
    }

    @Override
    public void afterPropertiesSet() {
        super.afterPropertiesSet();
        if (ref == null)
            throw new IllegalStateException("ref should not be null " + this);

        if (bean == null){
            bean = getApplicationContext().getBean(ref);
        }
    }

    @Override
    public void destroy() throws Exception {
        unexport();
    }

    @Override
    public String toString() {
        return "NettyRpcService{" +
                "id='" + id + '\'' +
                ", interfaceName='" + interfaceName + '\'' +
                ", ref='" + ref + '\'' +
                ", filter='" + filter + '\'' +
                ", scope='" + scope + '\'' +
                ", registry='" + registry + '\'' +
                '}';
    }
}
