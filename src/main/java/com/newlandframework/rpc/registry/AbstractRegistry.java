package com.newlandframework.rpc.registry;

import com.newlandframework.rpc.util.URL;
import org.apache.log4j.Logger;

public abstract class AbstractRegistry implements Registry {

    protected static Logger logger = Logger.getLogger(AbstractRegistry.class);

    @Override
    public void register(URL url) {

    }

    @Override
    public void unregister(URL url) {

    }

    @Override
    public void subscribe(URL url) {

    }

    @Override
    public void unsubscribe(URL url) {

    }

    public void destroy(){

    }
}
