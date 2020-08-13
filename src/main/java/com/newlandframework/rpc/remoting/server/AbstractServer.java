package com.newlandframework.rpc.remoting.server;

import com.newlandframework.rpc.remoting.handler.ChannelHandler;
import com.newlandframework.rpc.util.URL;

public abstract class AbstractServer {

    protected ChannelHandler handler;

    protected URL url;

    public AbstractServer(URL url, ChannelHandler handler){
        this.url = url;
        this.handler = handler;
    }

    public abstract void doOpen();

    public abstract void doClose();

}
