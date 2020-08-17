package com.newlandframework.rpc.remoting.server;

import com.newlandframework.rpc.remoting.handler.ChannelHandler;
import com.newlandframework.rpc.util.URL;

public abstract class AbstractServer implements Server{

    protected ChannelHandler handler;

    protected URL url;

    public AbstractServer(URL url, ChannelHandler handler){
        this.url = url;
        this.handler = handler;
        start();
    }

    public void start(){
        // TODO: 2020/8/16
        doOpen();
    }

    public void close(){
        // TODO: 2020/8/16
        doClose();
    }

    public abstract void doOpen();

    public abstract void doClose();

}
