package com.xu.rpc.remoting.server;

public class HeaderExchangeServer implements Server{

    private AbstractServer server;

    public HeaderExchangeServer(AbstractServer server) {
        this.server = server;
    }

    public void close(){
        // TODO: 2020/8/16
        doClose();
    }

    public void doClose(){
        // TODO: 2020/8/16
    }
}
