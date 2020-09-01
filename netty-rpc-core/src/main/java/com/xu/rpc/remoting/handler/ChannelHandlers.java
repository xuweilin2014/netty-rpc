package com.xu.rpc.remoting.handler;

public class ChannelHandlers {

    public static ChannelHandler wrapHandler(ChannelHandler handler){
        return new HeartbeatHandler(new ExchangeHandler(handler));
    }

}
