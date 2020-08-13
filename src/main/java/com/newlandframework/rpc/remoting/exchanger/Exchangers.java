package com.newlandframework.rpc.remoting.exchanger;

import com.newlandframework.rpc.remoting.handler.ChannelHandler;
import com.newlandframework.rpc.remoting.server.HeaderExchangeServer;
import com.newlandframework.rpc.remoting.server.NettyServer;
import com.newlandframework.rpc.util.URL;

public class Exchangers {

    public static HeaderExchangeServer bind(URL url, ChannelHandler handler){
        return new HeaderExchangeServer(new NettyServer(url, handler));
    }

}
