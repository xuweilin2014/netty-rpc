package com.xu.rpc.remoting.exchanger;

import com.xu.rpc.remoting.handler.ChannelHandler;
import com.xu.rpc.remoting.server.HeaderExchangeServer;
import com.xu.rpc.remoting.server.NettyServer;
import com.xu.rpc.util.URL;

public class Exchangers {

    public static HeaderExchangeServer bind(URL url, ChannelHandler handler){
        return new HeaderExchangeServer(new NettyServer(url, handler));
    }

}