package com.xu.rpc.remoting.exchanger;

import com.xu.rpc.commons.exception.RemotingException;
import com.xu.rpc.remoting.server.HeaderExchangeServer;
import com.xu.rpc.remoting.client.HeaderExchangeClient;
import com.xu.rpc.remoting.client.NettyClient;
import com.xu.rpc.remoting.handler.ChannelHandler;
import com.xu.rpc.remoting.server.NettyServer;
import com.xu.rpc.commons.URL;

public class Exchangers {

    public static HeaderExchangeServer bind(URL url, ChannelHandler handler){
        return new HeaderExchangeServer(new NettyServer(url, handler));
    }

    public static HeaderExchangeClient connect(URL url, ChannelHandler handler) throws RemotingException {
        return new HeaderExchangeClient(new NettyClient(url, handler));
    }

}

