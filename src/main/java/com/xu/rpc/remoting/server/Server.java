package com.xu.rpc.remoting.server;

import com.xu.rpc.remoting.client.EndPoint;
import com.xu.rpc.remoting.exchanger.RpcChannel;
import io.netty.channel.Channel;

import java.util.List;

public interface Server extends EndPoint {

    public List<RpcChannel> getChannels();

}
