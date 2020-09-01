package com.xu.rpc.remoting.client;

import com.xu.rpc.async.RpcFuture;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.util.URL;
import io.netty.channel.Channel;

public interface Client extends EndPoint{

    public void send(Object message) throws RemotingException;

    public Channel getChannel();

    public void reconnect() throws RemotingException;

    public boolean isConnected();
}
