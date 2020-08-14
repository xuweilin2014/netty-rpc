package com.newlandframework.rpc.remoting.handler;

import com.newlandframework.rpc.exception.RemotingException;
import io.netty.channel.Channel;

public interface ChannelHandler {

    public void sent(Channel channel, Object message) throws RemotingException;

    public void received(Channel channel, Object message) throws RemotingException;

    public void connected(Channel channel) throws RemotingException;

    public void disconnected(Channel channel) throws RemotingException;

}
