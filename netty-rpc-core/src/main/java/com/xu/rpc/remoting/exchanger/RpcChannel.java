package com.xu.rpc.remoting.exchanger;

import com.xu.rpc.commons.exception.RemotingException;
import com.xu.rpc.remoting.client.EndPoint;

import java.net.SocketAddress;

public interface RpcChannel extends EndPoint{

    public SocketAddress getRemoteAddress();

    public SocketAddress getLocalAddress();

    public boolean isConnected();

    public void setAttribute(String key, Object value);

    public Object getAttribute(String key);

    public void removeAttribute(String key);

    public void send(Object message) throws RemotingException;

    public void close();

}
