package com.xu.rpc.remoting.exchanger;

import com.xu.rpc.commons.exception.RemotingException;
import com.xu.rpc.commons.URL;
import io.netty.channel.Channel;
import io.netty.channel.ChannelFuture;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import java.net.SocketAddress;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

public class NettyChannel implements RpcChannel {

    private static final Logger logger = Logger.getLogger(NettyChannel.class);

    private static final Map<Channel, RpcChannel> CHANNELS = new ConcurrentHashMap<>();

    private final Channel channel;

    private final URL url;

    private final AtomicBoolean closed = new AtomicBoolean(false);

    private static final int SYNC_TIMEOUT = 1500;

    private final Map<String, Object> attributes = new ConcurrentHashMap<>();

    public NettyChannel(Channel channel, URL url){
        this.channel = channel;
        this.url = url;
    }

    @Override
    public SocketAddress getLocalAddress() {
        if (channel == null)
            throw new IllegalStateException("channel is null, cannot get local address");

        return channel.localAddress();
    }

    @Override
    public SocketAddress getRemoteAddress() {
        if (channel == null)
            throw new IllegalStateException("channel is null, cannot get remote address");

        return channel.remoteAddress();
    }

    @Override
    public boolean isConnected() {
        if (channel != null && channel.isActive())
            return true;
        return false;
    }

    @Override
    public void setAttribute(String key, Object value) {
        if (StringUtils.isEmpty(key))
            throw new IllegalArgumentException("cannot add attribute when key is empty, value is " + value);

        if (value == null)
            attributes.remove(key);
        else
            attributes.put(key, value);
    }

    @Override
    public Object getAttribute(String key) {
        if (StringUtils.isEmpty(key))
            throw new IllegalArgumentException("cannot get attribute when key is empty");

        return attributes.get(key);
    }

    @Override
    public void removeAttribute(String key) {
        if (StringUtils.isEmpty(key))
            return;

        attributes.remove(key);
    }

    @Override
    public void send(Object message) throws RemotingException {
        if (channel == null)
            throw new IllegalArgumentException("channel is empty, cannot send message to remote");

        if (closed.get() || !channel.isActive()) {
            throw new RemotingException("channel " + channel + " is closed or inactive, cannot send message to remote");
        }

        if (!channel.isWritable()) {
            throw new RemotingException("cannot write message to this channel " + channel);
        }

        boolean success;
        try {
            // 将消息发出
            ChannelFuture future = channel.writeAndFlush(message);
            // 阻塞等待 timeout 时间，直到发送成功或者超时
            success = future.await(SYNC_TIMEOUT);

            if (future.cause() != null)
                throw future.cause();

        } catch (Throwable e) {
            throw new RemotingException("failed to send message from client " + channel.localAddress() + ", caused by " + e.getMessage());
        }

        // 运行到这里时，success 为 false，说明等待发送的时间超时
        if (!success)
            throw new RemotingException("failed to send message from client " + channel.localAddress() + " to remote " + channel.remoteAddress() +
                    " within time " + SYNC_TIMEOUT);
    }

    @Override
    public void close() {
        close(SYNC_TIMEOUT);
    }

    @Override
    public void close(int timeout){
        if (closed.compareAndSet(false, true)){
            CHANNELS.remove(channel);
            try {
                attributes.clear();
            } catch (Exception e) {
                logger.warn(e.getMessage());
            }

            boolean success = false;
            try {
                ChannelFuture future = channel.close();
                success = future.await(timeout);

                if (future.cause() != null)
                    throw future.cause();

                // 超时，还没有关闭掉 channel
                if (!success)
                    logger.error("failed to close the channel " + channel + " within time " + timeout);

                logger.info("channel " + channel + " is closed");
            } catch (Throwable e) {
                logger.error("failed to close the channel " + channel + " caused by " + e.getMessage());
            }
        }
    }

    public static RpcChannel getChannel(Channel channel, URL url){
        if (channel == null)
            return null;

        if (!CHANNELS.containsKey(channel)){
            synchronized (CHANNELS){
                if (!CHANNELS.containsKey(channel)) {
                    NettyChannel nettyChannel = new NettyChannel(channel, url);
                    CHANNELS.put(channel, nettyChannel);
                }
            }
        }

        return CHANNELS.get(channel);
    }

    @Override
    public boolean isClosed() {
        return closed.get();
    }

    @Override
    public URL getUrl() {
        return url;
    }

    @Override
    public String toString() {
        return channel.toString();
    }
}
