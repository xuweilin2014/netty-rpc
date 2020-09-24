package com.xu.rpc.remoting.exchanger;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.parallel.NamedThreadFactory;
import com.xu.rpc.remoting.client.Client;
import com.xu.rpc.remoting.client.EndPoint;
import com.xu.rpc.remoting.server.Server;
import com.xu.rpc.commons.URL;
import io.netty.channel.Channel;
import io.netty.util.Attribute;
import org.apache.log4j.Logger;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

public abstract class HeartbeatExchangeEndpoint{

    private static final Logger logger = Logger.getLogger(HeartbeatExchangeEndpoint.class);

    private static final ScheduledThreadPoolExecutor heartbeatExecutor = new ScheduledThreadPoolExecutor(2, new NamedThreadFactory("RpcHeartbeatThread", true));

    private int heartbeat;

    private EndPoint endPoint;

    private int heartbeatTimeout;

    private ScheduledFuture<?> heartbeatFuture;

    private final AtomicBoolean closed = new AtomicBoolean(false);

    private final URL url;

    public HeartbeatExchangeEndpoint(EndPoint endPoint){
        if (endPoint == null)
            throw new IllegalArgumentException("endpoint == null.");

        this.endPoint = endPoint;
        url = endPoint.getUrl();
        heartbeat = endPoint.getUrl().getParameter(RpcConfig.HEARTBEAT_KEY, RpcConfig.DEFAULT_HEARTBEAT);
        // heartbeat 是强制开启的，如果其小于0，那么就直接设定为默认值
        if (heartbeat <= 0){
            heartbeat = RpcConfig.DEFAULT_HEARTBEAT;
        }
        // heartbeat 的 timeout 时间默认最低是 heartbeat 的 3倍
        heartbeatTimeout = endPoint.getUrl().getParameter(RpcConfig.HEARTBEAT_TIMEOUT_KEY, heartbeat * 3);
        if (heartbeatTimeout < heartbeat * 3){
            heartbeatTimeout = heartbeat * 3;
        }
    }

    protected void startHeartbeat(EndPoint endPoint){
        if (isClosed())
            return;

        stopHeartbeat();
        heartbeatFuture = heartbeatExecutor.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                if (endPoint != null){
                    List<RpcChannel> channels = new ArrayList<>();
                    boolean isServer = false;
                    if (endPoint instanceof Server){
                        channels.addAll(((Server) endPoint).getChannels());
                        isServer = true;
                    }
                    if (endPoint instanceof Client){
                        channels.add(((Client) endPoint).getChannel());
                    }

                    for (RpcChannel channel : channels) {
                        Long lastRead = (Long) channel.getAttribute(RpcConfig.LAST_READ_TIMESTAMP);
                        Long lastWrite = (Long) channel.getAttribute(RpcConfig.LAST_WRITE_TIMESTAMP);
                        long now = System.currentTimeMillis();
                        if (lastRead != null && lastWrite != null){
                            try{
                                // 如果在 heartbeat 时间范围内，连接 channel 上没有发送或者接收数据，那么就会向对方发送一个心跳包
                                if ((now - lastRead >= heartbeat)
                                        || (now - lastWrite >= heartbeat)){
                                    MessageRequest request = new MessageRequest();
                                    request.setHeartbeat(true);
                                    logger.info("send heartbeat packet to " + (isServer ? " client" : " server") + " address: " + channel.getRemoteAddress());
                                    channel.send(request);
                                }

                                // 如果超过 heartbeatTimeout 没有接收到对方发送过来的任何数据，那么就会采取对应措施
                                // 客户端：尝试进行重新连接
                                // 服务端：直接关闭掉连接
                                if (now - lastRead >= heartbeatTimeout){
                                    if (isServer) {
                                        logger.warn("heartbeat timeout, server " + endPoint.getUrl().getAddress() + " will close channel " + channel);
                                        channel.close();
                                    }else {
                                        logger.warn("heart beat timeout, client " + endPoint.getUrl().getAddress() + " will reconnect to server " + channel.getRemoteAddress());
                                        ((Client) endPoint).reconnect();
                                    }
                                }
                            }catch (Throwable t){
                                logger.warn("error occurs when sending heartbeat request to the remote");
                            }
                        }
                    }

                }
            }
        }, heartbeat, heartbeat, TimeUnit.MILLISECONDS);
    }

    private void stopHeartbeat(){
        if (heartbeatFuture != null && !heartbeatFuture.isCancelled()){
            try {
                heartbeatFuture.cancel(true);
                heartbeatExecutor.purge();
            } catch (Throwable e) {
                logger.warn("error occurs when cancelling the heartbeat future, caused by " + e.getMessage());
            }
        }
        heartbeatFuture = null;
    }

    public boolean isClosed() {
        return closed.get();
    }

    public void close(){
        if (closed.compareAndSet(false, true)) {
            stopHeartbeat();
            logger.info("closing heartbeat for " + endPoint.getUrl());
        }
    }


}
