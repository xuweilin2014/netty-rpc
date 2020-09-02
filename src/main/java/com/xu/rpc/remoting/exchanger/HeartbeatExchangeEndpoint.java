package com.xu.rpc.remoting.exchanger;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.parallel.NamedThreadFactory;
import com.xu.rpc.remoting.client.Client;
import com.xu.rpc.remoting.client.EndPoint;
import com.xu.rpc.remoting.server.Server;
import com.xu.rpc.util.URL;
import io.netty.channel.Channel;
import io.netty.util.Attribute;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

public abstract class HeartbeatExchangeEndpoint{

    private static final Logger logger = Logger.getLogger(HeartbeatExchangeEndpoint.class);

    protected static final ScheduledThreadPoolExecutor heartbeatExecutor = new ScheduledThreadPoolExecutor(2, new NamedThreadFactory("RpcHeartbeatThread", true));

    private int heartbeat;

    private int heartbeatTimeout;

    private ScheduledFuture<?> heartbeatFuture;

    private final AtomicBoolean closed = new AtomicBoolean(false);

    private final URL url;

    public HeartbeatExchangeEndpoint(EndPoint endPoint){
        if (endPoint == null)
            throw new IllegalArgumentException("endpoint == null.");

        url = endPoint.getUrl();
        heartbeat = endPoint.getUrl().getParameter(RpcConfig.HEARTBEAT_KEY, RpcConfig.DEFAULT_HEARTBEAT);
        // heartbeat 是强制开启的，如果其小于0，那么就直接设定为默认值
        if (heartbeat <= 0){
            heartbeat = RpcConfig.DEFAULT_HEARTBEAT;
        }
        // heartbeat 的 timeout 时间默认是 heartbeat 的 3倍
        heartbeatTimeout = endPoint.getUrl().getParameter(RpcConfig.HEARTBEAT_TIMEOUT_KEY, heartbeat * 3);
        if (heartbeatTimeout < heartbeat * 3){
            heartbeatTimeout = heartbeat * 3;
        }
    }

    protected void startHeartbeat(EndPoint endPoint){
        stopHeartbeat();
        heartbeatFuture = heartbeatExecutor.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                if (endPoint != null){
                    List<Channel> channels = new ArrayList<>();
                    boolean isServer = false;
                    if (endPoint instanceof Server){
                        channels.addAll(((Server) endPoint).getChannels());
                        isServer = true;
                    }
                    if (endPoint instanceof Client){
                        channels.add(((Client) endPoint).getChannel());
                    }

                    for (Channel channel : channels) {
                        Attribute<Long> readAttr = channel.attr(RpcConfig.LAST_READ_TIMESTAMP);
                        Attribute<Long> writeAttr = channel.attr(RpcConfig.LAST_WRITE_TIMESTAMP);
                        long now = System.currentTimeMillis();
                        if (readAttr != null && writeAttr != null){
                            try{
                                Long lastRead = readAttr.get();
                                Long lastWrite = writeAttr.get();
                                if ((lastRead != null && (now - lastRead >= heartbeat))
                                        || (lastWrite != null && (now - lastWrite >= heartbeat))){
                                    MessageRequest request = new MessageRequest();
                                    request.setHeartbeat(true);
                                    channel.writeAndFlush(request);
                                }

                                if (lastRead != null && (now - lastRead >= heartbeatTimeout)){
                                    if (isServer) {
                                        channel.close();
                                    }else {
                                        ((Client) endPoint).reconnect();
                                    }
                                }
                            }catch (Throwable t){
                                logger.warn("error occurs when sending heartbeat request to the remote.");
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
            // 如果是客户端关闭的话，什么都不做；如果
            doClose();
        }
    }

    public abstract void doClose();
}
