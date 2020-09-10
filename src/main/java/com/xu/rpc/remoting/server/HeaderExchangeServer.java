package com.xu.rpc.remoting.server;

import com.xu.rpc.remoting.exchanger.HeartbeatExchangeEndpoint;
import com.xu.rpc.commons.URL;
import com.xu.rpc.remoting.handler.ExchangeHandler;
import io.netty.channel.Channel;
import org.apache.log4j.Logger;

import java.util.List;

public class HeaderExchangeServer extends HeartbeatExchangeEndpoint implements Server{

    private static final Logger logger = Logger.getLogger(HeaderExchangeServer.class);

    private final Server server;

    public HeaderExchangeServer(Server server) {
        super(server);
        this.server = server;
        startHeartbeat(server);
    }

    @Override
    public void close(int timeout) {
        if (timeout > 0){
            long start = System.currentTimeMillis();
            // 如果 ExchangeHandler 中的 channels 集合不为空，表明还有任务在服务器端执行，
            // 因此继续等待 timeout 时间，之后进行强制关闭
            while (!ExchangeHandler.getChannels().isEmpty()
                    && System.currentTimeMillis() - start < timeout){
                try {
                    Thread.sleep(10);
                } catch (InterruptedException e) {
                    logger.warn(e.getMessage());
                }
            }
        }
        // 在 timeout 时间内，关闭掉 NettyServer 服务器
        server.close(timeout);
        // 关闭心跳机制
        super.close();
    }

    @Override
    public boolean isClosed() {
        return server.isClosed();
    }

    @Override
    public URL getUrl() {
        return server.getUrl();
    }

    @Override
    public List<Channel> getChannels() {
        return server.getChannels();
    }
}
