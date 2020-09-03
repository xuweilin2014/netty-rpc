package com.xu.rpc.remoting.server;

import com.xu.rpc.remoting.exchanger.HeartbeatExchangeEndpoint;
import com.xu.rpc.commons.URL;
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

    public void close(){
        server.close();
        doClose();
    }

    @Override
    public boolean isClosed() {
        return server.isClosed();
    }

    @Override
    public URL getUrl() {
        return server.getUrl();
    }

    public void doClose(){
        try {
            heartbeatExecutor.shutdown();
        } catch (Throwable e) {
            logger.error("error occurs when shutting down the executor, caused by " + e.getMessage());
        }
    }

    @Override
    public List<Channel> getChannels() {
        return server.getChannels();
    }
}
