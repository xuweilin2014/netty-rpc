package com.xu.rpc.remoting.server;

import com.xu.rpc.remoting.exchanger.HeartbeatExchangeEndpoint;
import com.xu.rpc.util.URL;
import io.netty.channel.Channel;
import org.apache.log4j.Logger;

import java.util.List;

public class HeaderExchangeServer extends HeartbeatExchangeEndpoint implements Server{

    private final Server server;

    private static final Logger logger = Logger.getLogger(HeaderExchangeServer.class);

    public HeaderExchangeServer(Server server) {
        super(server);
        this.server = server;
        startHeartbeat(server);
    }

    public void close(){
        // TODO: 2020/8/16
        doClose();
    }

    @Override
    public boolean isClosed() {
        return false;
    }

    @Override
    public URL getUrl() {
        return null;
    }

    public void doClose(){
        // TODO: 2020/8/16
    }

    @Override
    public List<Channel> getChannels() {
        return null;
    }
}
