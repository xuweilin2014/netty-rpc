package com.xu.rpc.remoting.handler;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.model.MessageResponse;
import com.xu.rpc.remoting.exchanger.RpcChannel;
import io.netty.channel.Channel;
import org.apache.log4j.Logger;

public class HeartbeatHandler extends AbstractHandlerDelegate {

    private static final Logger logger = Logger.getLogger(HeartbeatHandler.class);

    public HeartbeatHandler(ChannelHandler handler) {
        super(handler);
    }

    @Override
    public void received(RpcChannel channel, Object message) throws RemotingException {
        setReadTimestamp(channel);
        if (message instanceof MessageRequest && ((MessageRequest) message).isHeartbeat()){
            MessageResponse response = new MessageResponse();
            response.setHeartbeat(true);
            try {
                logger.info("heartbeat request packet is received, heartbeat response will be sent to " + channel.getRemoteAddress());
                channel.send(response);
            } catch (Exception e) {
                logger.warn("error occurs when sending heartbeat response.");
            }

            return;
        }

        if (message instanceof MessageResponse && ((MessageResponse) message).isHeartbeat()){
            logger.warn("heartbeat response packet is received from " + channel.getRemoteAddress());
            return;
        }

        super.received(channel, message);
    }

}
