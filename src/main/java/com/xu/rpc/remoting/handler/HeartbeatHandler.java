package com.xu.rpc.remoting.handler;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.model.MessageResponse;
import io.netty.channel.Channel;
import org.apache.log4j.Logger;

public class HeartbeatHandler extends AbstractHandlerDelegate {

    private static final Logger logger = Logger.getLogger(HeartbeatHandler.class);

    public HeartbeatHandler(ChannelHandler handler) {
        super(handler);
    }

    @Override
    public void received(Channel channel, Object message) throws RemotingException {
        setWriteTimestamp(channel);
        if (message instanceof MessageRequest && ((MessageRequest) message).isHeartbeat()){
            MessageResponse response = new MessageResponse();
            response.setHeartbeat(true);
            try {
                channel.writeAndFlush(response);
                logger.debug("received heartbeat request packet, heartbeat response will be sent.");
            } catch (Exception e) {
                logger.debug("error occurs when sending heartbeat response.");
            }

            return;
        }

        if (message instanceof MessageResponse && ((MessageResponse) message).isHeartbeat()){
            logger.debug("receive heartbeat response packet.");
            return;
        }

        super.received(channel, message);
    }

}
