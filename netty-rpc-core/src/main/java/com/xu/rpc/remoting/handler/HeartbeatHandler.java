package com.xu.rpc.remoting.handler;

import com.xu.rpc.commons.exception.RemotingException;
import com.xu.rpc.remoting.exchanger.RpcChannel;
import com.xu.rpc.core.model.MessageRequest;
import com.xu.rpc.core.model.MessageResponse;
import org.apache.log4j.Logger;

public class HeartbeatHandler extends AbstractHandlerDelegate {

    private static final Logger logger = Logger.getLogger(HeartbeatHandler.class);

    public HeartbeatHandler(ChannelHandler handler) {
        super(handler);
    }

    @Override
    public void received(RpcChannel channel, Object message) throws RemotingException {
        setReadTimestamp(channel);
        // 如果是心跳请求的话，返回一个心跳响应
        if (message instanceof MessageRequest && ((MessageRequest) message).isHeartbeat()){
            MessageResponse response = new MessageResponse();
            response.setHeartbeat(true);
            try {
                logger.debug("heartbeat request packet is received, heartbeat response will be sent to " + channel.getRemoteAddress());
                channel.send(response);
            } catch (Exception e) {
                logger.warn("error occurs when sending heartbeat response.");
            }

            return;
        }
        // 如果是心跳响应的话，则只打印一条日志，然后返回
        if (message instanceof MessageResponse && ((MessageResponse) message).isHeartbeat()){
            logger.debug("heartbeat response packet is received from " + channel.getRemoteAddress());
            return;
        }

        super.received(channel, message);
    }

}
