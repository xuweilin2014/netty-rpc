package com.xu.rpc.serialize;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.model.MessageRequest;
import com.xu.rpc.core.model.MessageResponse;
import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.MessageToByteEncoder;
import org.apache.log4j.Logger;

public class MessageEncoder extends MessageToByteEncoder<Object> {

    private final Serialize serialize;

    private static final Logger logger = Logger.getLogger(MessageEncoder.class);

    public MessageEncoder(final Serialize serialize) {
        this.serialize = serialize;
    }

    @Override
    protected void encode(final ChannelHandlerContext ctx, final Object msg, final ByteBuf out) throws Exception {
        if (msg instanceof MessageRequest) {
            byte[] bytes = serialize.serialize(msg);
            logger.debug(String.format("Message:%s, 序列化大小为:%d", msg, bytes.length));
            out.writeByte(RpcConfig.REQUEST);
            out.writeBytes(bytes);
        } else if (msg instanceof MessageResponse) {
            byte[] bytes = serialize.serialize(msg);
            logger.debug(String.format("Message:%s, 序列化大小为:%d", msg, bytes.length));
            out.writeByte(RpcConfig.RESPONSE);
            out.writeBytes(bytes);
        }
    }
}

