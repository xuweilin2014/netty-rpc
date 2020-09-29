package com.xu.rpc.serialize;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.model.MessageRequest;
import com.xu.rpc.core.model.MessageResponse;
import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.ByteToMessageDecoder;

import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class MessageDecoder extends ByteToMessageDecoder {

    private final Serialize serialize;

    public MessageDecoder(Serialize serialize) {
        this.serialize = serialize;
    }

    @Override
    protected void decode(ChannelHandlerContext ctx, ByteBuf in, List<Object> out) {
        byte type = in.readByte();
        byte[] bytes = new byte[in.readableBytes()];
        in.readBytes(bytes);

        if (type == RpcConfig.REQUEST) {
            out.add(serialize.deserialize(bytes, MessageRequest.class));
        } else if (type == RpcConfig.RESPONSE) {
            out.add(serialize.deserialize(bytes, MessageResponse.class));
        }
    }
}

