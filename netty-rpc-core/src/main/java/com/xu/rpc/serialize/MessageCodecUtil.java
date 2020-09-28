package com.xu.rpc.serialize;

import io.netty.buffer.ByteBuf;

import java.io.IOException;

public interface MessageCodecUtil {

    final static int MESSAGE_LENGTH = 4;

    void encode(final ByteBuf out, final Object message) throws IOException;

    Object decode(byte[] body) throws IOException;
}
