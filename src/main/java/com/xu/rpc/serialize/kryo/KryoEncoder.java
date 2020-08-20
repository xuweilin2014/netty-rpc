package com.xu.rpc.serialize.kryo;

import com.xu.rpc.serialize.MessageCodecUtil;
import com.xu.rpc.serialize.MessageEncoder;

public class KryoEncoder extends MessageEncoder {

    public KryoEncoder(MessageCodecUtil util) {
        super(util);
    }
}

