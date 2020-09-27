package com.xu.rpc.serialize.kryo;

import com.xu.rpc.serialize.MessageCodecUtil;
import com.xu.rpc.serialize.MessageDecoder;

public class KryoDecoder extends MessageDecoder {

    public KryoDecoder(MessageCodecUtil util) {
        super(util);
    }
}
