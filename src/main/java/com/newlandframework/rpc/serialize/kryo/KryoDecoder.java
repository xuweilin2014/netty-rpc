package com.newlandframework.rpc.serialize.kryo;

import com.newlandframework.rpc.serialize.MessageCodecUtil;
import com.newlandframework.rpc.serialize.MessageDecoder;

public class KryoDecoder extends MessageDecoder {

    public KryoDecoder(MessageCodecUtil util) {
        super(util);
    }
}
