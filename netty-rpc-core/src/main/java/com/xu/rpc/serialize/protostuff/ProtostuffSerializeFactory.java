package com.xu.rpc.serialize.protostuff;

import com.xu.rpc.commons.URL;
import com.xu.rpc.serialize.AbstractSerializeFactory;
import com.xu.rpc.serialize.Serialize;
import com.xu.rpc.serialize.SerializeFactory;

public class ProtostuffSerializeFactory extends AbstractSerializeFactory {

    public static final String NAME = "protostuff";

    @Override
    protected Serialize createSerialize() {
        return new ProtostuffSerialize();
    }
}
