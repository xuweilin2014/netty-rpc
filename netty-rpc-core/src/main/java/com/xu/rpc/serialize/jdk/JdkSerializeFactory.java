package com.xu.rpc.serialize.jdk;

import com.xu.rpc.serialize.AbstractSerializeFactory;
import com.xu.rpc.serialize.Serialize;

public class JdkSerializeFactory extends AbstractSerializeFactory {

    @Override
    protected Serialize createSerialize() {
        return new JdkSerialize();
    }

}
