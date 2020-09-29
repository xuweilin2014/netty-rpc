package com.xu.rpc.serialize;

import com.xu.rpc.commons.URL;
import com.xu.rpc.core.extension.Extension;
import com.xu.rpc.serialize.protostuff.ProtostuffSerializeFactory;

@Extension(ProtostuffSerializeFactory.NAME)
public interface SerializeFactory {

    Serialize getSerialize(URL url);

}
