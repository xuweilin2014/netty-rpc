package com.xu.rpc.serialize;

import com.xu.rpc.commons.URL;
import com.xu.rpc.core.RpcConfig;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public abstract class AbstractSerializeFactory implements SerializeFactory {

    private static final Map<String, Serialize> serializes = new ConcurrentHashMap<>();

    private static final Lock lock = new ReentrantLock();

    @Override
    public Serialize getSerialize(URL url) {
        String serializeKey = url.getParameter(RpcConfig.SERIALIZE_KEY, RpcConfig.PROTOSTUFF_SERIALIZE);

        Serialize serialize = serializes.get(serializeKey.toLowerCase());
        if (serialize == null){
            lock.lock();
            try{
                if (!serializes.containsKey(serializeKey)){
                    serializes.put(serializeKey, createSerialize());
                }
                serialize = serializes.get(serializeKey);
            }finally {
                lock.unlock();
            }
        }

        return serialize;
    }

    protected abstract Serialize createSerialize();
}
