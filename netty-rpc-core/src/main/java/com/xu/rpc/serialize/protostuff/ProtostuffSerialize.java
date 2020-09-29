package com.xu.rpc.serialize.protostuff;

import com.dyuproject.protostuff.LinkedBuffer;
import com.dyuproject.protostuff.ProtostuffIOUtil;
import com.dyuproject.protostuff.Schema;
import com.dyuproject.protostuff.runtime.RuntimeSchema;
import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.serialize.Serialize;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ProtostuffSerialize implements Serialize {

    private static Map<Class<?>, Schema<?>> cachedSchema = new ConcurrentHashMap<>();

    @SuppressWarnings("unchecked")
    @Override
    public <T> byte[] serialize(T obj) {
        Class<T> cls = (Class<T>) obj.getClass();
        LinkedBuffer buffer = LinkedBuffer.allocate(LinkedBuffer.DEFAULT_BUFFER_SIZE);
        try {
            Schema<T> schema = getSchema(cls);
            return ProtostuffIOUtil.toByteArray(obj, schema, buffer);
        } catch (Exception e) {
            throw new RpcException("error occurs in protostuff serialization, caused by " + e.getMessage());
        } finally {
            buffer.clear();
        }
    }

    @Override
    public  <T> T deserialize(byte[] data, Class<T> cls) {
        try {
            T t = cls.newInstance();
            Schema<T> schema = getSchema(cls);
            ProtostuffIOUtil.mergeFrom(data, t, schema);
            return t;
        } catch (Exception e) {
            throw new RpcException("error occurs in protostuff deserialization, caused by " + e.getMessage());
        }
    }

    @SuppressWarnings("unchecked")
    private static <T> Schema<T> getSchema(Class<T> cls) {
        Schema<T> schema = (Schema<T>) cachedSchema.get(cls);
        if (schema == null) {
            schema = RuntimeSchema.createFrom(cls);
            if (schema != null) {
                cachedSchema.put(cls, schema);
            }
        }
        return schema;
    }

}
