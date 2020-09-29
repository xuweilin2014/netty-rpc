package com.xu.rpc.serialize.jdk;

import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.serialize.Serialize;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

public class JdkSerialize implements Serialize {

    @Override
    public <T> byte[] serialize(T obj) throws RpcException {
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            ObjectOutputStream oos = new ObjectOutputStream(baos);
            oos.writeObject(obj);
            byte[] bytes = baos.toByteArray();
            baos.close();
            oos.close();
            return bytes;
        } catch (Throwable e) {
            throw new RpcException("error occurs in jdk serialization, caused by " + e.getMessage());
        }
    }

    @Override
    public <T> T deserialize(byte[] data, Class<T> cls) throws RpcException {
        try {
            ByteArrayInputStream bais = new ByteArrayInputStream(data);
            ObjectInputStream ois = new ObjectInputStream(bais);
            Object o = ois.readObject();
            return cls.cast(o);
        } catch (Throwable e) {
            throw new RpcException("error occurs in jdk deserialization, caused by " + e.getMessage());
        }
    }

}
