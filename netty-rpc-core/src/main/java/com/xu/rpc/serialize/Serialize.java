package com.xu.rpc.serialize;

import com.xu.rpc.commons.exception.RpcException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;


public interface Serialize {

    <T> byte[] serialize(T obj) throws RpcException;

    <T> T deserialize(byte[] data, Class<T> cls) throws RpcException;
}

