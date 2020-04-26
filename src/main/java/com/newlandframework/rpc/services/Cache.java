package com.newlandframework.rpc.services;


public interface Cache {

    void put(Object key, Object value);

    Object get(Object key);

}

