package com.xu.rpc.commons.cache.threadlocal;

import com.xu.rpc.commons.cache.Cache;

public class ThreadLocalCache<K, V> implements Cache<K, V> {

    public ThreadLocalCache(int capacity){

    }

    @Override
    public V get(K key) {
        return null;
    }

    @Override
    public void put(K key, V value) {

    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    public int capacity() {
        return 0;
    }

    @Override
    public void clear() {

    }

    @Override
    public void removeLast() {

    }
}
