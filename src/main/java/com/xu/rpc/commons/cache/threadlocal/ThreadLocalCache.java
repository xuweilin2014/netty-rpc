package com.xu.rpc.commons.cache.threadlocal;

import com.xu.rpc.commons.cache.Cache;

import java.util.HashMap;

public class ThreadLocalCache<K, V> implements Cache<K, V> {

    private final ThreadLocal<HashMap<K, V>> cache;

    public ThreadLocalCache(){
        this.cache = new ThreadLocal<HashMap<K, V>>(){
            @Override
            protected HashMap<K, V> initialValue() {
                return new HashMap<>();
            }
        };
    }

    @Override
    public V get(K key) {
        return cache.get().get(key);
    }

    @Override
    public void put(K key, V value) {
        cache.get().put(key, value);
    }

    @Override
    public void clear() {
        cache.get().clear();
    }

}
