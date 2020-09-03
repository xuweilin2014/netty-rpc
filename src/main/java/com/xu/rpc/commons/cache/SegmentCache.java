package com.xu.rpc.commons.cache;

public interface SegmentCache<K, V> {

    V get(K key);

    void put(K key, V value);

    void clear();

}
