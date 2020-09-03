package com.xu.rpc.commons.cache;

public interface Cache<K,V> {

    public V get(K key);

    public void put(K key, V value);

    public int size();

    public int capacity();

    public void clear();

    // 该方法，专门为segmentCache设计，
    public void removeLast();

}
