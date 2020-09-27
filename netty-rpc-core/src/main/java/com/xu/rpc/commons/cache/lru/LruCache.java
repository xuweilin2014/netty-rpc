package com.xu.rpc.commons.cache.lru;

import com.xu.rpc.commons.cache.Cache;

public interface LruCache<K, V> extends Cache<K, V> {

    public int size();

    public int capacity();

    // 该方法，专门为segmentCache设计，
    public void removeLast();

}
