package com.xu.rpc.commons.cache.threadlocal;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.cache.Cache;
import com.xu.rpc.commons.cache.CacheFactory;

public class ThreadLocalFactory implements CacheFactory {

    public static final String NAME = "threadlocal";

    @Override
    public Cache createCache(URL url) {
        return new ThreadLocalCache();
    }

}
