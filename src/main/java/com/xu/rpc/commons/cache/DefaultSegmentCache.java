package com.xu.rpc.commons.cache;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.util.AdaptiveExtensionUtil;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class DefaultSegmentCache<K, V> implements SegmentCache<K, V>{

    private final CacheFactory cacheFactory;

    // 桶的数量，或者说独立缓存的数量，其大小可以任意指定，不一定非要2的整数幂
    private final int segmentCount;

    // 采用分段锁的思路，这里每一个 cache 都有一个 ReadWriteLock, 在操作 cache 时，需要获取对应的 ReadWriteLock
    protected final Segment<K, V>[] caches;

    private static final int DEFAULT_SEGMENT_COUNT = Runtime.getRuntime().availableProcessors();

    private final AtomicBoolean cleared = new AtomicBoolean(false);

    /**
     * 初始化一个 segmentCache 缓存，考虑到缓存可能分布不均匀，故给每个 segment 分配的容量大小均是 capacity 的大小，
     * 实际容量由 SegmentCache 控制，给每个 segment 的容量设置为 capacity 并不会浪费内存，因为并没有实际分配内存空间，仅仅是一个阈值
     *
     * @param segmentCount 分段的数量
     * @param url 总线 url
     */
    @SuppressWarnings("unchecked")
    public DefaultSegmentCache(int segmentCount, URL url) {
        if (segmentCount <= 0) {
            throw new IllegalArgumentException("segmentCount 必须大于0");
        }
        this.segmentCount = segmentCount;
        caches = new Segment[segmentCount];

        cacheFactory = AdaptiveExtensionUtil.getCacheFactory(url);
        for (int i = 0; i < segmentCount; i++) {
            Cache cache =  cacheFactory.createCache(url);
            caches[i] = new Segment<>(cache);
        }
    }

    public DefaultSegmentCache(URL url) {
        this(DEFAULT_SEGMENT_COUNT, url);
    }


    public V get(K key) {
        int place = getSegmentPlace(key);
        Segment<K, V> cache = caches[place];
        return cache.get(key);
    }


    public void put(K key, V value) {
        int place = getSegmentPlace(key);
        Segment<K, V> cache = caches[place];
        if (cache.size() < cache.capacity()) {//小于
            cache.put(key, value);
            weedout(place);
            return;
        }
        // 当 size 大于等于 capacity 时，会对 segment 中的缓存进行清除
        weedout(place);
        // 递归调用自身重新加入缓存，直到成功为止
        put(key, value);
    }

    /**
     * 淘汰键值
     *
     * 当缓存的数量 size 大于等于 capacity 时，会随机选择一个 segment 清除掉它的最后一个
     */
    public void weedout(int place) {
        Segment weedSegment = caches[place];
        while (true) {
            if (weedSegment.size() < weedSegment.capacity()) {
                break;
            }
            if (weedSegment.size() > 0) {
                weedSegment.writeLock().lock();
                try {
                    if (weedSegment.size() < weedSegment.capacity()) {
                        break;
                    }
                    if (weedSegment.size() > 0) {
                        weedSegment.removeLast();
                        break;
                    }
                } finally {
                    weedSegment.writeLock().unlock();
                }
            }
        }
    }

    /**
     * 清空缓存
     */
    public void clear() {
        if (cleared.compareAndSet(false, true)){
            for (Segment<K, V> cache : caches) {
                cache.clear();
            }
        }
    }

    /**
     * 缓存：对真正的 cache 做了一层封装，也就是添加了锁机制，使其变为线程安全
     */
    static final class Segment<K, V> extends ReentrantReadWriteLock implements Cache<K, V> {

        // segment中缓存的具体实现
        private volatile Cache<K, V> cache;

        public Segment(Cache<K, V> cache) {
            this.cache = cache;
        }

        public V get(K key) {
            readLock().lock();
            try {
                return cache.get(key);
            } finally {
                readLock().unlock();
            }
        }

        public void put(K key, V value) {
            writeLock().lock();
            try {
                cache.put(key, value);
            } finally {
                writeLock().unlock();
            }

        }

        public void clear() {
            writeLock().lock();
            try {
                cache.clear();
            } finally {
                writeLock().unlock();
            }

        }

        public int size() {
            return cache.size();
        }

        public int capacity(){
            return cache.capacity();
        }

        public void removeLast() {
            writeLock().lock();
            try {
                cache.removeLast();
            } finally {
                writeLock().unlock();
            }
        }
    }

    // 考虑到事实情况，segmentCount可以任意指定大小，
    private int getSegmentPlace(K key) {
        return key.hashCode() % segmentCount;
    }
}
