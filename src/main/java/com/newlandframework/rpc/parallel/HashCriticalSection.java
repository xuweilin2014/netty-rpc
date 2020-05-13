package com.newlandframework.rpc.parallel;

import com.newlandframework.rpc.core.RpcSystemConfig;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;

/**
 * HashCriticalSection的作用为以下两个：
 * 1.将键值key通过fnv算法映射到partition个哈希分段中的某一个上
 * 2.控制多线程对某一个哈希分段的互斥访问（通过enter、exit方法）
 */
public class HashCriticalSection {
    // partition为哈希分段加锁算法中，哈希分段的个数，通过环境变量nettyrpc.jmx.metrics.hash.nums进行设定，默认为8个哈希桶。
    // 当然，可以改成其它大于1的整数，数值越大，哈希冲突越小，JMX监控的性能越好，但是代价是，JVM堆内存空间有所损耗
    private static Integer partition;

    private final Map<Integer, Semaphore> criticalSectionMap = new ConcurrentHashMap<>();

    public final static long BASIC = 0xcbf29ce484222325L;
    public final static long PRIME = 0x100000001b3L;

    public HashCriticalSection() {
        boolean fair = RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_LOCK_FAIR == 1;
        init(RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_HASH_NUMS, fair);
    }

    public HashCriticalSection(Integer counts, boolean fair) {
        init(counts, fair);
    }

    //hash方法将key值映射到partition个哈希段中的一个
    public static int hash(String key) {
        return Math.abs((int) (fnv1a64(key) % partition));
    }

    public static long fnv1a64(String key) {
        long hashCode = BASIC;
        for (int i = 0; i < key.length(); ++i) {
            char ch = key.charAt(i);

            if (ch >= 'A' && ch <= 'Z') {
                ch = (char) (ch + 32);
            }

            hashCode ^= ch;
            hashCode *= PRIME;
        }

        return hashCode;
    }

    private void init(Integer counts, boolean fair) {
        // counts为默认为8个，不过用户也可以自己指定分片个数
        partition = counts;
        // partition个哈希分段，每个哈希分段都有一个Semaphore来控制线程互斥访问
        for (int i = 0; i < partition; i++) {
            criticalSectionMap.put(i, new Semaphore(1, fair));
        }
    }

    public void enter(int hashKey) {
        // 当一个线程要访问一个哈希段时，就要获取到此哈希段对应的semaphore，并且使用这个semaphore来进行互斥访问
        Semaphore semaphore = criticalSectionMap.get(hashKey);
        try {
            semaphore.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void exit(int hashKey) {
        // 当一个线程不再访问一个哈希段时，获取到此哈希段对应的semaphore，然后将其释放
        Semaphore semaphore = criticalSectionMap.get(hashKey);
        semaphore.release();
    }
}

