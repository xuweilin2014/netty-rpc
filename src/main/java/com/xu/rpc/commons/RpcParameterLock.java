package com.xu.rpc.commons;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class RpcParameterLock implements Lock {

    /**
     * Holds a WeakKeyLockPair for each parameter. The mapping may be deleted upon garbage collection
     * if the canonical key is not strongly referenced anymore (by the threads using the Lock).
     */
    private static final Map<Object, Lock> locks = new WeakHashMap<>();

    private final Object key;

    private final Lock lock;

    private RpcParameterLock(Object key, Lock lock) {
        this.key = key;
        this.lock = lock;
    }

    private static final class WeakKeyLockPair {
        /**
         * The weakly-referenced parameter. If it were strongly referenced, the entries of
         * the lock Map would never be garbage collected, causing a memory leak.
         */
        private final Reference<Object> param;

        /** The actual lock object on which threads will synchronize. */
        private final Lock lock;

        private WeakKeyLockPair (Object param, Lock lock) {
            this.param = new WeakReference<>(param);
            this.lock = lock;
        }
    }

    public static Lock getParameterLock (Object param) {
        if (param == null)
            throw new IllegalArgumentException("param should not be null");

        Object canonical = null;
        Lock lock = null;

        synchronized (RpcParameterLock.class) {
            lock = locks.get(param);
            canonical = param;
            if (lock == null) {
                locks.put(param, new ReentrantLock());
                lock = locks.get(param);
            }
        }

        // ... but the key must be kept strongly referenced after this method returns,
        // so wrap it in the Lock implementation, which a thread of course needs
        // to be able to synchronize. This enforces a thread to have a strong reference
        // to the key, while it isn't aware of it (as this method declares to return a
        // Lock rather than a ParameterLock).
        return new RpcParameterLock(canonical, lock);
    }

    @Override
    public void lock() {
        lock.lock();
    }

    @Override
    public void lockInterruptibly() throws InterruptedException {
        lock.lockInterruptibly();
    }

    @Override
    public boolean tryLock() {
        return lock.tryLock();
    }

    @Override
    public boolean tryLock(long time, TimeUnit unit) throws InterruptedException {
        return lock.tryLock(time, unit);
    }

    @Override
    public void unlock() {
        lock.unlock();
    }

    @Override
    public Condition newCondition() {
        return lock.newCondition();
    }

}
