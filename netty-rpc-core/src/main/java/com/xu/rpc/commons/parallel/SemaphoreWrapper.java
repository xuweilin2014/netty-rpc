package com.xu.rpc.commons.parallel;

import lombok.Data;

import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;

@Data
public class SemaphoreWrapper {
    protected final AtomicBoolean released = new AtomicBoolean(false);

    protected Semaphore semaphore;

    public SemaphoreWrapper() {
        semaphore = new Semaphore(1);
    }

    public SemaphoreWrapper(int permits) {
        semaphore = new Semaphore(permits);
    }

    public SemaphoreWrapper(int permits, boolean fair) {
        semaphore = new Semaphore(permits, fair);
    }

    public SemaphoreWrapper(Semaphore semaphore) {
        this.semaphore = semaphore;
    }

    public void release() {
        if (this.semaphore != null) {
            if (this.released.compareAndSet(false, true)) {
                this.semaphore.release();
            }
        }
    }

    public void acquire() {
        if (this.semaphore != null) {
            try {
                semaphore.acquire();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    public boolean isRelease() {
        return released.get();
    }
}

