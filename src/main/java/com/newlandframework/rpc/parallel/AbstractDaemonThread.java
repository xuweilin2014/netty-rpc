package com.newlandframework.rpc.parallel;

/**
 * AbstractDaemonThread类有两个抽象方法需要子类来实现，
 * i.getDaemonThreadName方法，用来获取线程的名字
 * ii.run方法（实现的Runnable接口中的run方法）
 */
public abstract class AbstractDaemonThread implements Runnable {
    protected final Thread thread;

    public AbstractDaemonThread() {
        this.thread = new Thread(this, this.getDaemonThreadName());
    }

    public abstract String getDaemonThreadName();

    public void start() {
        this.thread.start();
    }

}

