package com.xu.rpc.test;

import com.xu.rpc.commons.exception.InvokeTimeoutException;
import com.xu.rpc.services.MultiCalculate;

import java.util.Random;
import java.util.concurrent.CountDownLatch;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 */
public class MultiCalcParallelRequestThread implements Runnable {

    private CountDownLatch signal;
    private CountDownLatch finish;
    private int taskNumber = 0;
    private MultiCalculate calc;

    public MultiCalcParallelRequestThread(MultiCalculate calc, CountDownLatch signal, CountDownLatch finish, int taskNumber) {
        this.signal = signal;
        this.finish = finish;
        this.taskNumber = taskNumber;
        this.calc = calc;
    }

    public void run() {
        try {
            signal.await();
            int multi = calc.multi(taskNumber, taskNumber);
            // System.out.println("calc multi result:[" + multi + "]");
        } catch (InterruptedException ex) {
            Logger.getLogger(MultiCalcParallelRequestThread.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvokeTimeoutException ex) {
            System.out.println(ex.getMessage());
        } finally {
            finish.countDown();
        }
    }
}

