package com.newlandframework.test;

import com.newlandframework.rpc.exception.InvokeTimeoutException;
import com.newlandframework.rpc.services.AddCalculate;

import java.util.concurrent.CountDownLatch;
import java.util.logging.Level;
import java.util.logging.Logger;

public class AddCalcParallelRequestThread implements Runnable {

    private CountDownLatch signal;
    private CountDownLatch finish;
    private int taskNumber = 0;
    private AddCalculate calc;

    public AddCalcParallelRequestThread(AddCalculate calc, CountDownLatch signal, CountDownLatch finish, int taskNumber) {
        this.signal = signal;
        this.finish = finish;
        this.taskNumber = taskNumber;
        this.calc = calc;
    }

    public void run() {
        try {
            signal.await();
            int add = calc.add(taskNumber, taskNumber);
            System.out.println("calc add result:[" + add + "]");
        } catch (InterruptedException ex) {
            Logger.getLogger(AddCalcParallelRequestThread.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvokeTimeoutException ex) {
            System.out.println(ex.getMessage());
        } finally {
            finish.countDown();
        }
    }
}

