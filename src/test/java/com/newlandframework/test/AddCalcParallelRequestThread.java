package com.newlandframework.test;

import com.xu.rpc.exception.InvokeTimeoutException;
import com.xu.rpc.services.AddCalculate;

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
            if (add != taskNumber * 2){
                System.err.println("compute error: " + taskNumber + " * 2 != " + add);
            }else{
                System.out.println("calc add result of " + taskNumber + " * 2 :[" + add + "]");
            }
        } catch (InterruptedException ex) {
            Logger.getLogger(AddCalcParallelRequestThread.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvokeTimeoutException ex) {
            System.out.println(ex.getMessage());
        } finally {
            finish.countDown();
        }
    }
}

