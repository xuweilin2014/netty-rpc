package com.xu.rpc.test;

import com.xu.rpc.services.AddCalculate;
import com.xu.rpc.services.MultiCalculate;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class RpcParallelTest {

    private static final ThreadPoolExecutor executor = (ThreadPoolExecutor) Executors.newCachedThreadPool();

    public static void parallelAddCalcTask(AddCalculate calc, int parallel) throws InterruptedException {
        //开始计时
        long start = System.currentTimeMillis();

        CountDownLatch signal = new CountDownLatch(1);
        CountDownLatch finish = new CountDownLatch(parallel);

        for (int index = 0; index < parallel; index++) {
            AddCalcParallelRequestThread client = new AddCalcParallelRequestThread(calc, signal, finish, index);
            new Thread(client).start();
        }

        signal.countDown();
        finish.await();

        double elapsed = System.currentTimeMillis() - start;
        String tip = String.format("加法计算RPC调用总共耗时: [%s] 毫秒", elapsed);
        System.out.println("每次请求的耗时：" + (elapsed / parallel));
        System.out.println(tip);
    }

    public static void parallelMultiCalcTask(MultiCalculate calc, int parallel) throws InterruptedException {
        //开始计时
        long start = System.currentTimeMillis();

        CountDownLatch signal = new CountDownLatch(1);
        CountDownLatch finish = new CountDownLatch(parallel);

        for (int index = 0; index < parallel; index++) {
            MultiCalcParallelRequestThread client = new MultiCalcParallelRequestThread(calc, signal, finish, index);
            new Thread(client).start();
        }

        signal.countDown();
        finish.await();

        double elapsed = System.currentTimeMillis() - start;
        String tip = String.format("乘法计算RPC调用总共耗时: [%s] 毫秒", elapsed);
        System.out.println("每次请求的耗时：" + (elapsed / parallel));
        System.out.println(tip);
    }

    public static void addTask(AddCalculate calc, int parallel) throws InterruptedException {
        RpcParallelTest.parallelAddCalcTask(calc, parallel);
        TimeUnit.MILLISECONDS.sleep(30);
    }

    public static void multiTask(MultiCalculate calc, int parallel) throws InterruptedException {
        RpcParallelTest.parallelMultiCalcTask(calc, parallel);
        TimeUnit.MILLISECONDS.sleep(30);
    }

    public static void main(String[] args) throws Exception {
        //并行度1000
        int parallel = 1000;
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");

        for (int i = 0; i < 30; i++) {
            addTask((AddCalculate) context.getBean("addCalc"), parallel);
            multiTask((MultiCalculate) context.getBean("multiCalc"), parallel);
            System.out.printf("netty-rpc server 消息协议序列化第[%d]轮并发验证结束!\n\n", i);
            Thread.sleep(15000);
        }

        context.destroy();
    }
}
