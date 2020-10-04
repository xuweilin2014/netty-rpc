package com.xu.rpc.test;

import com.xu.rpc.services.AddCalculate;
import com.xu.rpc.services.MultiCalculate;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class RpcParallelTest {

    private static final ThreadPoolExecutor executor = (ThreadPoolExecutor) Executors.newCachedThreadPool();

    public static void parallelAddCalcTask(AddCalculate calc, int parallel, List<Double> rts, List<Double> elapses, List<Double> qpss) throws InterruptedException {
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
        double rt = elapsed / parallel;
        double qps = (1000 * parallel) / elapsed;
        qpss.add(qps);
        rts.add(rt);
        elapses.add(elapsed);

        System.out.printf("加法计算RPC调用总共耗时: %4.2f 毫秒, 每次请求的耗时：%2.2f 毫秒， QPS为：%4.2f\n", elapsed, rt, qps);
    }

    public static void parallelMultiCalcTask(MultiCalculate calc, int parallel, List<Double> rts, List<Double> elapses, List<Double> qpss) throws InterruptedException {
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
        double rt = elapsed / parallel;
        double qps = (1000 * parallel) / elapsed;
        qpss.add(qps);
        rts.add(rt);
        elapses.add(elapsed);

        System.out.printf("乘法计算RPC调用总共耗时: %4.2f 毫秒, 每次请求的耗时：%2.2f 毫秒， QPS为：%4.2f\n", elapsed, rt, qps);
    }

    public static void addTask(AddCalculate calc, int parallel, List<Double> rts, List<Double> elapses, List<Double> qpss) throws InterruptedException {
        RpcParallelTest.parallelAddCalcTask(calc, parallel, rts, elapses, qpss);
        TimeUnit.MILLISECONDS.sleep(30);
    }

    public static void multiTask(MultiCalculate calc, int parallel, List<Double> rts, List<Double> elapses, List<Double> qpss) throws InterruptedException {
        RpcParallelTest.parallelMultiCalcTask(calc, parallel, rts, elapses, qpss);
        TimeUnit.MILLISECONDS.sleep(30);
    }

    public static void main(String[] args) throws Exception {
        //并行度1000
        int parallel = 3000;
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("classpath:rpc-invoke-config-client.xml");
        List<Double> rts = new ArrayList<>();
        List<Double> elapses = new ArrayList<>();
        List<Double> qps = new ArrayList<>();

        for (int i = 0; i < 30; i++) {
            addTask((AddCalculate) context.getBean("addCalc"), parallel, rts, elapses, qps);
            multiTask((MultiCalculate) context.getBean("multiCalc"), parallel, rts, elapses, qps);
            System.out.printf("netty-rpc server 消息协议序列化第[%d]轮并发验证结束!\n\n", i);
            Thread.sleep(2000);
        }

        System.out.println();
        System.out.println(String.format("平均调用耗时：%4.2f ms", getAvg(elapses.iterator())));
        System.out.println(String.format("平均每次请求的耗时：%2.2f ms", getAvg(rts.iterator())));
        System.out.println(String.format("平均QPS：%.2f", getAvg(qps.iterator())));
        context.destroy();
    }

    private static double getAvg(Iterator<Double> iterator){
        double sum = 0;
        int counter = 0;
        while (iterator.hasNext()){
            sum += iterator.next();
            counter++;
        }

        if (counter > 0)
            return sum / counter;

        return 0;
    }
}
