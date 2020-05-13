package com.newlandframework.rpc.jmx;

import lombok.Data;
import org.apache.commons.collections.iterators.UniqueFilterIterator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

/**
 * 一个MetricsTask的作用为：负责将一个visitorList中所有visitor记录的某个方法调用数据进行汇总
 */
public class MetricsTask implements Runnable {
    private final CyclicBarrier barrier;
    private List<ModuleMetricsVisitor> visitorList;
    private ModuleMetricsVisitor result;

    public MetricsTask(CyclicBarrier barrier, List<ModuleMetricsVisitor> visitorList) {
        this.barrier = barrier;
        this.visitorList = visitorList;
    }

    @Override
    public void run() {
        try {
            // :modified
            accumulate();
            // 在hashVisitorList中的所有visitorList的数据汇总完毕之前（一个MetricsTask负责一个visitorList数据的汇总），
            // 线程会阻塞在这里，只有全部汇总完毕之后，线程才会继续向下执行。
            barrier.await();
        } catch (InterruptedException | BrokenBarrierException e) {
            e.printStackTrace();
        }
    }

    private void count(List<ModuleMetricsVisitor> list) {
        long invokeCount = 0L;
        long invokeSuccCount = 0L;
        long invokeFailCount = 0L;
        long invokeFilterCount = 0L;
        long invokeTimespan = 0L;
        long invokeMinTimespan = list.get(0).getInvokeMinTimespan();
        long invokeMaxTimespan = list.get(0).getInvokeMaxTimespan();
        String lastStackTraceDetail = "";
        long lastErrorTime = list.get(0).getErrorLastTimeLongVal();

        ModuleMetrics metrics = new ModuleMetrics();
        metrics.setInvokeCount(invokeCount);
        metrics.setInvokeSuccCount(invokeSuccCount);
        metrics.setInvokeFailCount(invokeFailCount);
        metrics.setInvokeFilterCount(invokeFilterCount);
        metrics.setInvokeTimespan(invokeTimespan);
        metrics.setInvokeMinTimespan(invokeMinTimespan);
        metrics.setInvokeMaxTimespan(invokeMaxTimespan);
        metrics.setLastStackTraceDetail(lastStackTraceDetail);
        metrics.setLastErrorTime(lastErrorTime);

        merge(list, metrics);

        result.setInvokeCount(metrics.getInvokeCount());
        result.setInvokeSuccCount(metrics.getInvokeSuccCount());
        result.setInvokeFailCount(metrics.getInvokeFailCount());
        result.setInvokeFilterCount(metrics.getInvokeFilterCount());
        result.setInvokeTimespan(metrics.getInvokeTimespan());
        result.setInvokeMaxTimespan(metrics.getInvokeMaxTimespan());
        result.setInvokeMinTimespan(metrics.getInvokeMinTimespan());

        if (metrics.getLastErrorTime() > 0) {
            result.setErrorLastTimeLongVal(metrics.getLastErrorTime());
            result.setLastStackTraceDetail(metrics.getLastStackTraceDetail());
        }
    }

    private void merge(List<ModuleMetricsVisitor> list, ModuleMetrics metrics) {
        long invokeCount = metrics.getInvokeCount();
        long invokeSuccCount = metrics.getInvokeSuccCount();
        long invokeFailCount = metrics.getInvokeFailCount();
        long invokeFilterCount = metrics.getInvokeFilterCount();
        long invokeTimespan = metrics.getInvokeTimespan();
        long invokeMinTimespan = metrics.getInvokeMinTimespan();
        long invokeMaxTimespan = metrics.getInvokeMaxTimespan();
        long[] invokeHistogram = metrics.getInvokeHistogram();
        String lastStackTraceDetail = metrics.getLastStackTraceDetail();
        long lastErrorTime = metrics.getLastErrorTime();

        for (int i = 0; i < list.size(); i++) {
            boolean find = equals(result.getClassName(), list.get(i).getClassName(),
                    result.getMethodName(), list.get(i).getMethodName());
            if (find) {
                invokeCount += list.get(i).getInvokeCount();
                invokeSuccCount += list.get(i).getInvokeSuccCount();
                invokeFailCount += list.get(i).getInvokeFailCount();
                invokeFilterCount += list.get(i).getInvokeFilterCount();
                long timespan = list.get(i).getInvokeTimespan();
                if (timespan > 0) {
                    invokeTimespan = timespan;
                }
                long minTimespan = list.get(i).getInvokeMinTimespan();
                long maxTimespan = list.get(i).getInvokeMaxTimespan();
                if (minTimespan < invokeMinTimespan) {
                    invokeMinTimespan = minTimespan;
                }
                if (maxTimespan > invokeMaxTimespan) {
                    invokeMaxTimespan = maxTimespan;
                }

                long fail = list.get(i).getInvokeFailCount();
                if (fail > 0) {
                    long lastTime = list.get(i).getErrorLastTimeLongVal();
                    if (lastTime > lastErrorTime) {
                        lastErrorTime = lastTime;
                        lastStackTraceDetail = list.get(i).getLastStackTraceDetail();
                    }
                }
            }
        }

        metrics.setInvokeCount(invokeCount);
        metrics.setInvokeSuccCount(invokeSuccCount);
        metrics.setInvokeFailCount(invokeFailCount);
        metrics.setInvokeFilterCount(invokeFilterCount);
        metrics.setInvokeTimespan(invokeTimespan);
        metrics.setInvokeMinTimespan(invokeMinTimespan);
        metrics.setInvokeMaxTimespan(invokeMaxTimespan);
        metrics.setInvokeHistogram(invokeHistogram);
        metrics.setLastStackTraceDetail(lastStackTraceDetail);
        metrics.setLastErrorTime(lastErrorTime);
    }

    private void accumulate() {
        List<ModuleMetricsVisitor> list = visitorList;
        // 遍历visitorList，获取此visitorList中不同的元素，也就是ModuleMetricsVisitor。判断不同的标准是能否将
        // visitor加入到一个set集合中，而加入到set集合中，需要依靠ModuleMetricsVisitor中的equals和hashcode方法。
        //
        // set中插入数据时，先比较hashCode：
        // i.如果hashCode相同才会比较equals，equals相同，则两个对象相同，不能插入，equals不同，可以插入；
        // ii.如果hashCode不同，就直接插入了，两个对象hashCode不相等，他们equals一定是false。
        // 由于ModuleMetricsVisitor中的equals和hashcode只与className和methodName有关，因此对于同一个visitorList，
        // 其所有visitor的hashCode和equals方法均返回true
        Iterator iterator = new UniqueFilterIterator(list.iterator());
        while (iterator.hasNext()) {
            ModuleMetricsVisitor visitor = (ModuleMetricsVisitor) iterator.next();
            // 创建一个ModuleMetricsVisitor，用来保存visitorList中的汇总结果
            result = new ModuleMetricsVisitor(visitor.getClassName(), visitor.getMethodName());
            break;
        }

        count(list);
    }

    private boolean equals(String srcClassName, String destClassName,
                           String srcMethodName, String destMethodName) {
        return srcClassName.equals(destClassName) && srcMethodName.equals(destMethodName);
    }

    public ModuleMetricsVisitor getResult() {
        return result;
    }

    public void setResult(ModuleMetricsVisitor result) {
        this.result = result;
    }

    @Data
    private class ModuleMetrics {
        private long invokeCount;
        private long invokeSuccCount;
        private long invokeFailCount;
        private long invokeFilterCount;
        private long invokeTimespan;
        private long invokeMinTimespan;
        private long invokeMaxTimespan;
        private long[] invokeHistogram;
        private String lastStackTraceDetail;
        private long lastErrorTime;
    }
}

