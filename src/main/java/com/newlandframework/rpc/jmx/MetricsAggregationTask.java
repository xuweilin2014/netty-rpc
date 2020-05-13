package com.newlandframework.rpc.jmx;

import java.util.List;
import java.util.concurrent.CountDownLatch;

public class MetricsAggregationTask implements Runnable {
    private MetricsTask[] tasks;
    private List<ModuleMetricsVisitor> visitorList;
    private CountDownLatch latch;

    public MetricsAggregationTask(MetricsTask[] tasks, List<ModuleMetricsVisitor> visitors, CountDownLatch latch) {
        this.tasks = tasks;
        this.visitorList = visitors;
        this.latch = latch;
    }

    @Override
    public void run() {
        // :modified
        // 等待所有的task都运行完毕之后，MetricsAggregationTask的run方法就会运行，也就是把汇总得到的结果result
        // (也是ModuleMetricsVisitor对象）添加到visitorList中，最后返回
        try {
            for (MetricsTask task : tasks) {
                visitorList.add(task.getResult());
            }
        } finally {
            // 只有task全部添加到visitorList中，才会将CountDownLatch减一，使得getModuleMetricsVisitor
            // 方法继续向下执行。
            latch.countDown();
        }
    }
}

