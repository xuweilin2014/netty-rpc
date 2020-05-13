package com.newlandframework.rpc.jmx;

import com.newlandframework.rpc.core.RpcSystemConfig;

import javax.management.*;
import java.lang.management.ManagementFactory;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.LockSupport;


public abstract class AbstractModuleMetricsHandler extends NotificationBroadcasterSupport implements ModuleMetricsVisitorMXBean {
    protected List<ModuleMetricsVisitor> visitorList = new CopyOnWriteArrayList<>();
    protected static String startTime;
    private final Queue<Thread> waiters = new ConcurrentLinkedQueue<>();
    private static final int METRICS_VISITOR_LIST_SIZE = HashModuleMetricsVisitor.getInstance().getHashModuleMetricsVisitorListSize();
    private MetricsTask[] tasks = new MetricsTask[METRICS_VISITOR_LIST_SIZE];
    private ExecutorService executor = Executors.newFixedThreadPool(METRICS_VISITOR_LIST_SIZE);

    public AbstractModuleMetricsHandler() {
    }

    /**
     * ModuleMetricsHandler使用了单例模式，也就是ModuleMetricsHandler类型的handler对象在一个RPC服务器中只存在一个。
     * 但是可能会有多个线程来访问handler中getVisitor方法来获取与特定的className#methodName对应的visitor对象（每一个visitor对象记录了
     * 一个特定方法的具体调用情况，也就是说每一个visitor和一个特定的method一一对应）。因此，必须要注意线程间的竞争情况，
     * 所以使用了enter和exit这两个方法，来保证一次只有一个线程进入到临界区去获取visitor
     */
    public ModuleMetricsVisitor getVisitor(String className, String methodName) {
        try {
            enter();
            return visitCriticalSection(className, methodName);
        } finally {
            exit();
        }
    }

    /**
     * 返回ModuleMetricsVisitor分为两种情况：
     * 1.JMX_METRICS_HASH_SUPPORT为false，直接返回visitorList
     * 2.JMX_METRICS_HASH_SUPPORT为true，不能直接返回visitorList，因为此时visitorList为空。前面说过，hashVisitorList中的
     * 每一个visitorList都表示RPC服务器中的一个方法调用情况，此list中不同的visitor都有可能记录了对应方法（比如add方法）的调用情况，这是因为
     * 每一次客户端发起对add方法的调用时，都会通过一定的哈希算法选择visitorList中的某一个visitor来记录add方法的调用情况。因此，在做最终统计
     * 时，需要将list中的所有visitor记录的情况进行一个汇总。
     */
    @Override
    public List<ModuleMetricsVisitor> getModuleMetricsVisitor() {
        if (RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_HASH_SUPPORT) {
            CountDownLatch latch = new CountDownLatch(1);
            MetricsAggregationTask aggregationTask = new MetricsAggregationTask(tasks, visitorList, latch);

            //在创建CyclicBarrier时，指定的barrierAction是aggregationTask，表明在hashVisitorList中所有visitorList的数据都进行汇总之后，
            //会调用aggregationTask中的run方法，将得到结果保存到传入aggregationTask中的visitorList中，然后返回。
            CyclicBarrier barrier = new CyclicBarrier(METRICS_VISITOR_LIST_SIZE, aggregationTask);

            //对每一个visitorList中情况进行汇总的任务由task数组中的一个task来进行。
            for (int i = 0; i < METRICS_VISITOR_LIST_SIZE; i++) {
                tasks[i] = new MetricsTask(barrier, HashModuleMetricsVisitor.getInstance().getHashVisitorList().get(i));
                executor.execute(tasks[i]);
            }

            try {
                visitorList.clear();
                latch.await();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        return visitorList;
    }

    @Override
    public void addModuleMetricsVisitor(ModuleMetricsVisitor visitor) {
        visitorList.add(visitor);
    }

    @Override
    public MBeanNotificationInfo[] getNotificationInfo() {
        String[] types = new String[]{
                AttributeChangeNotification.ATTRIBUTE_CHANGE
        };
        String name = AttributeChangeNotification.class.getName();
        String description = "the event send from NettyRPC server!";
        MBeanNotificationInfo info =
                new MBeanNotificationInfo(types, name, description);
        return new MBeanNotificationInfo[]{info};
    }

    public final static String getStartTime() {
        if (startTime == null) {
            SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            startTime = format.format(new Date(ManagementFactory.getRuntimeMXBean().getStartTime()));
        }
        return startTime;
    }

    /**
     * enter方法使用了一个队列waiters来使得多个线程有序地，一次一个地进入到临界区域中，获取visitor。
     * enter方法具体的逻辑是只有waiters队列中的队头元素可以进入临界区，其余的线程只能阻塞等待
     */
    protected void enter() {
        Thread current = Thread.currentThread();
        waiters.add(current);

        while (waiters.peek() != current) {
            LockSupport.park(ModuleMetricsVisitor.class);
        }
        System.err.println("【" + current.getName() + "】线程进入临界区" );
    }

    /**
     * 当一个线程退出临界区之后，就会把自己从队列中移除，接着唤醒队列中新的队头元素，使得它可以进入临界区获得visitor对象
     */
    protected void exit() {
        System.err.println("【" + Thread.currentThread().getName() + "】线程退出临界区" );
        waiters.remove();
        LockSupport.unpark(waiters.peek());
    }

    protected abstract ModuleMetricsVisitor visitCriticalSection(String className, String methodName);

    public ExecutorService getExecutor() {
        return executor;
    }

    public void setExecutor(ExecutorService executor) {
        this.executor = executor;
    }
}

