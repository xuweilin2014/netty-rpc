package com.newlandframework.rpc.jmx;

import javax.management.JMException;
import javax.management.openmbean.*;
import java.beans.ConstructorProperties;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLongFieldUpdater;
import java.util.function.LongBinaryOperator;

/**
 * 作为ModuleMetricsHandler这个bean中的一个属性，必须要有getter方法来获取对象中的属性值
 */
public class MetricsVisitor {

    public static final long DEFAULT_INVOKE_MIN_TIMESPAN = 3600 * 1000L;

    private static final String[] THROWABLE_NAMES = {"message", "class", "stackTrace"};

    private static final String[] THROWABLE_DESCRIPTIONS = {"message", "class", "stackTrace"};

    private static final OpenType<?>[] THROWABLE_TYPES = new OpenType<?>[]{SimpleType.STRING, SimpleType.STRING, SimpleType.STRING};

    private static CompositeType THROWABLE_COMPOSITE_TYPE = null;

    private String className;

    private String methodName;

    //方法调用次数
    private volatile long invokeCount = 0L;
    //方法调用成功的次数
    private volatile long invokeSuccCount = 0L;
    //方法调用失败的次数
    private volatile long invokeFailCount = 0L;
    //方法被过滤的次数
    private volatile long invokeFilterCount = 0L;
    //方法调用的耗时累加
    private volatile long accumulateTimespan = 0L;
    //方法调用的最小耗时，初始值为3600s
    private volatile long invokeMinTimespan = DEFAULT_INVOKE_MIN_TIMESPAN;
    //方法调用的最大耗时，初始值为0s
    private volatile long invokeMaxTimespan = 0L;

    private Exception lastStackTrace;
    //方法最后一次调用失败堆栈明细
    private String lastStackTraceDetail;
    //方法最后一次调用失败的时间
    private long lastErrorTime;

    /**
     * 下面这7个AtomicLongFieldUpdater是用来更新7个Long类型的属性值：invokeCount、invokeSuccCount、invokeFailCount、invokeFilterCount、
     * accumulateTimespan、invokeMaxTimespan、invokeMinTimespan
     * 使用AtomicLongFieldUpdater有以下2个好处：
     * 1.AtomicLong对象的引用和值统统不需要，因而不会有大量的AtomicLong对象存在于堆上
     * 2.AtomicLongFieldUpdater是一个静态常量，它在类加载的时候就放在了堆空间的常量池中，对于N个对象，只需要一个AtomicLongFieldUpdater即可（类静态常量）
     */
    private static final AtomicLongFieldUpdater<MetricsVisitor> invokeCountUpdater =
            AtomicLongFieldUpdater.newUpdater(MetricsVisitor.class, "invokeCount");
    private static final AtomicLongFieldUpdater<MetricsVisitor> invokeSuccCountUpdater =
            AtomicLongFieldUpdater.newUpdater(MetricsVisitor.class, "invokeSuccCount");
    private static final AtomicLongFieldUpdater<MetricsVisitor> invokeFailCountUpdater =
            AtomicLongFieldUpdater.newUpdater(MetricsVisitor.class, "invokeFailCount");
    private static final AtomicLongFieldUpdater<MetricsVisitor> invokeFilterCountUpdater =
            AtomicLongFieldUpdater.newUpdater(MetricsVisitor.class, "invokeFilterCount");
    private static final AtomicLongFieldUpdater<MetricsVisitor> accumulateTimespanUpdater =
            AtomicLongFieldUpdater.newUpdater(MetricsVisitor.class, "accumulateTimespan");
    private static final AtomicLongFieldUpdater<MetricsVisitor> invokeMaxTimespanUpdater =
            AtomicLongFieldUpdater.newUpdater(MetricsVisitor.class, "invokeMaxTimespan");
    private static final AtomicLongFieldUpdater<MetricsVisitor> invokeMinTimespanUpdater =
            AtomicLongFieldUpdater.newUpdater(MetricsVisitor.class, "invokeMinTimespan");

    @ConstructorProperties({"className", "methodName"})
    public MetricsVisitor(String className, String methodName) {
        this.className = className;
        this.methodName = methodName;
        clear();
    }

    public void clear() {
        lastStackTraceDetail = "";
        accumulateTimespan = 0L;
        invokeMinTimespan = DEFAULT_INVOKE_MIN_TIMESPAN;
        invokeMaxTimespan = 0L;
        lastErrorTime = 0L;
        lastStackTrace = null;
        invokeCountUpdater.set(this, 0);
        invokeSuccCountUpdater.set(this, 0);
        invokeFailCountUpdater.set(this, 0);
        invokeFilterCountUpdater.set(this, 0);
    }

    public void reset() {
        className = "";
        methodName = "";
        clear();
    }

    public void setErrorLastTimeLongVal(long lastErrorTime) {
        this.lastErrorTime = lastErrorTime;
    }

    public long getErrorLastTimeLongVal() {
        return lastErrorTime;
    }

    public String getLastErrorTime() {
        if (lastErrorTime <= 0) {
            return null;
        }
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        return format.format(new Date(lastErrorTime));
    }

    public void setLastErrorTime(long lastErrorTime) {
        this.lastErrorTime = lastErrorTime;
    }

    public String getLastStackTrace() {
        if (lastStackTrace == null) {
            return null;
        }

        StringWriter buf = new StringWriter();
        lastStackTrace.printStackTrace(new PrintWriter(buf));
        return buf.toString();
    }

    public String getStackTrace(Throwable ex) {
        StringWriter buf = new StringWriter();
        ex.printStackTrace(new PrintWriter(buf));

        return buf.toString();
    }

    public synchronized void setLastStackTrace(Exception lastStackTrace) {
        this.lastStackTrace = lastStackTrace;
        //获取方法上次调用异常得堆栈信息
        this.lastStackTraceDetail = getLastStackTrace();
        //获取方法调用异常的时间
        this.lastErrorTime = System.currentTimeMillis();
    }

    public void setLastStackTraceDetail(String lastStackTraceDetail) {
        this.lastStackTraceDetail = lastStackTraceDetail;
    }

    public String getLastStackTraceDetail() {
        return lastStackTraceDetail;
    }

    public CompositeType getThrowableCompositeType() throws JMException {
        if (THROWABLE_COMPOSITE_TYPE == null) {
            THROWABLE_COMPOSITE_TYPE = new CompositeType("Throwable",
                    "Throwable",
                    THROWABLE_NAMES,
                    THROWABLE_DESCRIPTIONS,
                    THROWABLE_TYPES);
        }

        return THROWABLE_COMPOSITE_TYPE;
    }

    public CompositeData buildErrorCompositeData(Throwable error) throws JMException {
        if (error == null) {
            return null;
        }

        Map<String, Object> map = new HashMap<>(512);

        map.put("class", error.getClass().getName());
        map.put("message", error.getMessage());
        map.put("stackTrace", getStackTrace(error));

        return new CompositeDataSupport(getThrowableCompositeType(), map);
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String moduleName) {
        this.className = moduleName;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    /**
     * 获取、设置、增加invokeCount的值
     */
    public long getInvokeCount() {
        return invokeCountUpdater.get(this);
    }

    public void setInvokeCount(long invokeCount) {
        invokeCountUpdater.set(this, invokeCount);
    }

    public void incrementInvokeCount() {
        invokeCountUpdater.incrementAndGet(this);
    }

    /**
     * 获取、设置、增加invokeSuccCount的值
     */
    public long getInvokeSuccCount() {
        return invokeSuccCountUpdater.get(this);
    }

    public void setInvokeSuccCount(long invokeSuccCount) {
        invokeSuccCountUpdater.set(this, invokeSuccCount);
    }

    public void incrementInvokeSuccCount() {
        invokeSuccCountUpdater.incrementAndGet(this);
    }

    /**
     * 获取、设置、增加invokeFailCount的值
     */
    public long getInvokeFailCount() {
        return invokeFailCountUpdater.get(this);
    }

    public void setInvokeFailCount(long invokeFailCount) {
        invokeFailCountUpdater.set(this, invokeFailCount);
    }

    public void incrementInvokeFailCount() {
        invokeFailCountUpdater.incrementAndGet(this);
    }

    /**
     * 获取、设置、增加invokeFilterCountUpdater的值
     */
    public long getInvokeFilterCount() {
        return invokeFilterCountUpdater.get(this);
    }

    public void setInvokeFilterCount(long invokeFilterCount) {
        invokeFilterCountUpdater.set(this, invokeFilterCount);
    }

    public void incrementInvokeFilterCount() {
        invokeFilterCountUpdater.incrementAndGet(this);
    }

    public long getAccumulateTimespan() {
        return accumulateTimespan;
    }

    public void setAccumulateTimespan(long accumulateTimespan) {
        this.accumulateTimespan = accumulateTimespan;
    }

    public void accumulateTimespan(Long invokeTimespan){
        accumulateTimespanUpdater.accumulateAndGet(this, invokeTimespan,
                new LongBinaryOperator() {
            @Override
            public long applyAsLong(long left, long right) {
                return left + right;
            }
        });
    }

    public long getInvokeMinTimespan() {
        return invokeMinTimespan;
    }

    public void setInvokeMinTimespan(long invokeMinTimespan) {
        do{
            if (invokeMinTimespan >= this.invokeMinTimespan)
                break;
        }while (!invokeMinTimespanUpdater.compareAndSet(this,
                this.invokeMinTimespan, invokeMinTimespan));
    }

    public long getInvokeMaxTimespan() {
        return invokeMaxTimespan;
    }

    public void setInvokeMaxTimespan(long invokeMaxTimespan) {
        do{
            if (invokeMaxTimespan <= this.invokeMaxTimespan)
                break;
        }while (!invokeMaxTimespanUpdater.compareAndSet(this,
                this.invokeMaxTimespan, invokeMaxTimespan));
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((className == null) ? 0 : className.hashCode());
        result = prime * result + ((methodName == null) ? 0 : methodName.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        return className.equals(((MetricsVisitor) obj).className)
                && methodName.equals(((MetricsVisitor) obj).methodName);
    }

    @Override
    public String toString() {
        String metrics = String.format("<<[moduleName:%s]-[methodName:%s]>> [invokeCount:%d][invokeSuccCount:%d]" +
                "[invokeFilterCount:%d][invokeTimespan:%d][invokeMinTimespan:%d][invokeMaxTimespan:%d]" +
                "[invokeFailCount:%d][lastErrorTime:%d][lastStackTraceDetail:%s]\n",
                className, methodName, invokeCount, invokeSuccCount, invokeFilterCount, accumulateTimespan,
                invokeMinTimespan, invokeMaxTimespan, invokeFailCount, lastErrorTime, lastStackTraceDetail);
        return metrics;
    }
}

