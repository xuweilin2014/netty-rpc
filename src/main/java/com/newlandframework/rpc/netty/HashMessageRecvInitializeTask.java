package com.newlandframework.rpc.netty;

import com.newlandframework.rpc.core.ReflectionUtils;
import com.newlandframework.rpc.filter.ServiceFilterBinder;
import com.newlandframework.rpc.jmx.HashModuleMetricsVisitor;
import com.newlandframework.rpc.jmx.ModuleMetricsVisitor;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;
import com.newlandframework.rpc.parallel.HashCriticalSection;
import org.apache.commons.collections.Predicate;
import org.apache.commons.collections.iterators.FilterIterator;
import javax.management.JMException;
import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * 如果开启了JMX监控，并且指定了使用哈希分段加锁算法来提降低锁的颗粒度，就会使用这个类。
 * 哈希分段加锁算法的具体思想：
 * 哈希分段的个数以8为例，RPC服务器会为每一个接口中的每一个方法，创建8个ModuleMetricsVisitor，也就是一个visitorList。如果RPC服务器一共有
 * M个方法（包括所有不同接口），那么就会创建M个visitorList。最后这些visitorList都会被保存到hashVisitorList中。然后当客户端发起一次RPC请求，
 * 比如要调用AddCalculate接口中的add方法时，会创建一个HashMessageRecvInitializeTask对象（如果用户配置了参数的话），这个task在创建的时候，
 * 会根据requestId的值，使用fnv哈希算法生成一个hashKey（介于0~7之间）。前面说过，每一个方法都对应8个ModuleMetricsVisitor，
 * 此hashKey表明这个RPC调用应该更新哪一个ModuleMetricsVisitor关于这次add方法调用的数据。最后统计add方法调用的详情时，
 * 需要把这8个ModuleMetricsVisitor中的统计信息合并在一起，最后显示出来。
 */
public class HashMessageRecvInitializeTask extends AbstractMessageRecvInitializeTask {
    private int hashKey;
    private static HashCriticalSection criticalSection = new HashCriticalSection();
    private ModuleMetricsVisitor visitor;

    public HashMessageRecvInitializeTask(MessageRequest request, MessageResponse response, Map<String, Object> handlerMap) {
        super(request, response, handlerMap);
        //将请求的requestId通过哈希算法映射到partition个哈希分段中的某一个上
        hashKey = HashCriticalSection.hash(request.getMessageId());
    }

    @Override
    protected void injectInvoke() {
        Class cls = handlerMap.get(request.getClassName()).getClass();
        boolean binder = ServiceFilterBinder.class.isAssignableFrom(cls);
        if (binder) {
            cls = ((ServiceFilterBinder) handlerMap.get(request.getClassName())).getObject().getClass();
        }

        ReflectionUtils utils = new ReflectionUtils();

        try {
            Method method = ReflectionUtils.getDeclaredMethod(cls, request.getMethodName(), request.getTypeParameters());
            utils.listMethod(method, false);
            String signatureMethod = utils.getProvider().toString().trim();
            // 我们要调用的方法位于hashVisitorList中的哪一个visitorList中，也就是index
            int index = getHashVisitorListIndex(signatureMethod);
            List<ModuleMetricsVisitor> metricsVisitor = HashModuleMetricsVisitor.getInstance().getHashVisitorList().get(index);
            // 我们调用某个方法时，应该使用visitorList中索引为hashKey的ModuleMetricsVisitor来记录方法调用的数据
            visitor = metricsVisitor.get(hashKey);
            // 将方法调用的次数增加1，即将visitor中的invokeCount属性增加1
            incrementInvoke(visitor);
        } finally {
            utils.clearProvider();
        }
    }

    @Override
    protected void injectSuccInvoke(long invokeTimespan) {
        incrementInvokeSucc(visitor, invokeTimespan);
    }

    @Override
    protected void injectFailInvoke(Throwable error) {
        incrementInvokFail(visitor, error);
    }

    @Override
    protected void injectFilterInvoke() {
        incrementInvokFilter(visitor);
    }

    @Override
    protected void acquire() {
        criticalSection.enter(hashKey);
    }

    @Override
    protected void release() {
        criticalSection.exit(hashKey);
    }

    private int getHashVisitorListIndex(String signatureMethod) {
        int index;
        // size表明有多少个visitorList，也就是说RPC服务器一共有多少个方法
        int size = HashModuleMetricsVisitor.getInstance().getHashModuleMetricsVisitorListSize();

        // 由于每个方法都对应一个visitorList，并且这些visitorList都存放在hashVisitorList中，而hashVisitorList类型为
        // List<List<ModuleMetricsVisitor>>。因此要查找到我们要调用的方法对应的visitorList在hashVisitorList中的index。
        breakFor:
        for (index = 0; index < size; index++) {
            Iterator iterator = new FilterIterator(HashModuleMetricsVisitor.getInstance()
                    .getHashVisitorList().get(index).iterator(), new Predicate() {
                @Override
                public boolean evaluate(Object object) {
                    String statClassName = ((ModuleMetricsVisitor) object).getClassName();
                    String statMethodName = ((ModuleMetricsVisitor) object).getMethodName();
                    // 判断这个visitorList是否为我们要调用的方法
                    return statClassName.compareTo(request.getClassName()) == 0 && statMethodName.compareTo(signatureMethod) == 0;
                }
            });
            // 如果找到了我们要调用的方法对应的visitorList，就跳出循环，返回index
            while (iterator.hasNext()) {
                break breakFor;
            }
        }
        return index;
    }

    private void incrementInvoke(ModuleMetricsVisitor visitor) {
        //visitor.setHashKey(hashKey);
        visitor.incrementInvokeCount();
    }

    /**
     * 当一个方法调用成功后，调用此方法。它的主要作用是将方法调用成功的次数增加1、设置方法调用的耗时、
     * 更新方法调用的最小时长和最大时长
     */
    private void incrementInvokeSucc(ModuleMetricsVisitor visitor, long invokeTimespan) {
        visitor.incrementInvokeSuccCount();
        visitor.setInvokeTimespan(invokeTimespan);

        if (invokeTimespan < visitor.getInvokeMinTimespan()) {
            visitor.setInvokeMinTimespan(invokeTimespan);
        }
        if (invokeTimespan > visitor.getInvokeMaxTimespan()) {
            visitor.setInvokeMaxTimespan(invokeTimespan);
        }
    }

    /**
     * 当一个方法调用失败后，调用此方法。它的主要作用是将方法调用失败的次数增加1、设置方法调用发生异常的时间、
     * 设置方法调用异常的堆栈信息
     */
    private void incrementInvokFail(ModuleMetricsVisitor visitor, Throwable error) {
        visitor.incrementInvokeFailCount();
        visitor.setLastStackTrace((Exception) error);
        try {
            visitor.buildErrorCompositeData(error);
        } catch (JMException e) {
            e.printStackTrace();
        }
    }

    /**
     * 当一个方法的调用被过滤之后，调用此方法
     */
    private void incrementInvokFilter(ModuleMetricsVisitor visitor) {
        visitor.incrementInvokeFilterCount();
    }
}

