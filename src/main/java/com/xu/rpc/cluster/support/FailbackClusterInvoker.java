package com.xu.rpc.cluster.support;

import com.xu.rpc.cluster.AbstractClusterInvoker;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.cluster.LoadBalancer;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.parallel.NamedThreadFactory;
import com.xu.rpc.protocol.Invoker;
import lombok.Getter;
import lombok.Setter;
import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

// 失败自动恢复：invoker 调用失败之后，会定时重新再发送
public class FailbackClusterInvoker extends AbstractClusterInvoker {

    private static final Logger logger = Logger.getLogger(FailbackClusterInvoker.class);

    private static final int MAX_FAILED_RETRY_TIMES = 3;

    private static ScheduledThreadPoolExecutor failRetryExecutor = new ScheduledThreadPoolExecutor(1, new NamedThreadFactory("RpcFailbackRetryThread", true));

    private static Map<RpcInvocation, InvokerDelegate> failed = new ConcurrentHashMap<>();

    static {
        failRetryExecutor.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                try {
                    retry();
                } catch (Exception e) {
                    logger.error("error occurs when trying to retry the failed invoker.");
                }
            }
        }, RpcConfig.RETRY_PERIOD, RpcConfig.RETRY_PERIOD, TimeUnit.MILLISECONDS);
    }

    public FailbackClusterInvoker(Directory directory) {
        super(directory);
    }

    @Override
    public RpcResult doInvoke(RpcInvocation invocation, List<Invoker> invokers, LoadBalancer loadBalance) throws RpcException {
        RpcResult result = null;
        Invoker invoker = null;
        try {
            if (invokers == null || invokers.isEmpty()){
                throw new RpcException("there is no invoker to be invoked.");
            }

            invoker = select(invokers, null, loadBalance, invocation);
            // 在调用 AbstractInvoker#invoke 方法时，如果发生了任何异常，那么都会被捕获，放在 RpcResult 的 exception 属性中
            result = invoker.invoke(invocation);

            if (result.getException() != null){
                throw result.getException();
            }
        } catch (Throwable e) {
            logger.error("failed to invoke the invoker " + invoker + ", waiting for retry later.");
            // 注意，失败之后是将 FailbackClusterInvoker 对象本身作为 invoker 添加到 failed 集合中，等待线程重新调用
            // 重新调用时，会调用 invoker 的 invoke 方法，重新进行选择，负载均衡操作
            InvokerDelegate delegate = failed.get(invocation);
            if (delegate == null){
                failed.put(invocation, new InvokerDelegate(0, this));
                delegate = failed.get(invocation);
            }
            failed.put(invocation, delegate);
        }

        return result;
    }

    private static void retry(){
        if (!failed.isEmpty()){

            Set<RpcInvocation> keySet = failed.keySet();
            for (RpcInvocation invocation : keySet) {
                InvokerDelegate delegate = failed.get(invocation);

                try {
                    // 对每一个 invoker 最多重试 3 次
                    if (delegate.getInvokeCount() >= MAX_FAILED_RETRY_TIMES){
                        logger.error("retry times exceeds " + MAX_FAILED_RETRY_TIMES + " times and retry for this invoker will be abandoned.");
                        failed.remove(invocation);
                        continue;
                    }

                    RpcResult result = delegate.getInvoker().invoke(invocation);
                    // 同样的，被捕获的异常都会放在 RpcResult 中的 exception 属性中
                    if (result.getException() != null){
                        throw result.getException();
                    }

                    failed.remove(invocation);
                    logger.info("attempt to invoke the invoker " + failed.get(invocation) + " successfully!");
                } catch (Throwable e) {
                    delegate.incrementCount();
                    logger.error("failed to invoke the invoker for " + delegate.getInvoker().getInterface() +
                            " and has tried " + failed.get(invocation).getInvokeCount() + " times, waiting for retry later.");
                }
            }
        }
    }

    @Getter
    @Setter
    static class InvokerDelegate{

        private int invokeCount;

        private Invoker invoker;

        public InvokerDelegate(int invokeCount, Invoker invoker) {
            this.invokeCount = invokeCount;
            this.invoker = invoker;
        }

        protected void incrementCount(){
            invokeCount++;
        }
    }

}
