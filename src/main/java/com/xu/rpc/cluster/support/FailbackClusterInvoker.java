package com.xu.rpc.cluster.support;

import com.xu.rpc.cluster.AbstractClusterInvoker;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.cluster.LoadBalancer;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.parallel.NamedThreadFactory;
import com.xu.rpc.protocol.Invoker;
import org.apache.log4j.Logger;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

// 失败自动恢复：invoker 调用失败之后，会定时重新再发送
public class FailbackClusterInvoker extends AbstractClusterInvoker {

    private static final Logger logger = Logger.getLogger(FailbackClusterInvoker.class);

    private static ScheduledThreadPoolExecutor failRetryExecutor = new ScheduledThreadPoolExecutor(2, new NamedThreadFactory("RpcFailbackRetryThread", true));

    private static Map<RpcInvocation, Invoker> failed = new ConcurrentHashMap<>();

    static {
        failRetryExecutor.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                try {
                    retry();
                } catch (Exception e) {
                    logger.warn("error occurs when trying to retry the failed invoker.");
                }
            }
        }, RpcConfig.RETRY_PERIOD, RpcConfig.RETRY_PERIOD, TimeUnit.MILLISECONDS);
    }

    public FailbackClusterInvoker(Directory directory) {
        super(directory);
    }

    @Override
    public Object doInvoke(RpcInvocation invocation, List<Invoker> invokers, LoadBalancer loadBalance) throws RpcException {
        Object result = null;
        Invoker invoker = null;
        try {
            if (invokers == null || invokers.isEmpty()){
                throw new RpcException("there is no invoker to be invoked.");
            }

            invoker = select(invokers, null, loadBalance, invocation);
            result = invoker.invoke(invocation);
        } catch (Throwable e) {
            logger.warn("error occurs when trying to invoke the invoker for " + invoker + ", waiting for retry later.");
            // 注意，失败之后是将 FailbackClusterInvoker 对象本身作为 invoker 添加到 failed 集合中，等待线程重新调用
            // 重新调用时，会调用 invoker 的 invoke 方法，重新进行选择，负载均衡操作
            failed.put(invocation, this);
        }

        return result;
    }

    private static void retry(){
        if (!failed.isEmpty()){
            for (Map.Entry<RpcInvocation, Invoker> entry : failed.entrySet()) {
                RpcInvocation invocation = entry.getKey();
                Invoker invoker = entry.getValue();
                try {
                    invoker.invoke(invocation);
                    failed.remove(invocation);
                } catch (RpcException e) {
                    logger.warn("error occurs again when trying to invoke the invoker for " + invoker.getInterface() +
                            ", waiting for retry later.");
                }
            }
        }
    }

}
