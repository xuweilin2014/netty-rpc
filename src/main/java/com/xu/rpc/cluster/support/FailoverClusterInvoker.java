package com.xu.rpc.cluster.support;

import com.xu.rpc.cluster.AbstractClusterInvoker;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.cluster.LoadBalance;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

public class FailoverClusterInvoker extends AbstractClusterInvoker {

    private static final Logger logger = Logger.getLogger(FailoverClusterInvoker.class);

    public FailoverClusterInvoker(Directory directory) {
        super(directory);
    }

    @SuppressWarnings("ConstantConditions")
    @Override
    public Object doInvoke(RpcInvocation invocation, List<Invoker> invokers, LoadBalance loadBalance) throws RpcException{
        // 获取到重试次数（第一次不算在内）
        // 如果用户没有配置重试次数，那么获取到的就是默认的次数 3 次，第一次 + 两次重试
        // 如果用户配置了重试次数，那么会将其加上一，也就是加上第一次调用，所以说用户配置的重试次数，第一次不算在内
        int retries = getURL().getParameter(RpcConfig.RETRIES_KEY, RpcConfig.DEFAULT_RETRIES) + 1;
        List<Invoker> copyInvokers = invokers;
        List<Invoker> selected = new ArrayList<>();
        Throwable lastException = null;

        int counter;
        for (counter = 0; counter < retries; counter++) {
            if (isDestroyed())
                throw new RpcException("failover cluster invoker is now destroyed, cannot be invoked!");
            // 在进行重试的时候重新列举 Invoker，这样做的好处是，如果某个服务挂了，通过调用 list 可得到最新可用的 Invoker 列表
            if (counter > 0){
                copyInvokers = list(invocation);
            }
            if (invokers == null || invokers.isEmpty()){
                throw new RpcException("there is no invoker to invoke.");
            }

            try{
                Invoker invoker = select(copyInvokers, selected, loadBalance, invocation);
                // select 方法返回的 invoker 可能为 null。
                // 添加 invoker 到 selected 列表中，selected 集合中的 invoker 没有正常提供服务
                if (invoker != null)
                    selected.add(invoker);

                Object result = invoker.invoke(invocation);
                if (lastException != null){
                    logger.warn("although retry successfully, but there are failed invokers " + selected);
                }
                return result;
            }catch (Throwable t){
                lastException = t;
            }
        }

        //若重试失败，则抛出异常
        throw new RpcException("tried " + counter + " times, netty-rpc failed to invoke.");
    }

}
