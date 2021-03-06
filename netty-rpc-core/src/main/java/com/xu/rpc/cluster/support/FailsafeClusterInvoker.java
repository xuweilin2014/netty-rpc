package com.xu.rpc.cluster.support;

import com.xu.rpc.cluster.AbstractClusterInvoker;
import com.xu.rpc.cluster.Directory;
import com.xu.rpc.cluster.LoadBalancer;
import com.xu.rpc.commons.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import org.apache.log4j.Logger;
import java.util.List;

// 失败安全：调用失败之后，仅仅会打印出错误日志，然后返回一个空结果，这个结果需要被忽略
public class FailsafeClusterInvoker extends AbstractClusterInvoker {

    private static Logger logger = Logger.getLogger(FailsafeClusterInvoker.class);

    public FailsafeClusterInvoker(Directory directory) {
        super(directory);
    }

    @Override
    public RpcResult doInvoke(RpcInvocation invocation, List<Invoker> invokers, LoadBalancer loadBalance) throws RpcException {
        RpcResult result = null;
        try {
            Invoker invoker = select(invokers, null, loadBalance, invocation);
            result = invoker.invoke(invocation);

            if (result.getException() != null)
                throw result.getException();

            return result;
        } catch (Throwable e) {
            logger.error("failed to invoke the invoker, caused by " + e.getMessage());
            return new RpcResult();
        }
    }

}
