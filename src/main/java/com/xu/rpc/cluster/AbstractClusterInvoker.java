package com.xu.rpc.cluster;

import com.sun.codemodel.internal.JInvocation;
import com.sun.org.apache.bcel.internal.generic.FADD;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.extension.ExtensionLoader;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.util.AdaptiveExtensionUtil;
import com.xu.rpc.util.URL;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

public abstract class AbstractClusterInvoker implements Invoker{

    private static final Logger logger = Logger.getLogger(AbstractClusterInvoker.class);

    private Directory directory;

    private AtomicBoolean destroyed = new AtomicBoolean(false);

    private volatile Invoker stickyInvoker = null;

    public AbstractClusterInvoker(Directory directory){
        this.directory = directory;
    }

    @Override
    public Object invoke(RpcInvocation invocation) throws RpcException {
        if (invocation == null)
            return null;

        LoadBalance loadBalance = AdaptiveExtensionUtil.getLoadBalance(getURL());
        List<Invoker> invokers = list(invocation);
        return doInvoke(invocation, invokers, loadBalance);
    }

    public Invoker select(List<Invoker> invokers, List<Invoker> selected, LoadBalance loadBalance, RpcInvocation invocation)
            throws RpcException{
        if (invokers == null || invokers.isEmpty()){
            logger.error("no provider available to select.");
            return null;
        }

        // 如果线程走到当前代码处，说明没有开启 sticky 配置，或者前面的 stickyInvoker 为空，或者不可用。
        // 此时继续调用 doSelect ，进入真正的选择逻辑，选择 Invoker
        boolean sticky = invokers.get(0).getURL().getParameter(RpcConfig.STICKY_KEY, false);

        // 检测 invokers 列表是否包含 stickyInvoker，如果不包含，说明 stickyInvoker 代表的服务提供者挂了，此时需要将其置空
        // 这里的 invokers 列表可以看作是存活着的服务提供者列表，如果这个列表不包含 stickyInvoker，那么自然而然地认为 stickyInvoker 挂了
        if (stickyInvoker != null && !invokers.contains(stickyInvoker)){
            stickyInvoker = null;
        }

        // stickyInvoker不为null，并且没在已选列表中，返回上次的服务提供者stickyInvoker，但之前强制校验可达性，如果没有开启强制校验，也不能返回 stickyInvoker。
        // selected 如果包含的 stickyInvoker 的话，说明 stickyInvoker 在此之前没有成功提供服务（但其仍然处于存活状态）。此时我们认为这个服务不可靠，
        // 不应该在重试期间内再次被调用，因此这个时候不会返回该 stickyInvoker。如果 selected 不包含 stickyInvoker，此时还需要进行可用性检测，比如检测服务提供者网络连通性等。
        // 当可用性检测通过，才可返回 stickyInvoker。
        if (sticky && stickyInvoker != null){
            if (stickyInvoker.isAvailable() && (selected == null || !selected.contains(stickyInvoker))){
                return stickyInvoker;
            }
        }

        Invoker invoker = select0(invokers, selected, loadBalance, invocation);

        // 如果 sticky 为 true，则将负载均衡组件选出的 Invoker 赋值给 stickyInvoker
        if (sticky){
            stickyInvoker = invoker;
        }

        return invoker;
    }

    // 其它异常均被捕获，在使用负载均衡策略选择 invoker 的时候，可能会抛出异常
    // 返回的结果可能为 null，分别在 1 和 4 处
    private Invoker select0(List<Invoker> invokers, List<Invoker> selected, LoadBalance loadBalance, RpcInvocation invocation)
                throws RpcException{
        // 1.当 invokers 为 null 或者空集的时候，直接返回 null。
        if (invokers == null || invokers.size() == 0){
            logger.error("no provider available to select.");
            return null;
        }

        // 2.当 invokers 的大小为 1 时，直接返回其中的 invoker，不过此 invoker 可能不可用
        if (invokers.size() == 1)
            return invokers.get(0);

        // 3.先使用负载均衡进行选择
        Invoker invoker = loadBalance.select(invocation, invokers, getURL());

        // 4.判断选择的 invoker 是否经过可用性检查，并且是否包含在 selected 集合中 (包含则表明无法提供服务）。
        // 如果是的话，就进行重新选择，注意重新选择的 invoker 可能为 null（如果为 null 会打印错误日志）；如果不是的话，直接返回
        if ((!invoker.isAvailable()) || (selected != null && selected.contains(invoker))){
            try {
                invoker = reselect(invokers, selected, loadBalance, invocation);
                if (invoker == null){
                    logger.warn("no suitable provider available.");
                }
            } catch (Throwable t) {
                logger.error("error occurs when trying to reselect invokers.");
            }
        }

        return invoker;
    }

    private Invoker reselect(List<Invoker> invokers, List<Invoker> selected, LoadBalance loadBalance, RpcInvocation invocation)
            throws RpcException {
        List<Invoker> reselectInvokers = new ArrayList<>();

        // 遍历 invokers 列表进行可用性检测，并且判断 invoker 不在 selected 集合中
        for (Invoker invoker : invokers) {
            if (invoker.isAvailable()){
                if (selected == null || !selected.contains(invoker)){
                    reselectInvokers.add(invoker);
                }
            }
        }
        // 如果 reselectInvokers 集合不为空的话，使用负载均衡进行选择
        if (!reselectInvokers.isEmpty()){
            return loadBalance.select(invocation, reselectInvokers, getURL());
        }

        // 若线程走到此处，说明 reselectInvokers 集合为空，也就是说 selected 集合之外的 invoker 都不可用。
        // 所以接着从 selected 列表中查找可用的 Invoker，并将其添加到 reselectInvokers 集合中
        if (selected != null) {
            for (Invoker invoker : selected) {
                if (invoker.isAvailable())
                    reselectInvokers.add(invoker);
            }
            if (!reselectInvokers.isEmpty()){
                return loadBalance.select(invocation, reselectInvokers, getURL());
            }
        }

        // 表明没有符合条件的 invoker，直接返回 null
        return null;
    }

    @Override
    public boolean isAvailable() {
        if (stickyInvoker != null)
            return stickyInvoker.isAvailable();
        return directory.isAvailable();
    }

    @Override
    public Class<?> getInterface() {
        return directory.getInterface();
    }

    @Override
    public URL getURL() {
        return directory.getURL();
    }

    @Override
    public void destroy() {
        if (destroyed.compareAndSet(false, true)){
            directory.destroy();
        }
    }

    public boolean isDestroyed(){
        return destroyed.get();
    }

    protected List<Invoker> list(RpcInvocation invocation){
        return directory.getInvokers(invocation);
    }

    public abstract Object doInvoke(RpcInvocation invocation, List<Invoker> invokers, LoadBalance loadBalance) throws RpcException;

}