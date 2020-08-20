package com.xu.rpc.registry.zookeeper;

import com.sun.org.apache.bcel.internal.generic.FADD;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.registry.FailbackRegistry;
import com.xu.rpc.registry.KeeperStateListener;
import com.xu.rpc.registry.NotifyListener;
import com.xu.rpc.util.URL;
import org.I0Itec.zkclient.IZkChildListener;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

public class ZookeeperRegistry extends FailbackRegistry {

    // 使用 ZkClient 作为客户端
    private ZkClientWrapper zookeeperClient;

    private AtomicBoolean closed = new AtomicBoolean(false);

    private Map<NotifyListener, IZkChildListener> listeners = new ConcurrentHashMap<>();

    public ZookeeperRegistry(URL url) {
        super(url);
        zookeeperClient = new ZkClientWrapper(url.getAddress(), RpcConfig.ZOOKEEPER_TIMEOUT);
        zookeeperClient.addKeeperStateListener(new KeeperStateListener() {
            @Override
            public void stateChanged(int state) {
                if (state == KeeperStateListener.RECONNECTED){
                    // 当客户端因为网络抖动的原因和 Zookeeper 断开连接，然后在 sessionTimeout 时间段之内没有连接上
                    // 注册中心，那么这个时候，session 就会被认为是 expired，各种临时节点以及 Watcher 就会被移除掉。
                    // 如果之后，客户端又连接上了 Zookeeper，那么就会被认为是一个 new session，接着调用这里的 recover
                    // 方法，重新添加临时节点，以及各种 Watcher
                    recover();
                }
            }
        });
    }

    @Override
    public void doRegister(URL url) {
        if (closed.get())
            return;
        try {
            // 创建的节点目录结构为 /rpc/com.xxx.ServiceName/url
            // 其中，/rpc 和 /rpc/com.xxx.ServiceName 都是永久节点，而 /rpc/com.xxx.ServiceName/url 则是临时节点
            // 因此，在创建的过程中，如果创建的节点是临时节点和永久节点必须要分情况讨论
            create(toRegistryPath(url), true);
        }catch (Throwable t){
            throw new RpcException("failed to register " + url.toFullString() + " to zookeeper "
                    + url.getAddress() +" , please check.", t);
        }
    }

    @Override
    public void doUnregister(URL url) {
        if (closed.get())
            return;
        try{
            zookeeperClient.delete(toRegistryPath(url));
        }catch (Throwable t){
            throw new RpcException("failed to delete " + url.toFullString() + " , please check.", t);
        }
    }

    @SuppressWarnings("Java8MapApi")
    @Override
    public void doSubscribe(URL url, NotifyListener listener) {
        if (closed.get())
            return;

        IZkChildListener zkChildListener = listeners.get(listener);
        if (zkChildListener == null){
            // 在服务目录下注册一个子节点状态监听器，当添加或者删除节点之后，都会回调这个监听器，并且随后调用 notify 方法
            // 需要注意的是，每当目录下的子节点状态发生变化时，就会把目录下的所有子节点返回
            // 目录的结构类似于 /rpc/com.xxx.ServiceName/url1...
            // 由于项目只在注册中心上注册提供者信息，因此在 service name 之下没有 providers、routers 等目录划分
            zkChildListener = new IZkChildListener() {
                @Override
                public void handleChildChange(String parentPath, List<String> currentChilds) throws Exception {
                    ZookeeperRegistry.this.notify(url, listener, toURLs(currentChilds));
                }
            };
            listeners.put(listener, zkChildListener);
        }

        List<String> childs = zookeeperClient.subscribeChildChanges(toRegistryDir(url), zkChildListener);
        notify(url, listener, toURLs(childs));
    }

    @Override
    public void doUnsubscribe(URL url, NotifyListener listener) {
        if (closed.get())
            return;

        IZkChildListener zkChildListener = listeners.get(listener);
        if (zkChildListener == null){
            throw new IllegalStateException("listener haven't been registered on the zookeeper, cannot unsubscribe it.");
        }
        zookeeperClient.unsubscribeChildChanges(toRegistryDir(url), zkChildListener);
    }

    // rpc/com.xxx.ServiceName/url 中前面两个都是永久节点，而后面的 url 则是临时节点
    private void create(String path, boolean ephemeral){
        int index = path.lastIndexOf('/');
        if (index > 0){
            String parent = path.substring(0, index);
            if (!zookeeperClient.isPathExist(parent)){
                create(parent, false);
            }
        }

        if (ephemeral){
            zookeeperClient.createEphemeral(path);
        }else{
            zookeeperClient.createPersistent(path);
        }
    }

    private String toRegistryDir(URL url){
        return RpcConfig.ROOT_DIR + RpcConfig.DIR_SEPARATOR + url.getServiceName();
    }

    private String toRegistryPath(URL url) {
        return toRegistryDir(url) + RpcConfig.DIR_SEPARATOR + url.toFullString();
    }

    private List<URL> toURLs(List<String> childs){
        List<URL> urls = new ArrayList<>();
        for (String child : childs) {
            urls.add(URL.valueOf(child));
        }
        return urls;
    }

    public void destroy(){
        if (closed.compareAndSet(false, true)) {
            super.destroy();
            try {
                zookeeperClient.close();
            } catch (Exception e) {
                logger.error("failed to close zookeeper client, caused by " + e.getMessage());
            }
        }
    }

}
