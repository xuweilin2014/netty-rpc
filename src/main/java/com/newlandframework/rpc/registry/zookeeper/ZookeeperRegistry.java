package com.newlandframework.rpc.registry.zookeeper;

import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.registry.FailbackRegistry;
import com.newlandframework.rpc.registry.KeeperStateListener;
import com.newlandframework.rpc.util.URL;
import org.I0Itec.zkclient.IZkStateListener;
import org.apache.zookeeper.Watcher;

import java.util.concurrent.atomic.AtomicBoolean;

public class ZookeeperRegistry extends FailbackRegistry {

    // 使用 ZkClient 作为客户端
    private ZkClientWrapper zookeeperClient;

    public ZookeeperRegistry(URL url) throws Exception {
        zookeeperClient = new ZkClientWrapper(url.getAddress(), RpcSystemConfig.ZOOKEEPER_TIMEOUT);
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
        try {
            // 默认创建的节点为临时节点，当此 server 下线时，这些临时的节点会被删除
            zookeeperClient.createEphemeral(toRegistryPath(url));
        }catch (Throwable t){
            throw new RpcException("failed to register " + url.toFullString() + " to zookeeper "
                    + url.getAddress() +" , please check.", t);
        }
    }

    @Override
    public void doUnregister(URL url) {
        try{
            zookeeperClient.delete(toRegistryPath(url));
        }catch (Throwable t){
            throw new RpcException("failed to delete " + url.toFullString() + " , please check.", t);
        }
    }

    @Override
    public void doSubscribe(URL url) {
        // TODO: 2020/8/12
    }

    @Override
    public void doUnsubscribe(URL url) {
        // TODO: 2020/8/12
    }

    private String toRegistryDir(URL url){
        return RpcSystemConfig.ROOT_DIR + RpcSystemConfig.DIR_SEPARATOR + url.getServiceName();
    }

    private String toRegistryPath(URL url) {
        return toRegistryDir(url) + RpcSystemConfig.DIR_SEPARATOR + url.toFullString();
    }

    public void destroy(){

    }

}
