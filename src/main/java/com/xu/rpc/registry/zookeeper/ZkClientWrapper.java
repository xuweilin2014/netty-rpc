package com.xu.rpc.registry.zookeeper;

import com.xu.rpc.registry.KeeperStateListener;
import com.xu.rpc.util.Assert;
import org.I0Itec.zkclient.IZkChildListener;
import org.I0Itec.zkclient.IZkStateListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.apache.zookeeper.Watcher.Event.KeeperState;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * ZkClientWrapper 本身对 ZkClient 进行了一个简单的封装，增加了如下功能：
 * 1.连接超时检测
 * 2.ZkClientWrapper 本身可以接受注册监听器 KeeperStateListener，当 Zookeeper 注册中心的状态发生变化时，就会
 * 回调注册的监听器，并传入此时 Zookeeper 的状态。
 */
public class ZkClientWrapper {

    public static Logger logger = Logger.getLogger(ZkClientWrapper.class);

    private ZkClient zkClient;

    private volatile KeeperState state = KeeperState.SyncConnected;

    private List<KeeperStateListener> listeners = new CopyOnWriteArrayList<>();

    public ZkClientWrapper(String address, int connectionTimeout) {
        // address 是 Zookeeper 的地址，而 Integer.MAX_VALUE 则是连接超时时间，如果超过这个时间还没有连上注册中心，就会抛出异常
        try{
            zkClient =  new ZkClient(address, connectionTimeout);
        }catch (Throwable t){
            logger.warn("zookeeper timeout! cannot connect to zookeeper in " + connectionTimeout + " ms, please check it.");
        }

        subscribeStateChanges(new IZkStateListener() {
            @Override
            public void handleStateChanged(KeeperState state) throws Exception {
                ZkClientWrapper.this.state = state;
            }

            @Override
            public void handleNewSession() throws Exception {
                callbackListeners(KeeperStateListener.RECONNECTED);
            }

            @Override
            public void handleSessionEstablishmentError(Throwable error) throws Exception {
            }
        });
    }

    private void callbackListeners(int state) {
        for (KeeperStateListener listener : listeners) {
            listener.stateChanged(state);
        }
    }

    public void addKeeperStateListener(KeeperStateListener listener){
        Assert.notNull(listener, "KeeperStateListener cannot be null");
        listeners.add(listener);
    }

    public void subscribeStateChanges(IZkStateListener listener) {
        Assert.notNull(zkClient, new IllegalStateException("zkClient is null, cannot connect to zookeeper!"));
        Assert.notNull(listener, "IZkStateListener cannot be null.");
        zkClient.subscribeStateChanges(listener);
    }

    public void createEphemeral(String path) {
        Assert.notNull(zkClient, new IllegalStateException("zkClient is null, cannot connect to zookeeper!"));
        zkClient.createEphemeral(path);
    }

    public void createPersistent(String path) {
        Assert.notNull(zkClient, new IllegalStateException("zkClient is null, cannot connect to zookeeper!"));
        zkClient.createPersistent(path);
    }

    public void delete(String path){
        Assert.notNull(zkClient, new IllegalStateException("zkClient is null, cannot connect to zookeeper!"));
        zkClient.delete(path);
    }

    public List<String> subscribeChildChanges(String path, IZkChildListener listener){
        Assert.notNull(zkClient, new IllegalStateException("zkClient is null, cannot connect to zookeeper!"));
        Assert.notNull(listener, new IllegalStateException("listener is null."));
        return zkClient.subscribeChildChanges(path, listener);
    }

    public void unsubscribeChildChanges(String path, IZkChildListener listener){
        Assert.notNull(zkClient, new IllegalStateException("zkClient is null, cannot connect to zookeeper!"));
        Assert.notNull(listener, new IllegalStateException("listener is null."));
        zkClient.unsubscribeChildChanges(path, listener);
    }

    public void close(){
        Assert.notNull(zkClient, new IllegalStateException("zkClient is null, cannot connect to zookeeper!"));
        zkClient.close();
    }

    public boolean isPathExist(String path) {
        Assert.notNull(zkClient, new IllegalStateException("zkClient is null, cannot connect to zookeeper!"));
        return zkClient.exists(path);
    }

    public boolean isConnected() {
        return state == KeeperState.SyncConnected;
    }
}
