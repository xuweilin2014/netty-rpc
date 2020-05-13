package com.newlandframework.rpc.jmx;

import com.newlandframework.rpc.core.ReflectionUtils;
import com.newlandframework.rpc.core.RpcSystemConfig;
import com.newlandframework.rpc.netty.MessageRecvExecutor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Iterator;

/**
 * HashModuleMetricsVisitor是对ModuleMetricsVisitor进行一层封装。即RPC服务器中每一个接口的每一个方法创建一个visitorList，
 * 长度为SYSTEM_PROPERTY_JMX_METRICS_HASH_NUMS。最后把这些visitorList加入到hashVisitorList中。
 */
public class HashModuleMetricsVisitor {
    private List<List<ModuleMetricsVisitor>> hashVisitorList = new ArrayList<>();

    private static final HashModuleMetricsVisitor INSTANCE = new HashModuleMetricsVisitor();

    private HashModuleMetricsVisitor() {
        init();
    }

    public static HashModuleMetricsVisitor getInstance() {
        return INSTANCE;
    }

    public int getHashModuleMetricsVisitorListSize() {
        return hashVisitorList.size();
    }

    private void init() {
        Map<String, Object> map = MessageRecvExecutor.getInstance().getHandlerMap();
        ReflectionUtils utils = new ReflectionUtils();
        //集合s中保存的为RPC服务器的所有接口
        Set<String> s = map.keySet();
        Iterator<String> iter = s.iterator();
        String key;
        while (iter.hasNext()) {
            key = iter.next();
            try {
                //获取一个接口中的所有方法签名
                List<String> list = utils.getClassMethodSignature(Class.forName(key));
                for (String signature : list) {
                    List<ModuleMetricsVisitor> visitorList = new ArrayList<>();
                    //为每一个方法签名都创建一个visitorList，长度为JMX_METRICS_HASH_NUMS，并且创建之后加入到hashVisitorList
                    for (int i = 0; i < RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_HASH_NUMS; i++) {
                        ModuleMetricsVisitor visitor = new ModuleMetricsVisitor(key, signature);
                        visitor.setHashKey(i);
                        visitorList.add(visitor);
                    }
                    hashVisitorList.add(visitorList);
                }
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            }
        }
    }

    public void signal() {
        ModuleMetricsHandler.getInstance().getLatch().countDown();
    }

    public List<List<ModuleMetricsVisitor>> getHashVisitorList() {
        return hashVisitorList;
    }

}

