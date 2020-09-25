package com.xu.rpc.cluster.loadbalance;

import com.xu.rpc.cluster.LoadBalancer;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.commons.URL;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public abstract class AbstractLoadBalancer implements LoadBalancer {

    private static Map<String, Integer> map = new ConcurrentHashMap<>();

    static {
        map.put("169.254.207.250:10880", 0);
        map.put("169.254.207.250:10881", 0);
        map.put("169.254.207.250:10882", 0);
        map.put("169.254.207.250:10883", 0);
    }

    @Override
    public Invoker select(RpcInvocation invocation, List<Invoker> invokers, URL url) throws RpcException {
        if (invokers == null || invokers.size() == 0)
            return null;

        if (invokers.size() == 1)
            return invokers.get(0);

        Invoker invoker = doSelect(invocation, invokers, url);
        System.out.println("choose invoker for url " + invoker.getUrl());

        Integer times = map.get(invoker.getUrl().getAddress());
        map.put(invoker.getUrl().getAddress(), times + 1);
        System.out.println(map);

        return invoker;
    }

    public abstract Invoker doSelect(RpcInvocation invocation, List<Invoker> invokers, URL url) throws RpcException;
}
