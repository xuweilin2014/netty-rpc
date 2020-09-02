package com.xu.rpc.spring.bean;

public class NettyRpcApplication {
    // 是否开启监控
    private String metrics;

    // 应用的名称
    private String name;

    public String getMetrics() {
        return metrics;
    }

    public void setMetrics(String metrics) {
        this.metrics = metrics;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
