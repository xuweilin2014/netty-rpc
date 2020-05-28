package com.newlandframework.rpc.filter;


/**
 * ServiceFilterBinder用来保存RPC服务端的服务实现类，以及对应的过滤器
 */
public class ServiceFilterBinder {
    private Object object;
    private Filter filter;

    public Object getObject() {
        return object;
    }

    public void setObject(Object object) {
        this.object = object;
    }

    public Filter getFilter() {
        return filter;
    }

    public void setFilter(Filter filter) {
        this.filter = filter;
    }
}

