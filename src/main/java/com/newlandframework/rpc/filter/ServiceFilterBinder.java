package com.newlandframework.rpc.filter;


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

