package com.xu.rpc.protocol;

public interface Exporter<T> {

    public Invoker<T> getInvoker();

    public void unexport();

}
