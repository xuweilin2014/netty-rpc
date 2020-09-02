package com.xu.rpc.protocol;

public interface Exporter<T> {

    public Invoker getInvoker();

    public void unexport();

}
