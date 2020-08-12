package com.newlandframework.rpc.protocol;

public interface Exporter {

    public Invoker getInvoker();

    public void unexport();

}
