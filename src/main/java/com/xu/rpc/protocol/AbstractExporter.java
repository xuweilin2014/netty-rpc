package com.xu.rpc.protocol;

import java.util.concurrent.atomic.AtomicBoolean;

public class AbstractExporter<T> implements Exporter<T> {

    private final Invoker<T> invoker;

    private final AtomicBoolean unexported = new AtomicBoolean(false);

    public AbstractExporter(Invoker<T> invoker){
        if (invoker == null)
            throw new IllegalArgumentException("invoker cannot be null");
        this.invoker = invoker;
    }

    @Override
    public Invoker<T> getInvoker() {
        return invoker;
    }

    @Override
    public void unexport() {
        if (unexported.compareAndSet(false, true)){
            invoker.destroy();
        }
    }

}
