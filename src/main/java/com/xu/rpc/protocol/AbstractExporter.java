package com.xu.rpc.protocol;

public class AbstractExporter implements Exporter {

    private Invoker invoker;

    public AbstractExporter(Invoker invoker){
        if (invoker == null)
            throw new IllegalArgumentException("invoker cannot be null");
        this.invoker = invoker;
    }

    @Override
    public Invoker getInvoker() {
        return invoker;
    }

    @Override
    public void unexport() {
        // TODO: 2020/8/9
    }
}
