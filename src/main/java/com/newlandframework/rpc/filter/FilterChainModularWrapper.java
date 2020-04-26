package com.newlandframework.rpc.filter;

import com.newlandframework.rpc.core.Modular;
import com.newlandframework.rpc.core.ModuleInvoker;
import com.newlandframework.rpc.core.ModuleProvider;
import com.newlandframework.rpc.model.MessageRequest;

import java.util.List;


public class FilterChainModularWrapper implements Modular {
    private Modular modular;
    private List<ChainFilter> filters;

    public FilterChainModularWrapper(Modular modular) {
        if (modular == null) {
            throw new IllegalArgumentException("module is null");
        }
        this.modular = modular;
    }

    @Override
    public <T> ModuleProvider<T> getProvider(ModuleInvoker<T> invoker, MessageRequest request) {
        return modular.getProvider(buildChain(invoker), request);
    }

    private <T> ModuleInvoker<T> buildChain(ModuleInvoker<T> invoker) {
        ModuleInvoker last = invoker;

        if (filters.size() > 0) {
            for (int i = filters.size() - 1; i >= 0; i--) {
                ChainFilter filter = filters.get(i);
                ModuleInvoker<T> next = last;
                last = new ModuleInvoker<T>() {
                    @Override
                    public Object invoke(MessageRequest request) throws Throwable {
                        return filter.invoke(next, request);
                    }

                    @Override
                    public Class<T> getInterface() {
                        return invoker.getInterface();
                    }

                    @Override
                    public String toString() {
                        return invoker.toString();
                    }

                    @Override
                    public void destroy() {
                        invoker.destroy();
                    }
                };
            }
        }
        return last;
    }

    public List<ChainFilter> getFilters() {
        return filters;
    }

    public void setFilters(List<ChainFilter> filters) {
        this.filters = filters;
    }
}

