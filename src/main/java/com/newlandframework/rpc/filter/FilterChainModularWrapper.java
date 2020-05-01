package com.newlandframework.rpc.filter;

import com.newlandframework.rpc.core.ModularProviderHolder;
import com.newlandframework.rpc.core.ModularInvoker;
import com.newlandframework.rpc.core.ModularProvider;
import com.newlandframework.rpc.model.MessageRequest;

import java.util.List;

/**
 * 此类主要用来形成filter的调用链，可以把filters中的每一个filter都包装成ModularInvoker对象（通过buildChain方法），
 * 举例来说，如果filters中有两个filter：filterA、filterB。那么就从filters的最后面开始遍历，把filterB包装成ModularInvoker对象，
 * 此ModularInvoker对象是一个内部类，在它的invoke方法中，会调用filterB的invoke对象，并且把next对象传进去。
 *
 * next对象就是buildChain传进来的invoker对象，也是ModularInvoker类型，它是在AbstractMessageRecvInitializeTask类中的一个内部类，
 * 它的invoke方法会调用目标类MethodInvoker的invoke方法（也就是最终调用RPC的服务应用）。
 *
 * 接下来，循环遍历到了filterA，然后这时next的值就是刚刚对filterB封装好的ModularInvoker对象。此时把filterA封装成一个ModularInvoker对象，
 * 在其invoke方法中，会调用filterA的invoke方法，然后把next对象传进去。最后返回这个ModularInvoker对象，也就是last。
 */
public class FilterChainModularWrapper implements ModularProviderHolder {
    private ModularProviderHolder modular;
    private List<ChainFilter> filters;

    public FilterChainModularWrapper(ModularProviderHolder modular) {
        if (modular == null) {
            throw new IllegalArgumentException("modular is null");
        }
        this.modular = modular;
    }

    @Override
    public <T> ModularProvider<T> getProvider(ModularInvoker<T> invoker, MessageRequest request) {
        return modular.getProvider(buildChain(invoker), request);
    }

    private <T> ModularInvoker<T> buildChain(ModularInvoker<T> invoker) {
        ModularInvoker last = invoker;

        if (filters.size() > 0) {
            for (int i = filters.size() - 1; i >= 0; i--) {
                ChainFilter filter = filters.get(i);
                ModularInvoker<T> next = last;
                last = new ModularInvoker<T>() {
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

