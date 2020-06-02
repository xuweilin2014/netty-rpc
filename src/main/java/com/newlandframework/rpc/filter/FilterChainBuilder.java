package com.newlandframework.rpc.filter;

import com.newlandframework.rpc.core.ChainFilterInvoker;
import com.newlandframework.rpc.core.ChainFilterInvokerProvider;
import com.newlandframework.rpc.model.MessageRequest;

import java.util.List;

/**
 * FilterChainBuilder类是用来形成filter的调用链
 *
 * 举例来说，如果在XML文件中，配置的过滤器链中有两个ChainFilter：filterA、filterB。那么就会把每一个filter包装成一个ChainFilterInvoker对象，
 * 并且形成如下结构：
 *
 * |----------------------|           |----------------------|          |----------------------|
 * |  |---------------|   |           |  |---------------|   |          |  |---------------|   |
 * |  | ChainFilterA  |   | --------> |  | ChainFilterB  |   | -------> |  | MethodInvoker |   |
 * |  |---------------|   |           |  |---------------|   |          |  |---------------|   |
 * |----------------------|           |----------------------|          |----------------------|
 *    ChainFilterInvokerA               ChainFilterInvokerB                ChainFilterInvokerC
 *
 * 通过FilterChainBuilder中的doBuildChain方法，将配置的每一个ChainFilter包装成一个ChainFilterInvoker对象，然后把ChainFilterInvoker
 * 连接起来，形成一个类似于链表的结构（但其实不是链表），最后返回第一个ChainFilterInvoker对象，用来进行链式调用。其中，最后一个ChainFilterInvoker中
 * 封装的不是普通的ChainFilter，而是MethodInvoker对象，用来真正执行客户端要求调用的方法。
 *
 */
public class FilterChainBuilder{

    private List<ChainFilter> filters;

    public FilterChainBuilder() {
    }

    public <T> ChainFilterInvokerProvider<T> buildChain(ChainFilterInvoker<T> invoker, MessageRequest request) {
        ChainFilterInvoker<T> firstInvoker = doBuildChain(invoker);

        return new ChainFilterInvokerProvider<T>() {
            @Override
            public ChainFilterInvoker<T> getInvoker() {
                return firstInvoker;
            }

            @Override
            public void destroyInvoker() {
                firstInvoker.destroy();
            }
        };
    }


    private <T> ChainFilterInvoker<T> doBuildChain(ChainFilterInvoker<T> invoker) {
        ChainFilterInvoker<T> first = invoker;

        if (filters.size() > 0) {
            for (int i = filters.size() - 1; i >= 0; i--) {
                ChainFilter filter = filters.get(i);
                ChainFilterInvoker<T> next = first;
                first = new ChainFilterInvoker<T>() {
                    //在ChainFilterInvoker中的invoke方法，用来真正调用ChainFilter的intercept方法，可以对客户端请求进行处理。
                    //在ChainFilter中的intercept方法中，会传入ChainFilterInvoker链中的下一个ChainFilterInvoker对象，在这个对象中又封装了
                    //下一个ChainFilter，因此调用可以继续下去
                    @Override
                    public Object invoke(MessageRequest request) throws Throwable {
                        return filter.intercept(next, request);
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
        return first;
    }

    public List<ChainFilter> getFilters() {
        return filters;
    }

    public void setFilters(List<ChainFilter> filters) {
        this.filters = filters;
    }
}

