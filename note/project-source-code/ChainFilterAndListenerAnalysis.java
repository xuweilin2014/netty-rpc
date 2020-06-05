public class ChainFilterAndListenerAnalysis{

    public interface ModularProviderHolder {
        <T> ModuleProvider<T> getProvider(ModularInvoker<T> invoker, MessageRequest request);
    }

    public interface ModularInvoker<T> {
        Class<T> getInterface();
        Object invoke(MessageRequest request) throws Throwable;
        void destroy();
    }

    public interface ModuleProvider<T> {
        ModularInvoker<T> getInvoker();
        void destoryInvoker();
    }

    public class DefaultModular implements ModularProviderHolder {
        @Override
        public <T> ModuleProvider<T> getProvider(ModuleInvoker<T> invoker, MessageRequest request) {
            return new ModuleProvider<T>() {
                @Override
                public ModuleInvoker<T> getInvoker() {
                    return invoker;
                }
    
                @Override
                public void destoryInvoker() {
                    invoker.destroy();
                }
            };
        }
    }

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
        // modular类型是DefaultModular
        private ModularProviderHolder modular;
        // filters中有两个ClassLoaderChainFilter和EchoChainFilter
        private List<ChainFilter> filters;
    
        public FilterChainModularWrapper(ModularProviderHolder modular) {
            if (modular == null) {
                throw new IllegalArgumentException("modular is null");
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
    
        // getter和setter方法....
    }

    /**
     * class:AbstractMessageRecvInitializeTask
     * 这里的modular对象是FilterChainModularWrapper，而调用其provider.getInvoker方法就会返回对filterA进行封装的ModularInvoker，
     * 然后调用invoke方法最终会调用到filterA的invoke方法，并且把filter链中下一个filter（也就是filterB）封装成的ModularInvoker对象（next）传进去。
     * 然后在filterA的invoke方法中继续调用next的invoke方法，紧接着就会调用filterB的invoke方法，最后调用到目标对象MethodInvoker中的invoke方法。
     * 
     * 也就是说，在真正调用RPC服务之前（MethodInvoker中的invoke通过反射调用），会通过XML配置文件所配置filter链，对RPC请求进行处理。
     */ 
    private Object invoke(MethodInvoker mi, MessageRequest request) throws Throwable {
        if (modular != null) {
            ModularProvider provider = modular.getProvider(new ModularInvoker() {
                @Override
                public Class getInterface() {
                    return mi.getClass().getInterfaces()[0];
                }
                @Override
                public Object invoke(MessageRequest request) throws Throwable {
                    return mi.invoke(request);
                }
                @Override
                public void destroy() {
                }
            }, request);
            return provider.getInvoker().invoke(request);
        } else {
            return mi.invoke(request);
        }
    }

}