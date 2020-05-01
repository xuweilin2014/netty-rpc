package com.newlandframework.rpc.async;

import net.sf.cglib.proxy.LazyLoader;

public class AsyncLoadResultInterceptor implements LazyLoader {

    private AsyncCallResult result;

    public AsyncLoadResultInterceptor(AsyncCallResult result) {
        this.result = result;
    }

    @Override
    public Object loadObject() throws Exception {
        return result.loadFuture();
    }

}

