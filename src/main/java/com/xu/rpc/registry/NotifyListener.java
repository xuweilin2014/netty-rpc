package com.xu.rpc.registry;

import com.xu.rpc.exception.RpcException;
import com.xu.rpc.commons.URL;

import java.util.List;

public interface NotifyListener {

    public void notify(List<URL> urls) throws RpcException;

}
