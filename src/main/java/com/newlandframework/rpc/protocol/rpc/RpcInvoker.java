package com.newlandframework.rpc.protocol.rpc;

import com.newlandframework.rpc.exception.RpcException;
import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.protocol.AbstractInvoker;
import com.newlandframework.rpc.protocol.AbstractProxyInvoker;
import com.newlandframework.rpc.util.URL;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.apache.commons.lang3.time.StopWatch;

import java.lang.reflect.InvocationTargetException;

/**
 * RpcInvoker#invoke方法通过向服务器发起远程调用
 */
public class RpcInvoker extends AbstractInvoker {


    @Override
    public Object doInvoke(MessageRequest request) throws RpcException {
        return null;
    }

    @Override
    public URL getURL() {
        return null;
    }
}

