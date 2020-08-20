package com.xu.rpc.protocol.rpc;

import com.xu.rpc.exception.RpcException;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.protocol.AbstractInvoker;
import com.xu.rpc.util.URL;

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

