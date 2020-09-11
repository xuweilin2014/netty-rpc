package com.xu.rpc.filter.support;

import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcInvocation;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.core.extension.Activate;
import com.xu.rpc.exception.RpcException;
import com.xu.rpc.filter.ChainFilter;
import com.xu.rpc.protocol.Invoker;

/**
 * 令牌验证
 *
 * 如果某些服务提供者不希望消费者绕过注册中心直连自己，可以使用令牌验证。基本的原理是服务提供者在
 * 发布服务的时候会生成令牌，并且与服务一起注册到注册中心。消费者必须通过注册中心才能获取到token
 */
@Activate(group = {RpcConfig.PROVIDER}, value = RpcConfig.TOKEN_KEY, order = 2)
public class TokenChainFilter implements ChainFilter {

    @Override
    public RpcResult intercept(Invoker invoker, RpcInvocation invocation) throws RpcException {
        String token = invoker.getUrl().getParameter(RpcConfig.TOKEN_KEY);
        // token 值为空，说明此服务没有开启令牌验证，直接调用 invoke 方法
        if (token == null || token.length() == 0 || RpcConfig.FALSE.equals(token)) {
            return invoker.invoke(invocation);
        } else{
            String consumerToken = invocation.getToken();
            // 消费者的 token 值和服务区端指定的 token 值相同，直接调用 invoke 方法
            if (token.equals(consumerToken)){
                return invoker.invoke(invocation);
            // 消费者的 token 值和服务器端指定的 token 值不同，禁止调用此服务
            }else{
                throw new RpcException("forbidden to invoke service " + invoker.getInterface().getName()
                        + " for method " + invocation.getMethodName() + " since token is invalid.");
            }
        }
    }

}
