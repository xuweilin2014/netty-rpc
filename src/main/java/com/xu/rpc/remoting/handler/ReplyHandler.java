package com.xu.rpc.remoting.handler;

import com.xu.rpc.exception.RemotingException;
import com.xu.rpc.model.MessageRequest;
import io.netty.channel.Channel;

public abstract class ReplyHandler extends AbstractHandlerDelegate {

    public abstract Object reply(Object message, Channel channel) throws RemotingException;

}
