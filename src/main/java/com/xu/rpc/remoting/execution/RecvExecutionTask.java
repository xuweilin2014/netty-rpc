package com.xu.rpc.remoting.execution;

import com.xu.rpc.core.RpcResult;
import com.xu.rpc.model.MessageRequest;
import com.xu.rpc.model.MessageResponse;
import com.xu.rpc.remoting.handler.ChannelHandler;
import com.xu.rpc.remoting.handler.ReplyHandler;
import io.netty.channel.Channel;
import org.apache.log4j.Logger;

import java.io.PrintWriter;
import java.io.StringWriter;


public class RecvExecutionTask implements Runnable {

    private static final Logger logger = Logger.getLogger(RecvExecutionTask.class);

    protected MessageRequest request;

    protected MessageResponse response;

    protected Channel channel;

    protected MethodInvokeStatus invokeStatus = MethodInvokeStatus.INIT;

    protected long invokeTimespan;

    protected ReplyHandler handler;

    public RecvExecutionTask(MessageRequest request, ChannelHandler handler, Channel channel) {
        this.request = request;
        this.handler = (ReplyHandler) handler;
        this.channel = channel;
    }

    @Override
    public void run() {
        response = new MessageResponse();
        response.setMessageId(request.getMessageId());

        RpcResult result = (RpcResult) handler.reply(request, channel);

        if (result.getResult() != null) {
            logger.info("rpc request " + request.getMethodName() + " in " + request.getInterfaceName() + " is executed successfully. ");
            // 调用本地方法成功的话，就将结果信息封装到MessageResponse对象中
            invokeStatus = MethodInvokeStatus.DONE;
            response.setResult(result.getResult());
            response.setError(null);
        }else if (result.getException() != null){
            logger.error("rpc server invoke error!\n" + result.getException().getMessage());
            // 如果通过反射调用方法的过程中发生了异常，并且这个异常没有被捕获的话，就会在此被捕获，并且设置到MessageResponse的error属性中，并且打印出来。
            invokeStatus = MethodInvokeStatus.EXCEPTIONAL;
            response.setResult(null);
            response.setError(result.getException());
        }else{
            //noinspection GrazieInspection
            throw new IllegalStateException("invoke result error, result and exception is not configured.");
        }

        // 设置方法调用的结果，以供客户端进行对应判断与处理
        response.setInvokeStatus(invokeStatus);
        channel.writeAndFlush(response);
    }


    public MethodInvokeStatus getInvokeStatus() {
        return invokeStatus;
    }

    public void setInvokeStatus(MethodInvokeStatus invokeStatus) {
        this.invokeStatus = invokeStatus;
    }

    public String getStackTrace(Throwable ex) {
        StringWriter buf = new StringWriter();
        ex.printStackTrace(new PrintWriter(buf));

        return buf.toString();
    }


    public MessageRequest getRequest() {
        return request;
    }

    public void setRequest(MessageRequest request) {
        this.request = request;
    }
}

