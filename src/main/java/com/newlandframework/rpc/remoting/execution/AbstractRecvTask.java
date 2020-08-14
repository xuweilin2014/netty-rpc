package com.newlandframework.rpc.remoting.execution;

import com.newlandframework.rpc.model.MessageRequest;
import com.newlandframework.rpc.model.MessageResponse;
import com.newlandframework.rpc.protocol.rpc.RpcInvoker;
import com.newlandframework.rpc.remoting.handler.ChannelHandler;
import com.newlandframework.rpc.remoting.handler.ReplyHandler;
import com.newlandframework.rpc.util.BeanFactoryUtil;
import io.netty.channel.Channel;
import org.apache.log4j.Logger;

import java.io.PrintWriter;
import java.io.StringWriter;


public abstract class AbstractRecvTask implements Runnable {

    private static final Logger logger = Logger.getLogger(AbstractRecvTask.class);

    protected MessageRequest request;

    protected MessageResponse response;

    protected Channel channel;

    protected static final String METHOD_MAPPED_NAME = "invoke";

    protected MethodInvokeStatus invokeStatus = MethodInvokeStatus.INIT;

    protected long invokeTimespan;

    protected ReplyHandler handler;

    public AbstractRecvTask(MessageRequest request, ChannelHandler handler, Channel channel) {
        this.request = request;
        this.handler = (ReplyHandler) handler;
        this.channel = channel;
    }

    @Override
    public void run() {
        response = new MessageResponse();
        try {
            response.setMessageId(request.getMessageId());

            Object result = handler.reply(request, channel);

            if (request.getRejected()){
                logger.error("illegal request, netty rpc server refused to respond.");

                invokeStatus = MethodInvokeStatus.REJECTED;
                response.setResult(null);
                response.setError(new Throwable("illegal request, netty rpc server refused to respond."));
            }else{
                logger.info("rpc request " + request.getMethodName() + " in " + request.getInterfaceName() + " is executed successfully. ");

                // 调用本地方法成功的话，就将结果信息封装到MessageResponse对象中
                invokeStatus = MethodInvokeStatus.DONE;
                response.setResult(result);
                response.setError(null);
            }
        } catch (Throwable t) {
            logger.error("rpc server invoke error!\n" + t.getMessage());

            // 如果通过反射调用方法的过程中发生了异常，并且这个异常没有被捕获的话，就会在此被捕获，并且设置到MessageResponse的error属性中，并且打印出来。
            invokeStatus = MethodInvokeStatus.EXCEPTIONAL;
            response.setResult(null);
            response.setError(t);
        } finally {

            // 设置方法调用的结果，以供客户端进行对应判断与处理
            response.setInvokeStatus(invokeStatus);
        }
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

