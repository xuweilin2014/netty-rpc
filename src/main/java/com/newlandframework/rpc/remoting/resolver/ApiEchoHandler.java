package com.newlandframework.rpc.remoting.resolver;

import com.newlandframework.rpc.core.AbilityDetailProvider;
import com.newlandframework.rpc.jmx.ModuleMetricsHtmlBuilder;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpRequest;

import java.io.UnsupportedEncodingException;

import static com.newlandframework.rpc.core.RpcSystemConfig.SYSTEM_PROPERTY_JMX_METRICS_SUPPORT;
import static io.netty.handler.codec.http.HttpResponseStatus.OK;
import static io.netty.handler.codec.http.HttpVersion.HTTP_1_1;

/**
 * ApiEchoHandler的作用如下：
 * i.在浏览器页面中显示Rpc服务端中可以提供的服务详情，其实就是客户端可以调用的对象的接口，在页面中显示这些接口的全类名和方法签名
 * ii.在浏览器中显示Rpc服务端中，每个服务或者说方法被调用的次数，调用的耗时等统计信息
 */
public class ApiEchoHandler extends ChannelInboundHandlerAdapter {
    private static final String CONTENT_TYPE = "Content-Type";
    private static final String CONTENT_LENGTH = "Content-Length";
    private static final String CONNECTION = "Connection";
    private static final String KEEP_ALIVE = "keep-alive";
    private static final String METRICS = "metrics";
    private static final String METRICS_ERR_MSG = "NettyRPC nettyrpc.jmx.invoke.metrics attribute is closed!";

    public ApiEchoHandler() {
    }

    @Override
    public void channelReadComplete(ChannelHandlerContext ctx) {
        ctx.flush();
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) {
        if (msg instanceof HttpRequest) {
            HttpRequest req = (HttpRequest) msg;
            byte[] content = buildResponseMsg(req);
            FullHttpResponse response = new DefaultFullHttpResponse(HTTP_1_1, OK, Unpooled.wrappedBuffer(content));
            response.headers().set(CONTENT_TYPE, "text/html");
            response.headers().set(CONTENT_LENGTH, response.content().readableBytes());
            response.headers().set(CONNECTION, KEEP_ALIVE);
            ctx.write(response);
        }
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        cause.printStackTrace();
        ctx.close();
    }

    private byte[] buildResponseMsg(HttpRequest req) {
        byte[] content = null;

        //http请求的uri中是否包括metrics，即是否表明要获取NettyRPC模块调用情况
        boolean metrics = (req.getUri().indexOf(METRICS) != -1);

        /*
         * 1.如果系统支持JMX，并且metrics为true的话（也就是用户请求获取NettyRPC模块调用情况），就会构造调用信息，并且传递给content。
         * 2.如果系统不支持JMX，并且metrics为true的话，就会直接返回"NettyRPC nettyrpc.jmx.invoke.metrics attribute is closed!"
         * 3.如果metrics为false的话，表明用户只是想知道NettyRPC服务器端可以提供的能力，即可以调用的接口信息
         */

        if (SYSTEM_PROPERTY_JMX_METRICS_SUPPORT && metrics) {
            try {
                //返回构造的content，用来在网页上显示RPC服务器中每个方法的调用具体信息，比如：调用次数、调用成功次数、
                //调用失败次数等等
                content = ModuleMetricsHtmlBuilder.getInstance().buildModuleMetrics().getBytes("GBK");
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
        } else if (!SYSTEM_PROPERTY_JMX_METRICS_SUPPORT && metrics) {
            content = METRICS_ERR_MSG.getBytes();
        } else {
            AbilityDetailProvider provider = new AbilityDetailProvider();
            //返回的content表明NettyRPC服务器端可以被调用的接口信息
            content = provider.listAbilityDetail(true).toString().getBytes();
        }
        return content;
    }
}

