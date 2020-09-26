package com.xu.rpc.remoting.echo;

import com.sun.org.apache.regexp.internal.RE;
import com.xu.rpc.commons.DateStore;
import com.xu.rpc.commons.URL;
import com.xu.rpc.core.AbilityDetailProvider;
import com.xu.rpc.core.RpcConfig;
import com.xu.rpc.core.RpcResult;
import com.xu.rpc.jmx.MetricsHtmlBuilder;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpRequest;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import java.io.UnsupportedEncodingException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArraySet;

import static io.netty.handler.codec.http.HttpResponseStatus.OK;
import static io.netty.handler.codec.http.HttpVersion.HTTP_1_1;

/**
 * ApiEchoHandler的作用如下：
 * i.在浏览器页面中显示Rpc服务端中可以提供的服务详情，其实就是客户端可以调用的对象的接口，在页面中显示这些接口的全类名和方法签名
 * ii.在浏览器中显示Rpc服务端中，每个服务或者说方法被调用的次数，调用的耗时等统计信息
 */
public class ApiEchoHandler extends ChannelInboundHandlerAdapter {

    public static final Logger logger = Logger.getLogger(ApiEchoHandler.class);

    private static final String CONTENT_TYPE = "Content-Type";

    private static final String CONTENT_LENGTH = "Content-Length";

    private static final String CONNECTION = "Connection";

    private static final String KEEP_ALIVE = "keep-alive";

    private static final String METRICS_ERR_MSG = "NettyRPC nettyrpc.jmx.invoke.metrics attribute is closed!";

    private final URL url;

    private final String host;

    private int port;

    public ApiEchoHandler(URL url, String host, int port) {
        this.url = url;
        this.host = host;
        this.port = port;
    }

    @Override
    public void channelReadComplete(ChannelHandlerContext ctx) {
        ctx.flush();
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) {
        if (msg instanceof HttpRequest) {
            HttpRequest req = (HttpRequest) msg;

            if (req.uri().contains(RpcConfig.OVERRIDE_KEY)){
                doMock(req.uri());
                return;
            }

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

        String uri = req.uri();

        // http 请求的 uri 中是否包括 metrics 字符串，即是否表明要获取 netty-rpc 模块调用情况
        boolean metrics = uri.contains(RpcConfig.METRICS_KEY);
        // http 请求的 uri 中是否包括 ability 字符串，即是否表明要获取 netty-rpc 服务能力
        boolean ability = uri.contains(RpcConfig.ABILITY_KEY);
        // metrics 是否开启
        String isMetricsOpen = url.getParameter(RpcConfig.METRICS_KEY, RpcConfig.TRUE);

        // 1.如果监控开启的话，并且 metrics 为 true 的话，就会构造调用信息，并且传递给 content。
        // 2.如果监控没有开启的话，无论 metrics 为什么值，就会直接返回 "NettyRPC nettyrpc.jmx.invoke.metrics attribute is closed!"
        // 3.如果 metrics 为 false 的话，表明用户只是想知道 netty-rpc 服务器端可以提供的能力，即可以调用的接口信息
        if (RpcConfig.TRUE.equals(isMetricsOpen) && metrics) {
            try {
                // 返回构造的 content，用来在网页上显示 netty-rpc 服务器中每个方法的调用具体信息，比如：调用次数、调用成功次数、调用失败次数等等
                content = MetricsHtmlBuilder.getInstance().buildMetrics().getBytes("GBK");
            } catch (UnsupportedEncodingException e) {
                logger.error(e.getMessage());
            }
        } else if (ability) {
            AbilityDetailProvider provider = new AbilityDetailProvider();
            // 返回的 content 表明 netty-rpc 服务器端可以被调用的接口信息
            content = provider.listAbilityDetail(host, port).toString().getBytes();
        } else {
            logger.error(METRICS_ERR_MSG);
            content = METRICS_ERR_MSG.getBytes();
        }

        return content;
    }

    // 对服务进行屏蔽和恢复操作
    @SuppressWarnings({"ConstantConditions", "unchecked"})
    private void doMock(String uri){
        if (!uri.contains(RpcConfig.OVERRIDE_KEY))
            return;

        String[] kvs = URL.decode(uri).substring(uri.indexOf("?") + 1).split("&");
        Map<String, String> values = new HashMap<>();
        for (String kv : kvs) {
            if (kv.contains("=")){
                values.put(kv.split("=")[0], kv.split("=")[1]);
            }
        }

        // set 其实就是 MockChainFilter 中的一个黑名单集合 blacklist，通过这个名单，
        // 来控制哪些服务可以被访问，哪些不可以
        Set<String> set = (Set<String>) DateStore.get(RpcConfig.MOCK_SET_KEY);
        if (set == null){
            DateStore.put(RpcConfig.MOCK_SET_KEY, new CopyOnWriteArraySet<>());
            set = (Set<String>) DateStore.get(RpcConfig.MOCK_SET_KEY);
        }

        if (!values.containsKey(RpcConfig.MOCK_KEY)){
            throw new IllegalStateException("invalid uri for without mock key, uri " + uri);
        }

        String mock = values.remove(RpcConfig.MOCK_KEY);
        String key = genKey(values);
        // mock 为 true，表明要屏蔽服务
        if (RpcConfig.TRUE.equalsIgnoreCase(mock))
            set.add(key);
        // mock 为 false，表明要恢复服务
        else if (RpcConfig.FALSE.equalsIgnoreCase(mock))
            set.remove(key);
    }

    private String genKey(Map<String, String> kvs){
        Map<String, String> map = new HashMap<>();
        // map 中只包含 methodName 这个键值对
        map.put(RpcConfig.METHOD_KEY, kvs.get(RpcConfig.METHOD_KEY));
        URL u = new URL(kvs.get(RpcConfig.PROTOCOL_KEY), kvs.get(RpcConfig.IP_ADDRESS), Integer.parseInt(kvs.get(RpcConfig.PORT)),
                kvs.get(RpcConfig.INTERFACE_KEY), map);
        return u.toString();
    }

}

