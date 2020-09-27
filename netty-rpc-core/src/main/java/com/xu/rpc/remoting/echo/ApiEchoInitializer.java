package com.xu.rpc.remoting.echo;


/*public class ApiEchoInitializer extends ChannelInitializer<SocketChannel> {

*//*    private final SslContext sslCtx;

    private final URL url;

    public ApiEchoInitializer(SslContext sslCtx, URL url, String host, int port) {
        this.sslCtx = sslCtx;
        this.url = url;
    }

    @Override
    public void initChannel(SocketChannel ch) {
        ChannelPipeline p = ch.pipeline();
        if (sslCtx != null) {
            p.addLast(sslCtx.newHandler(ch.alloc()));
        }
        p.addLast(new HttpServerCodec());
        p.addLast(new ApiEchoHandler());
    }*//*
}*/

