package com.xu.rpc.util;

import java.net.InetSocketAddress;

public class NetUtil {

    public static String toAddressString(InetSocketAddress address){
        Assert.notNull(address, "address == null.");
        return address.getAddress().getHostAddress() + ":" + address.getPort();
    }

}
