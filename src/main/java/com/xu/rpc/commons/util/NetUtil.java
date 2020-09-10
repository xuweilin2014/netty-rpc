package com.xu.rpc.commons.util;

import com.xu.rpc.commons.Assert;

import java.net.InetSocketAddress;

public class NetUtil {

    public static String toAddressString(InetSocketAddress address){
        Assert.notNull(address, "address == null.");
        return address.getAddress().getHostAddress() + ":" + address.getPort();
    }

}
