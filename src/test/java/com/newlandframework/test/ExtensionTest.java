package com.newlandframework.test;

import com.newlandframework.rpc.core.extension.ExtensionLoader;
import com.newlandframework.rpc.filter.ChainFilter;
import java.io.IOException;


public class ExtensionTest {

    public static void main(String[] args) throws ClassNotFoundException, IOException {
        ChainFilter ext = ExtensionLoader.getExtensionLoader(ChainFilter.class).getExtension("echo");
        System.out.println(ext);
        ext = ExtensionLoader.getExtensionLoader(ChainFilter.class).getExtension("echot");
        System.out.println(ext);
        ext = ExtensionLoader.getExtensionLoader(ChainFilter.class).getExtension("classloader");
        System.out.println(ext);
    }

}
