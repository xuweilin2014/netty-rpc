package com.xu.rpc.commons;

import com.sun.org.apache.bcel.internal.generic.IF_ACMPEQ;
import com.xu.rpc.core.RpcConfig;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

@Getter
@Setter
public final class URL {

    public static final Pattern IP_PATTER = Pattern.compile("\\d{1,3}(\\.\\d{1,3}){3,5}$");

    private String protocol;

    private String host;

    private int port;

    private String path;

    private Map<String, String> parameters;

    public URL() {
        protocol = null;
        host = null;
        port = 0;
        path = null;
        parameters = null;
    }


    public URL(String name, String host, int port, Map<String, String> parameters){
        this(name, host, port, null, parameters);
    }

    public URL(String name, String host, int port, String path, Map<String, String> parameters) {
        this.protocol = name;
        this.host = host;
        this.port = Math.max(port, 0);
        this.path = path;

        if (parameters == null)
            this.parameters = new HashMap<>();
        else
            this.parameters = Collections.unmodifiableMap(parameters);
    }

    public String getParameter(String key){
        if (StringUtils.isEmpty(key))
            throw new IllegalArgumentException("missing key, cannot get parameter value in url");

        if (parameters == null)
            return null;
        else
            return parameters.get(key);
    }

    public String getParameter(String key, String defaultValue){
        String value = getParameter(key);
        if (value == null || value.length() > 0)
            return defaultValue;

        return value;
    }
    
    public URL removeParameter(String key){
        if (StringUtils.isEmpty(key) || parameters == null
                || !parameters.containsKey(key))
           return this;

        Map<String, String> map = new HashMap<>(parameters);
        map.remove(key);
        return new URL(protocol, host, port, map);
    }

    public URL setProtocol(String protocol){
        return new URL(protocol, host, port, path, parameters);
    }

    public String getAddress(){
        return host + RpcConfig.ADDRESS_DELIMITER + port;
    }

    public String getServiceName() {
        return getParameter(RpcConfig.INTERFACE_KEY, path);
    }

    public int getParameter(String key, int defaultValue){
        String value = getParameter(key);
        if (value == null || value.length() == 0)
            return defaultValue;

        return Integer.parseInt(value);
    }

    public boolean getParameter(String key, boolean defaultValue){
        String value = getParameter(key);
        if (value == null || value.length() == 0)
            return defaultValue;

        return Boolean.parseBoolean(value);
    }

    // 将 url 变为字符串，格式为：protocol://host:port/ServiceName?key1=value1&key2=value2....
    public String toFullString(){
        StringBuilder buf = new StringBuilder();
        if (!StringUtils.isEmpty(protocol))
            buf.append(protocol);
        else
            buf.append(RpcConfig.EMPTY_PROTOCOL);
        buf.append("://");

        Assert.notEmpty(host, " host == null.");
        Assert.notNull(port, "port == null.");
        Assert.notEmpty(path, "path == null.");

        buf.append(host);
        if (port > 0)
            buf.append(RpcConfig.ADDRESS_DELIMITER).append(port);
        buf.append("/");
        buf.append(path);


        if (parameters.size() > 0){
            buf.append("?");
            appendParameters(buf, parameters);
        }

        return buf.toString();
    }

    public URL addParameterAndEncoded(String key, String value){
        if (StringUtils.isEmpty(key)){
            throw new IllegalArgumentException("key is empty.");
        }
        if (StringUtils.isEmpty(value)){
            throw new IllegalArgumentException("value is empty.");
        }

        try {
            value = URLEncoder.encode(value, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e.getMessage());
        }

        return addParameter(key, value);
    }

    public String getParameterAndDecoded(String key){
        return getParameterAndDecoded(key, null);
    }

    public String getParameterAndDecoded(String key, String defaultValue){
        String value = getParameter(key);
        if (StringUtils.isEmpty(value))
            value = defaultValue;

        try {
            return URLDecoder.decode(value, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new IllegalStateException(e.getMessage());
        }
    }

    public static URL valueOf(String url){
        String protocol = null;
        String host = null;
        int port = 0;
        String path = null;
        Map<String, String> parameters = new HashMap<>();

        int i = url.indexOf("?");
        String body = i == -1 ? null : url.substring(i + 1);
        String header = i == -1 ? url : url.substring(0, i);

        if ((i = header.indexOf("://")) != -1){
            protocol = header.substring(0, i);
            String rest = header.substring(i + 1);

            if ((i = rest.indexOf("/")) == -1) {
                throw new IllegalStateException("url format is invalid, missing / in url " + url);
            }

            String address = rest.substring(0, i);
            if (address.contains(":")){
                port = Integer.parseInt(address.split(":")[1]);
            }
            host = address.split(":")[0];
            path = rest.substring(i + 1);
        }else{
            throw new IllegalStateException("url format is invalid, missing :// in url " + url);
        }

        if (body != null){
            while ((i = body.indexOf("&")) != -1) {
                String kv = body.substring(0, i);
                body = body.substring(i + 1);
                if (kv.contains("=")){
                    int index = kv.indexOf("=");
                    parameters.put(kv.substring(0, index), kv.substring(index + 1));
                }else{
                    throw new IllegalStateException("url format is invalid, missing = in url " + url);
                }
            }
        }

        return new URL(protocol, host, port, parameters);
    }

    public URL addParameter(String key, String value) {
        if (StringUtils.isEmpty(key) || StringUtils.isEmpty(value))
            return this;

        if (value.equals(parameters.get(key)))
            return this;

        Map<String, String> map = new HashMap<>(parameters);
        map.put(key, value);

        return new URL(protocol, host, port, path, map);
    }

    public static String toQueryString(Map<String, String> map){
        StringBuilder buf = new StringBuilder();
        appendParameters(buf, map);

        return buf.toString();
    }

    private static void appendParameters(StringBuilder buf, Map<String, String> parameters){
        if (parameters != null && parameters.size() > 0){
            int counter = 0;
            for (Map.Entry<String, String> entry : parameters.entrySet()) {
                if (counter > 0)
                    buf.append("&");
                buf.append(entry.getKey());
                buf.append("=");
                buf.append(entry.getValue());
                counter++;
            }
        }
    }

    public static Pattern getIpPatter() {
        return IP_PATTER;
    }

}
