package com.newlandframework.rpc.util;

import com.newlandframework.rpc.core.RpcSystemConfig;
import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

public final class URL {

    public static final Pattern IP_PATTER = Pattern.compile("\\d{1,3}(\\.\\d{1,3}){3,5}$");

    private String protocol;

    private String username;

    private String password;

    private String host;

    private int port;

    private String path;

    private Map<String, String> parameters;

    public URL() {
        protocol = null;
        username = null;
        password = null;
        host = null;
        port = 0;
        path = null;
        parameters = null;
    }


    public URL(String name, String host, int port, String path, Map<String, String> parameters) {
        this(name, null, null, host, port, path, parameters);
    }

    public URL(String name, String username, String password, String host, int port, String path, Map<String, String> parameters) {
        this.protocol = name;
        this.username = username;
        this.password = password;
        this.host = host;
        this.port = port;
        this.path = path;

        if (parameters == null)
            this.parameters = new HashMap<>();
        else
            this.parameters = parameters;
    }

    public String getParameter(String key){
        if (key == null || key.length() == 0)
            throw new IllegalArgumentException("missing key, cannot get parameter value in url");

        if (parameters == null)
            return null;
        else
            return parameters.get(key);
    }

    public String getAddress(){
        return host + ":" + port;
    }

    public String getServiceName() {
        return getParameter(RpcSystemConfig.INTERFACE_KEY, path);
    }
    
    public String getParameter(String key, String defaultValue){
        // TODO: 2020/8/10  
        return "";
    }

    public int getParameter(String key, int defaultValue){
        // TODO: 2020/8/16
        return 0;
    }

    public boolean getParameter(String key, boolean defaultValue){
        // TODO: 2020/8/16
        return false;
    }

    public String toFullString(){
        return "";
    }

    public URL addParameterAndEncoded(String key, String value){
        if (StringUtils.isEmpty(key)){
            throw new IllegalArgumentException("key cannot be null when adding parameter to url.");
        }
        if (StringUtils.isEmpty(value)){
            throw new IllegalArgumentException("value cannot be null when adding parameter to url.");
        }
        // TODO: 2020/8/9 实现 encode 方法 
        return addParameter(key, value);
    }

    public String getParameterAndDecoded(String key){
        // TODO: 2020/8/10  
        return "";
    }

    public static URL valueOf(String url){
        // TODO: 2020/8/10
        return new URL();
    }

    private URL addParameter(String key, String value) {
        if (StringUtils.isEmpty(key) || StringUtils.isEmpty(value))
            return this;

        if (value.equals(parameters.get(key)))
            return this;

        parameters.put(key, value);
        return this;
    }

    public static Pattern getIpPatter() {
        return IP_PATTER;
    }

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public Map<String, String> getParameters() {
        return parameters;
    }

    public void setParameters(Map<String, String> parameters) {
        this.parameters = parameters;
    }

}
