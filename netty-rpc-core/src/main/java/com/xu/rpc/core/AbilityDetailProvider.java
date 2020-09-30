package com.xu.rpc.core;

import com.xu.rpc.commons.URL;
import com.xu.rpc.commons.util.ReflectionUtils;
import com.xu.rpc.core.extension.ExtensionLoader;
import com.xu.rpc.protocol.AbstractProtocol;
import com.xu.rpc.protocol.Exporter;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.protocol.ProtocolFilterWrapper;
import com.xu.rpc.protocol.*;
import org.apache.log4j.Logger;

import java.lang.reflect.Method;
import java.util.*;

public class AbilityDetailProvider implements AbilityDetail {

    private static final Logger logger = Logger.getLogger(AbilityDetailProvider.class);

    private static final String FORBID_BUTTON = "<input type=\"button\" value=\"屏蔽\" onclick=\"forbid('%d','%s','%s','%s')\"/>";

    private static final String RECOVER_BUTTON = "<input type=\"button\" value=\"恢复\" onclick=\"recover('%d','%s','%s','%s')\"/>";

    private static final String HTML_BEGIN = "<!DOCTYPE html>"+
            "<html lang=\"en\">"+
            "<head>"+
            "    <meta charset=\"UTF-8\">"+
            "    <title>netty-rpc服务能力展示页面</title>"+
            "    <script type=\"application/javascript\">"+
            "        function forbid(id, interfaceName, methodName, url) {"+
            "            var choose = confirm(\"确定要屏蔽服务 \" + interfaceName + \"#\" + methodName + \" 的调用吗?\");"+
            "            if(choose === true) {"+
            "                var httpRequest = new XMLHttpRequest();"+
            "                httpRequest.open(\'GET\', url, true);"+
            "                httpRequest.send();"+
            "                alert(\"服务 \" + interfaceName + \"#\" + methodName + \" 已被屏蔽\");"+
            "                document.getElementById(id).innerText = \"屏蔽\";"+
            "            }"+
            "        }"+
            "        function recover(id, interfaceName, methodName, url) {"+
            "            var choose = confirm(\"确定要恢复服务 \" + interfaceName + \"#\" + methodName + \" 的调用吗?\");"+
            "            if(choose === true) {"+
            "                var httpRequest = new XMLHttpRequest();"+
            "                httpRequest.open(\'GET\', url, true);"+
            "                httpRequest.send();"+
            "                alert(\"服务 \" + interfaceName + \"#\" + methodName + \" 已恢复\");"+
            "                document.getElementById(id).innerText = \"正常\";"+
            "            }"+
            "        }"+
            "    </script>"+
            "    <style type=\"text/css\">"+
            "        table.gridtable {"+
            "            font-family: verdana,arial,sans-serif;"+
            "            font-size:11px;"+
            "            color:#333333;"+
            "            border-width: 1px;"+
            "            border-color: #666666;"+
            "            border-collapse: collapse;"+
            "        }"+
            "        table.gridtable th {"+
            "            border-width: 1px;"+
            "            padding: 8px;"+
            "            border-style: solid;"+
            "            border-color: #666666;"+
            "            background-color: #dedede;"+
            "        }"+
            "        table.gridtable td {"+
            "            border-width: 1px;"+
            "            padding: 8px;"+
            "            border-style: solid;"+
            "            border-color: #666666;"+
            "            background-color: #ffffff;"+
            "        }"+
            "    </style>"+
            "</head>"+
            "<body>"+
            "    <table border=\"1\" class=\"gridtable\" align=\"center\">"+
            "        <tr>"+
            "            <th colspan=\"6\">netty-rpc服务能力展示</th>"+
            "        </tr>"+
            "        <tr>"+
            "            <th>协议</th>"+
            "            <th>服务</th>"+
            "            <th>方法</th>"+
            "            <th>状态</th>"+
            "            <th>屏蔽</th>"+
            "            <th>恢复</th>"+
            "        </tr>";

    public static final String HTML_END = "    </table>"+
            "</body>"+
            "</html>";

    private final static String TR_BEGIN = "<tr>\n";

    private final static String TR_END = "</tr>";

    private static final String TD_BEGIN = "<td>\n";

    private static final String TD_END = "</td>";

    private static final String STATUS_LABEL = "<label id='%d'>正常</label>";

    @Override
    public StringBuilder listAbilityDetail(String host, int port) {
        List<Protocol> exts = ExtensionLoader.getExtensionLoader(Protocol.class).getExtensions();

        ReflectionUtils utils = new ReflectionUtils();

        utils.getProvider().append(HTML_BEGIN);
        int counter = 1;

        for (Protocol ext : exts) {
            if (ext instanceof ProtocolFilterWrapper){
                AbstractProtocol protocol = (AbstractProtocol) ((ProtocolFilterWrapper) ext).getProtocol();
                Map<String, Exporter<?>> exporters = protocol.getExporters();

                if (exporters != null && exporters.size() > 0){
                    for (Map.Entry<String, Exporter<?>> entry : protocol.getExporters().entrySet()) {
                        Exporter<?> exporter = entry.getValue();
                        if (exporter != null && exporter.getInvoker() != null){
                            URL url = exporter.getInvoker().getUrl();
                            String interfaceName = exporter.getInvoker().getInterface().getName();
                            for (Method method : exporter.getInvoker().getInterface().getDeclaredMethods()) {
                                String methodName = utils.getMethodSignature(method);
                                // 显示导出服务的协议
                                utils.getProvider().append(TR_BEGIN);
                                utils.getProvider().append(TD_BEGIN);
                                utils.getProvider().append(url.getProtocol()).append("://").append(url.getAddress());
                                utils.getProvider().append(TD_END);

                                // 显示服务接口
                                utils.getProvider().append(TD_BEGIN);
                                utils.getProvider().append(interfaceName);
                                utils.getProvider().append(TD_END);

                                // 显示服务接口中的方法
                                utils.getProvider().append(TD_BEGIN);
                                utils.getProvider().append(methodName);
                                utils.getProvider().append(TD_END);

                                // 显示服务的状态：屏蔽/恢复
                                utils.getProvider().append(TD_BEGIN);
                                utils.getProvider().append(String.format(STATUS_LABEL, counter));
                                utils.getProvider().append(TD_END);

                                StringBuilder recoverButton = new StringBuilder();
                                StringBuilder forbidButton = new StringBuilder();

                                // 生成两个按钮，屏蔽按钮和恢复按钮
                                if (!RpcConfig.INJVM_PROTOCOL.equalsIgnoreCase(protocol.getName())){
                                    String duplicateU = "http://" + host + RpcConfig.ADDRESS_DELIMITER + port + "/" + RpcConfig.OVERRIDE_KEY + "?" + RpcConfig.INTERFACE_KEY + "=" +
                                            interfaceName + "&" + RpcConfig.METHOD_KEY + "=" + methodName + "&" + RpcConfig.PROTOCOL_KEY + "=" + url.getProtocol() + "&"
                                            + RpcConfig.IP_ADDRESS + "=" + url.getHost() + "&" + RpcConfig.PORT + "=" + url.getPort();
                                    // 点击屏蔽按钮，将会发送屏蔽服务的请求给 EchoServer，具体就是将服务加入到 MockChainFilter 的黑名单中，进行拦截
                                    String u = duplicateU + "&" + RpcConfig.MOCK_KEY + "=" + RpcConfig.TRUE;
                                    forbidButton.append(String.format(FORBID_BUTTON, counter, interfaceName, methodName, u));
                                    // 点击恢复按钮，将会发送恢复服务的请求给 EchoServer，将服务从 MockChainFilter 的黑名单中移除
                                    u = duplicateU + "&" + RpcConfig.MOCK_KEY + "=" + RpcConfig.FALSE;
                                    recoverButton.append(String.format(RECOVER_BUTTON, counter++, interfaceName, methodName, u));
                                }

                                utils.getProvider().append(TD_BEGIN);
                                utils.getProvider().append(forbidButton);
                                utils.getProvider().append(TD_END);

                                utils.getProvider().append(TD_BEGIN);
                                utils.getProvider().append(recoverButton);
                                utils.getProvider().append(TD_END);
                                utils.getProvider().append(TR_END);
                            }
                        }
                    }
                }
            }
        }

        utils.getProvider().append(HTML_END);

        return utils.getProvider();
    }
}

