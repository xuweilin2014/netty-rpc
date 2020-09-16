package com.xu.rpc.core;

import com.xu.rpc.core.extension.ExtensionLoader;
import com.xu.rpc.protocol.AbstractProtocol;
import com.xu.rpc.protocol.Exporter;
import com.xu.rpc.protocol.Invoker;
import com.xu.rpc.protocol.Protocol;
import com.xu.rpc.remoting.server.NettyServer;
import com.xu.rpc.commons.util.ReflectionUtils;
import com.xu.rpc.spring.config.AbstractConfig;
import org.apache.log4j.Logger;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class AbilityDetailProvider implements AbilityDetail {

    private static final Logger logger = Logger.getLogger(AbilityDetailProvider.class);

    private final static String STYLE = "<style type=\"text/css\">\n" +
            "table.gridtable {\n" +
            "    font-family: verdana,arial,sans-serif;\n" +
            "    font-size:11px;\n" +
            "    color:#333333;\n" +
            "    border-width: 1px;\n" +
            "    border-color: #666666;\n" +
            "    border-collapse: collapse;\n" +
            "}\n" +
            "table.gridtable th {\n" +
            "    border-width: 1px;\n" +
            "    padding: 8px;\n" +
            "    border-style: solid;\n" +
            "    border-color: #666666;\n" +
            "    background-color: #dedede;\n" +
            "}\n" +
            "table.gridtable td {\n" +
            "    border-width: 1px;\n" +
            "    padding: 8px;\n" +
            "    border-style: solid;\n" +
            "    border-color: #666666;\n" +
            "    background-color: #ffffff;\n" +
            "}\n" +
            "</style>";

    private final static String HEADER = "<table class=\"gridtable\">\n" +
            "<tr>\n" +
            "    <th>NettyRPC Ability Detail</th>\n" +
            "</tr>";

    private final static String TAIL = "</table>";

    private final static String CELL_BEGIN = "<tr><td>";

    private final static String CELL_END = "</td></tr>";

    private static final String CEIL_BEGIN_WITH_STYLE = "<tr><td style=\"background-color: #FFE4C4\">";

    @Override
    public StringBuilder listAbilityDetail(boolean html) {
        List<Protocol> exts = ExtensionLoader.getExtensionLoader(Protocol.class).getExtensions();

        ReflectionUtils utils = new ReflectionUtils();

        if (html) {
            utils.getProvider().append(STYLE).append(HEADER);
        }

        for (Protocol ext : exts) {
            if (ext instanceof AbstractProtocol){
                AbstractProtocol protocol = (AbstractProtocol) ext;
                Map<String, Exporter<?>> exporters = protocol.getExporters();

                if (exporters != null && exporters.size() > 0){
                    // 显示使用哪种类型的协议导出服务
                    utils.getProvider().append(CEIL_BEGIN_WITH_STYLE);
                    utils.getProvider().append("export to " + protocol.getName() + " service");
                    for (Map.Entry<String, Exporter<?>> entry : protocol.getExporters().entrySet()) {
                        Exporter<?> exporter = entry.getValue();
                        if (exporter != null){
                            // 显示服务接口中的所有方法
                            if (html) {
                                utils.getProvider().append(CELL_BEGIN);
                                utils.listRpcProviderDetail(exporter.getInvoker().getInterface(), html);
                                utils.getProvider().append(CELL_END);
                            } else {
                                utils.listRpcProviderDetail(exporter.getInvoker().getInterface(), html);
                            }
                        }
                    }
                    utils.getProvider().append(CELL_END);
                }
            }
        }

        if (html) {
            utils.getProvider().append(TAIL);
        }

        return utils.getProvider();
    }
}

