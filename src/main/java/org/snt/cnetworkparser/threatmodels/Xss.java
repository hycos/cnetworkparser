package org.snt.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.graph.Operand;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;

import java.util.Map;

public class Xss extends ThreatModel {

    private static String script = "\\< *[Ss][Cc][Rr][Ii][Pp][Tt] *\\>" +
            "[a-zA-Z0-9\\(\\);]+\\</ *[Ss][Cc][Rr][Ii][Pp][Tt] \\>";

    private static String img = "\\< *[Ii][Mm][Gg] [Ss][Rr][Cc]=[Jj][Aa][Vv][Aa][Ss][Cc]" +
            "[Rr][Ii][Pp][Tt]:[a-zA-Z0-9\\(\\);]+ *\\>";

    private static String xss = "(" + script + "|" + img + ")";


    final static Logger LOGGER = LoggerFactory.getLogger(Xss.class);

    public Xss() {
        super();
        tmodel.put(NodeKind.XSS, this);
    }


    @Override
    public ConstraintNetworkBuilder delegate(NodeKind type) {
        switch(type) {
            case XSS:
                try {
                    return getXMLIThreatModel();
                } catch (EUFInconsistencyException e) {
                    assert false;
                }
        }
        return null;
    }

    @Override
    public Map<NodeKind, ThreatModel> getThreatModels() {
        return this.tmodel;
    }


    private ConstraintNetworkBuilder getXMLIThreatModel() throws EUFInconsistencyException {

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        Node op = new Operand(xss, NodeKind.STRREXP);
        cn.addOperand(NodeKind.STRREXP, xss);
        cn.setStartNode(op);
        return cn;
    }

}
