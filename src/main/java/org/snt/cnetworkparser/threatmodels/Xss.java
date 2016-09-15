package org.snt.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.Operand;
import org.snt.cnetwork.core.OperandKind;
import java.util.Map;

public class Xss extends ThreatModel {

    private static String script = "\\< *[Ss][Cc][Rr][Ii][Pp][Tt] *\\>" +
            "[a-zA-Z0-9\\(\\);]+\\</ *[Ss][Cc][Rr][Ii][Pp][Tt] \\>";

    private static String img = "\\< *[Ii][Mm][Gg] [Ss][Rr][Cc]=[Jj][Aa][Vv][Aa][Ss][Cc]" +
            "[Rr][Ii][Pp][Tt]:[a-zA-Z0-9\\(\\);]+ *\\>";

    private static String xss = "(" + script + "|" + img + ")";


    final static Logger logger = LoggerFactory.getLogger(Xss.class);

    public Xss() {
        super();
        tmodel.put(OperandKind.XSS, this);
    }


    @Override
    public ConstraintNetwork delegate(OperandKind type) {
        switch(type) {
            case XSS:
                return getXMLIThreatModel();
        }
        return null;
    }

    @Override
    public Map<OperandKind, ThreatModel> getThreatModels() {
        return this.tmodel;
    }


    private ConstraintNetwork getXMLIThreatModel() {

        ConstraintNetwork cn = new ConstraintNetwork();
        Node op = new Operand(xss, OperandKind.STRREXP);
        cn.addVertex(op);
        cn.setStartNode(op);
        return cn;
    }

}
