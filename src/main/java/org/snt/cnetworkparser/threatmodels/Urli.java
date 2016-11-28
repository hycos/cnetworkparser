package org.snt.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;

import java.util.Map;

public class Urli extends ThreatModel {

    final static Logger LOGGER = LoggerFactory.getLogger(Urli.class);

    private static String urlBlacklist = "[a-zA-Z0-9]+=[a-zA-Z0-9]+(&[a-zA-Z0-9]+=[a-zA-Z0-9])*";

    public Urli() {
        super();
        tmodel.put(NodeKind.URLI, this);
    }

    @Override
    public ConstraintNetwork delegate(NodeKind type) {
        switch(type) {
            case URLI:
                return getURLiThreatModel();
        }
        return null;
    }

    @Override
    public Map<NodeKind, ThreatModel> getThreatModels() {
        return this.tmodel;
    }

    private ConstraintNetwork getURLiThreatModel() {
        ConstraintNetwork cn = new ConstraintNetwork();
        Node op = new Operand(urlBlacklist, NodeKind.STRREXP);
        cn.addVertex(op);
        cn.setStartNode(op);
        return cn;
    }

}
