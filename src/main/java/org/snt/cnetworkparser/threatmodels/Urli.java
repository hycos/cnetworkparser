package org.snt.cnetworkparser.threatmodels;

import dk.brics.automaton.RegExp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.Operand;
import org.snt.cnetwork.core.OperandKind;
import org.snt.cnetwork.utils.AutomatonUtils;

import java.util.Map;

public class Urli extends ThreatModel {

    final static Logger LOGGER = LoggerFactory.getLogger(Urli.class);

    private static String urlBlacklist = "[a-zA-Z0-9]+=[a-zA-Z0-9]+(&[a-zA-Z0-9]+=[a-zA-Z0-9])*";

    public Urli() {
        super();
        tmodel.put(OperandKind.URLI, this);
    }

    @Override
    public ConstraintNetwork delegate(OperandKind type) {
        switch(type) {
            case URLI:
                return getURLiThreatModel();
        }
        return null;
    }

    @Override
    public Map<OperandKind, ThreatModel> getThreatModels() {
        return this.tmodel;
    }

    private ConstraintNetwork getURLiThreatModel() {
        ConstraintNetwork cn = new ConstraintNetwork();
        Node op = new Operand(urlBlacklist, OperandKind.STRREXP);
        cn.addVertex(op);
        cn.setStartNode(op);
        return cn;
    }

}
