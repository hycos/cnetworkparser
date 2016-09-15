package org.snt.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.Operand;
import org.snt.cnetwork.core.OperandKind;

import java.util.Map;

public class Ldapi extends ThreatModel {

    final static Logger logger = LoggerFactory.getLogger(Ldapi.class);

    private static String ldapBlacklist = "['\" /\\#\\<\\>,;\\+\\*\\)\\(\0\\|\\&]";

    public Ldapi() {
        super();
        tmodel.put(OperandKind.LDAPI, this);
    }

    @Override
    public ConstraintNetwork delegate(OperandKind type) {
        switch(type) {
            case LDAPI:
                return getLDAPIThreatModel();
        }
        return null;
    }

    @Override
    public Map<OperandKind, ThreatModel> getThreatModels() {
        return this.tmodel;
    }

    private ConstraintNetwork getLDAPIThreatModel() {
        ConstraintNetwork cn = new ConstraintNetwork();
        Node op = new Operand(ldapBlacklist, OperandKind.STRREXP);
        cn.addVertex(op);
        cn.setStartNode(op);
        return cn;
    }

}
