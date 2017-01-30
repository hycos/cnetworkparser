package org.snt.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.exception.EUFInconsistencyException;

import java.util.Map;

public class Ldapi extends ThreatModel {

    final static Logger LOGGER = LoggerFactory.getLogger(Ldapi.class);

    private static String ldapBlacklist = "['\" /\\#\\<\\>,;\\+\\*\\)\\(\0\\|\\&]";

    public Ldapi() {
        super();
        tmodel.put(NodeKind.LDAPI, this);
    }

    @Override
    public ConstraintNetworkBuilder delegate(NodeKind type) {
        switch(type) {
            case LDAPI:
                try {
                    return getLDAPIThreatModel();
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

    private ConstraintNetworkBuilder getLDAPIThreatModel() throws EUFInconsistencyException {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder(false);
        Node op = new Operand(ldapBlacklist, NodeKind.STRREXP);
        cn.addNode(op);
        cn.setStartNode(op);
        return cn;
    }

}
