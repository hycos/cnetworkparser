package org.snt.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.graph.Operand;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;

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
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        Node op = new Operand(ldapBlacklist, NodeKind.STRREXP);
        cn.addOperand(NodeKind.STRREXP, ldapBlacklist);
        cn.setStartNode(op);
        return cn;
    }

}
