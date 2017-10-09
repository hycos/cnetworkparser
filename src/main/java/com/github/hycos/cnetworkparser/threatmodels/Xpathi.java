package com.github.hycos.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.graph.Operand;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;

import java.util.Map;

public class Xpathi extends ThreatModel {


    final static Logger LOGGER = LoggerFactory.getLogger(Xpathi.class);

    public Xpathi() {
        super();
        tmodel.put(NodeKind.XPATHNUM, this);
        tmodel.put(NodeKind.XPATHSTR, this);
    }

    @Override
    public ConstraintNetworkBuilder delegate(NodeKind type) {
        switch (type) {
            case XPATHNUM:
                try {
                    return getNumTautology();
                } catch (EUFInconsistencyException e) {
                    assert false;
                }
            case XPATHSTR:
                try {
                    return getStrTautology();
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


    private ConstraintNetworkBuilder getStrTautology() throws EUFInconsistencyException {

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, NodeKind.STRREXP);

        String item = "sv1";
        Node v1 = new Operand(item, NodeKind.STRVAR);

        Node orv1 = cn.addOperation(NodeKind.CONCAT, or, v1);

        String seq = "'.*=.*'";
        Node eq = new Operand(seq, NodeKind.STRREXP);

        Node v2 = new Operand("sv2", NodeKind.STRVAR);

        Node orv1comp = cn.addOperation(NodeKind.CONCAT, eq, v2);

        Node orv1compv2 = cn.addOperation(NodeKind.CONCAT, orv1, orv1comp);

        cn.addConstraint(NodeKind.STR_EQUALS, v1, v2);

        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, NodeKind.STRREXP);

        cn.addOperation(NodeKind.CONCAT, orv1compv2, comment);

        cn.setStartNode(orv1compv2);

        return cn;
    }

    private ConstraintNetworkBuilder getNumTautology() throws EUFInconsistencyException {

        /**ConstraintNetwork cn = new ConstraintNetwork();

         Node n0 = getEqNumTautology(cn);
         Node n1 = getGtNumTautology(cn);
         Node n2 = getStNumTautology(cn);

         Node tm = new Operand("tm", NodeKind.STRVAR);

         Node m1 = cn.addOperation(NodeKind.MATCHES, tm, n0);
         Node m2 = cn.addOperation(NodeKind.MATCHES, tm, n1);
         Node m3 = cn.addOperation(NodeKind.MATCHES, tm, n2);

         Node or1 = cn.addOperation(NodeKind.OR, m1, m2);

         Node or2 = cn.addConstraint(NodeKind.OR, m3, or1);

         cn.setStartNode(tm);**/


        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        Node n = null;

        n = getGeqNumTautology(cn);

        cn.setStartNode(n);

        return cn;

    }

    private Node getEqNumTautology(ConstraintNetworkBuilder cn) throws EUFInconsistencyException {

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, NodeKind.STRREXP);

        Node v1 = new Operand("sv1", NodeKind.NUMVAR);

        Node toStrV1 = cn.addOperation(NodeKind.TOSTR, v1);

        Node orv1 = cn.addOperation(NodeKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" *= *", NodeKind.STRREXP);

        Node orv1comp = cn.addOperation(NodeKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv2", NodeKind.NUMVAR);

        Node toStrV2 = cn.addOperation(NodeKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(NodeKind.CONCAT, orv1comp, toStrV2);

        cn.addOperation(NodeKind.EQUALS, v1, v2);

        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, NodeKind.STRREXP);

        cn.addOperation(NodeKind.CONCAT, orv1compv2, comment);

        return orv1compv2;
    }

    private Node getGtNumTautology(ConstraintNetworkBuilder cn) throws EUFInconsistencyException {

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, NodeKind.STRREXP);

        Node v1 = new Operand("sv3", NodeKind.NUMVAR);

        Node toStrV1 = cn.addOperation(NodeKind.TOSTR, v1);

        Node orv1 = cn.addOperation(NodeKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" *\\> *", NodeKind.STRREXP);

        Node orv1comp = cn.addOperation(NodeKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv4", NodeKind.NUMVAR);

        Node toStrV2 = cn.addOperation(NodeKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(NodeKind.CONCAT, orv1comp, toStrV2);

        cn.addOperation(NodeKind.GREATER, v1, v2);

        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, NodeKind.STRREXP);

        cn.addOperation(NodeKind.CONCAT, orv1compv2, comment);

        return orv1compv2;
    }

    private Node getStNumTautology(ConstraintNetworkBuilder cn) throws EUFInconsistencyException {

        String sor = ".*' +[Oo][Rr] +' +";
        Node or = new Operand(sor, NodeKind.STRREXP);

        Node v1 = new Operand("sv5", NodeKind.NUMVAR);

        Node toStrV1 = cn.addOperation(NodeKind.TOSTR, v1);

        Node orv1 = cn.addOperation(NodeKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" *\\> *", NodeKind.STRREXP);

        Node orv1comp = cn.addOperation(NodeKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv6", NodeKind.NUMVAR);

        Node toStrV2 = cn.addOperation(NodeKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(NodeKind.CONCAT, orv1comp, toStrV2);

        cn.addOperation(NodeKind.SMALLER, v1, v2);

        return orv1compv2;
    }

    private Node getGeqNumTautology(ConstraintNetworkBuilder cn) throws EUFInconsistencyException {

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, NodeKind.STRREXP);

        Node v1 = new Operand("sv7", NodeKind.NUMVAR);

        Node toStrV1 = cn.addOperation(NodeKind.TOSTR, v1);

        Node orv1 = cn.addOperation(NodeKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" +\\>= +", NodeKind.STRREXP);

        Node orv1comp = cn.addOperation(NodeKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv8", NodeKind.NUMVAR);

        Node toStrV2 = cn.addOperation(NodeKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(NodeKind.CONCAT, orv1comp, toStrV2);

        cn.addConstraint(NodeKind.GREATEREQ, v1, v2);

        return orv1compv2;
    }

}
