package org.snt.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;

import java.util.Map;

public class Xpathi extends ThreatModel {


    final static Logger LOGGER = LoggerFactory.getLogger(Xpathi.class);

    public Xpathi() {
        super();
        tmodel.put(OperandKind.XPATHNUM, this);
        tmodel.put(OperandKind.XPATHSTR, this);
    }

    @Override
    public ConstraintNetwork delegate(OperandKind type) {
        switch(type) {
            case XPATHNUM:
                return getNumTautology();
            case XPATHSTR:
                return getStrTautology();
        }
        return null;
    }

    @Override
    public Map<OperandKind, ThreatModel> getThreatModels() {
        return this.tmodel;
    }


    private ConstraintNetwork getStrTautology() {

        ConstraintNetwork cn = new ConstraintNetwork();

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, OperandKind.STRREXP);

        String item = "sv1";
        Node v1 = new Operand(item, OperandKind.STRVAR);

        Node orv1 = cn.addOperation(OperationKind.CONCAT, or, v1);

        String seq = "'.*=.*'";
        Node eq = new Operand(seq, OperandKind.STRREXP);

        Node v2 = new Operand("sv2", OperandKind.STRVAR);

        Node orv1comp = cn.addOperation(OperationKind.CONCAT, eq, v2);

        Node orv1compv2 = cn.addOperation(OperationKind.CONCAT, orv1, orv1comp);

        cn.addConstraint(OperationKind.STR_EQUALS,v1,v2);

        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, OperandKind.STRREXP);

        cn.addOperation(OperationKind.CONCAT,orv1compv2,comment);

        cn.setStartNode(orv1compv2);

        return cn;
    }

    private ConstraintNetwork getNumTautology() {

        /**ConstraintNetwork cn = new ConstraintNetwork();

        Node n0 = getEqNumTautology(cn);
        Node n1 = getGtNumTautology(cn);
        Node n2 = getStNumTautology(cn);

        Node tm = new Operand("tm", OperandKind.STRVAR);

        Node m1 = cn.addOperation(OperationKind.MATCHES, tm, n0);
        Node m2 = cn.addOperation(OperationKind.MATCHES, tm, n1);
        Node m3 = cn.addOperation(OperationKind.MATCHES, tm, n2);

        Node or1 = cn.addOperation(OperationKind.OR, m1, m2);

        Node or2 = cn.addConstraint(OperationKind.OR, m3, or1);

        cn.setStartNode(tm);**/


        ConstraintNetwork cn = new ConstraintNetwork();
        Node n = getGeqNumTautology(cn);

        cn.setStartNode(n);

        return cn;

    }

    private Node getEqNumTautology(ConstraintNetwork cn) {

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, OperandKind.STRREXP);

        Node v1 = new Operand("sv1", OperandKind.NUMVAR);

        Node toStrV1 = cn.addOperation(OperationKind.TOSTR, v1);

        Node orv1 = cn.addOperation(OperationKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" *= *", OperandKind.STRREXP);

        Node orv1comp = cn.addOperation(OperationKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv2", OperandKind.NUMVAR);

        Node toStrV2 = cn.addOperation(OperationKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(OperationKind.CONCAT, orv1comp, toStrV2);

        cn.addOperation(OperationKind.EQUALS, v1,v2);

        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, OperandKind.STRREXP);

        cn.addOperation(OperationKind.CONCAT,orv1compv2,comment);

        return orv1compv2;
    }

    private Node getGtNumTautology(ConstraintNetwork cn) {

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, OperandKind.STRREXP);

        Node v1 = new Operand("sv3", OperandKind.NUMVAR);

        Node toStrV1 = cn.addOperation(OperationKind.TOSTR, v1);

        Node orv1 = cn.addOperation(OperationKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" *\\> *", OperandKind.STRREXP);

        Node orv1comp = cn.addOperation(OperationKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv4", OperandKind.NUMVAR);

        Node toStrV2 = cn.addOperation(OperationKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(OperationKind.CONCAT, orv1comp, toStrV2);

        cn.addOperation(OperationKind.GREATER, v1,v2);

        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, OperandKind.STRREXP);

        cn.addOperation(OperationKind.CONCAT,orv1compv2,comment);

        return orv1compv2;
    }

    private Node getStNumTautology(ConstraintNetwork cn) {

        String sor = ".*' +[Oo][Rr] +' +";
        Node or = new Operand(sor, OperandKind.STRREXP);

        Node v1 = new Operand("sv5", OperandKind.NUMVAR);

        Node toStrV1 = cn.addOperation(OperationKind.TOSTR, v1);

        Node orv1 = cn.addOperation(OperationKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" *\\> *", OperandKind.STRREXP);

        Node orv1comp = cn.addOperation(OperationKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv6", OperandKind.NUMVAR);

        Node toStrV2 = cn.addOperation(OperationKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(OperationKind.CONCAT, orv1comp, toStrV2);

        cn.addOperation(OperationKind.SMALLER, v1,v2);

        return orv1compv2;
    }

    private Node getGeqNumTautology(ConstraintNetwork cn) {

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, OperandKind.STRREXP);

        Node v1 = new Operand("sv7", OperandKind.NUMVAR);

        Node toStrV1 = cn.addOperation(OperationKind.TOSTR, v1);

        Node orv1 = cn.addOperation(OperationKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" +\\>= +", OperandKind.STRREXP);

        Node orv1comp = cn.addOperation(OperationKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv8", OperandKind.NUMVAR);

        Node toStrV2 = cn.addOperation(OperationKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(OperationKind.CONCAT, orv1comp, toStrV2);

        cn.addConstraint(OperationKind.GREATEREQ, v1,v2);

        return orv1compv2;
    }

}
