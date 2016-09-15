package org.snt.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;

import java.util.Map;

public class Sqli extends ThreatModel {


    final static Logger logger = LoggerFactory.getLogger(Sqli.class);

    final static String cdomain = "[\\x00-\\x1F\\x80-\\x9F]";
    //final static String cdomain = "[a-z]";

    public Sqli() {
        super();
        tmodel.put(OperandKind.SQLINUM, this);
        tmodel.put(OperandKind.SQLISTR, this);
    }

    @Override
    public ConstraintNetwork delegate(OperandKind type) {
        switch(type) {
            case SQLINUM :
                return getNumTautology();
            case SQLISTR :
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

        Node or = new Operand(".*' +[Oo][Rr] +'", OperandKind.STRREXP);

        Node v1 = new Operand("sv1", OperandKind.STRVAR);

        Node orv1 = cn.addOperation(OperationKind.CONCAT, or, v1);

        Node eq = new Operand("'.*=.*'", OperandKind.STRREXP);

        Node v2 = new Operand("sv2", OperandKind.STRVAR);

        Node orv1comp = cn.addOperation(OperationKind.CONCAT, eq, v2);

        Node orv1compv2 = cn.addOperation(OperationKind.CONCAT, orv1, orv1comp);

        cn.addConstraint(OperationKind.STR_EQUALS,v1,v2);

        String scomment = "' *(\\-\\-|#)";
        Node comment = new Operand(scomment, OperandKind.STRREXP);

        Node start = cn.addOperation(OperationKind.CONCAT,orv1compv2,comment);

        cn.setStartNode(start);

        return cn;
    }

    private ConstraintNetwork getNumTautology() {

        ConstraintNetwork cn = new ConstraintNetwork();

        String sor = "[0-9]+ +[Oo][Rr] +";
        Node or = new Operand(sor, OperandKind.STRREXP);

        Node v1 = new Operand("sv7", OperandKind.NUMVAR);

        Node toStrV1 = cn.addOperation(OperationKind.TOSTR, v1);

        Node orv1 = cn.addOperation(OperationKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" +\\>= +", OperandKind.STRREXP);

        Node orv1comp = cn.addOperation(OperationKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv8", OperandKind.NUMVAR);

        Node toStrV2 = cn.addOperation(OperationKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(OperationKind.CONCAT, orv1comp, toStrV2);

        //String scomment = "(\\-\\-|#)";
        //Node comment = new Operand(scomment, OperandKind.STRREXP);

        //cn.addOperation(OperationKind.CONCAT,orv1compv2,comment);

        cn.addConstraint(OperationKind.GREATEREQ, v1,v2);

        cn.setStartNode(orv1compv2);
        return cn;
    }






}
