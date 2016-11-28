package org.snt.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;

import java.util.Map;

public class Sqli extends ThreatModel {


    final static Logger LOGGER = LoggerFactory.getLogger(Sqli.class);

    final static String cdomain = "[\\x00-\\x1F\\x80-\\x9F]";
    //final static String cdomain = "[a-z]";

    public Sqli() {
        super();
        tmodel.put(NodeKind.SQLINUM, this);
        tmodel.put(NodeKind.SQLISTR, this);
    }

    @Override
    public ConstraintNetwork delegate(NodeKind type) {
        switch(type) {
            case SQLINUM :
                return getNumTautology();
            case SQLISTR :
                return getStrTautology();
        }
        return null;
    }

    @Override
    public Map<NodeKind, ThreatModel> getThreatModels() {
        return this.tmodel;
    }


    private ConstraintNetwork getStrTautology() {
        ConstraintNetwork cn = new ConstraintNetwork();

        Node or = new Operand(".*' +[Oo][Rr] +'", NodeKind.STRREXP);

        Node v1 = new Operand("sv1", NodeKind.STRVAR);

        Node orv1 = cn.addOperation(NodeKind.CONCAT, or, v1);

        Node eq = new Operand("'.*=.*'", NodeKind.STRREXP);

        Node v2 = new Operand("sv2", NodeKind.STRVAR);

        Node orv1comp = cn.addOperation(NodeKind.CONCAT, eq, v2);

        Node orv1compv2 = cn.addOperation(NodeKind.CONCAT, orv1, orv1comp);

        cn.addConstraint(NodeKind.STR_EQUALS,v1,v2);

        String scomment = "' *(\\-\\-|#)";
        Node comment = new Operand(scomment, NodeKind.STRREXP);

        Node start = cn.addOperation(NodeKind.CONCAT,orv1compv2,comment);

        cn.setStartNode(start);

        return cn;
    }

    private ConstraintNetwork getNumTautology() {

        ConstraintNetwork cn = new ConstraintNetwork();

        String sor = "[0-9]+ +[Oo][Rr] +";
        Node or = new Operand(sor, NodeKind.STRREXP);

        Node v1 = new Operand("sv7", NodeKind.NUMVAR);

        Node toStrV1 = cn.addOperation(NodeKind.TOSTR, v1);

        Node orv1 = cn.addOperation(NodeKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" +\\>= +", NodeKind.STRREXP);

        Node orv1comp = cn.addOperation(NodeKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv8", NodeKind.NUMVAR);

        Node toStrV2 = cn.addOperation(NodeKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(NodeKind.CONCAT, orv1comp, toStrV2);

        //String scomment = "(\\-\\-|#)";
        //Node comment = new Operand(scomment, NodeKind.STRREXP);

        //cn.addOperation(NodeKind.CONCAT,orv1compv2,comment);

        cn.addConstraint(NodeKind.GREATEREQ, v1,v2);

        cn.setStartNode(orv1compv2);
        return cn;
    }






}
