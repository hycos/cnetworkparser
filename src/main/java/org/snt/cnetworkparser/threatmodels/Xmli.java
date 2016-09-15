package org.snt.cnetworkparser.threatmodels;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;

import java.util.Map;

public class Xmli extends ThreatModel {


    private static String xmlInjection = ".*(\\<((! *- *-)?|( *- *-)?\\>)|\\< *CDATA\\[\\[.*\\]\\] *\\>).*";




    final static Logger logger = LoggerFactory.getLogger(Xmli.class);

    public Xmli() {
        super();
        tmodel.put(OperandKind.XMLI, this);
    }


    @Override
    public ConstraintNetwork delegate(OperandKind type) {
        switch(type) {
            case XMLI:
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

        Node strvar = new Operand("sv1", OperandKind.STRVAR);
        //Node op = new Operand(xmlInjection, OperandKind.STRREXP);

        //Node matches1 = cn.addOperation(OperationKind.MATCHES, strvar, op);

        Node content = new Operand("content", OperandKind.STRVAR);


        Node startag = new Operand("stag", OperandKind.STRVAR);
        Node open1 = new Operand("\\<", OperandKind.STRLIT);
        Node close1 = new Operand("\\>", OperandKind.STRLIT);

        Node s1 = cn.addOperation(OperationKind.CONCAT, open1, startag);
        Node s2 = cn.addOperation(OperationKind.CONCAT, s1, close1);

        Node endtag = new Operand("etag", OperandKind.STRVAR);
        Node open2 = new Operand("\\<\\/", OperandKind.STRLIT);
        Node close2 = new Operand("\\>", OperandKind.STRLIT);

        Node e1 = cn.addOperation(OperationKind.CONCAT, open2, endtag);
        Node e2 = cn.addOperation(OperationKind.CONCAT, e1, close2);

        Node regex = new Operand("[a-zA-Z0-9]+", OperandKind.STRREXP);

        cn.addConstraint(OperationKind.STR_EQUALSIC, startag, endtag);
        cn.addConstraint(OperationKind.MATCHES, startag, regex);
        cn.addConstraint(OperationKind.MATCHES, endtag, regex);

        Node r1 = cn.addOperation(OperationKind.CONCAT, s2, content);
        Node r2 = cn.addOperation(OperationKind.CONCAT, r1, e2);

        //Node matches2 = cn.addConstraint(OperationKind.MATCHES, strvar, con);

        Node matches2 = cn.addConstraint(OperationKind.MATCHES, strvar, r2);

        //cn.addConstraint(OperationKind.OR, matches1, matches2);

        cn.setStartNode(strvar);

        return cn;

        /**
        ConstraintNetwork cn = new ConstraintNetwork();
        Node op = new Operand(xmlInjection, OperandKind.STRREXP);
        cn.addVertex(op);
        cn.setStartNode(op);
        return cn;**/
    }

}
