package org.snt.cnetworkparser.lang.sol;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetworkparser.exception.ParserException;

import java.util.HashMap;
import java.util.Set;

public class ConfigReader {

    final static Logger LOGGER = LoggerFactory.getLogger(ConfigReader.class);

    private HashMap<String, Operation> opLookup = new HashMap<String, Operation>();
    private ConfigReader inst;
    private ConstraintNetwork cnetwork;

    public ConfigReader() {
        this.cnetwork = new ConstraintNetwork();
    }


    public void addVariable(String type, String label){
        Operand op = null;

        //LOGGER.info("type " + type);
        //LOGGER.info("label " + label);

        if(type.equals("string")) {
            op = new Operand(label, NodeKind.STRVAR);
        } else if(type.equals("int")) {
            op = new Operand(label, NodeKind.NUMVAR);
        } else if (type.equals("bool")) {
            op = new Operand(label, NodeKind.BOOLVAR);
        }

        assert(op != null);

        this.cnetwork.addNode(op);
    }



    public void addNode(Node n) {
        this.cnetwork.addNode(n);
    }

    public void addOperand(Node op){
        this.cnetwork.addNode(op);
    }

    public void addConnection(Node src, Node target, EdgeKind kind, int priority) {
        this.cnetwork.addConnection(src, target, kind, priority);
    }

    public void addConnection(Edge e) {
        this.cnetwork.addConnection(e);
    }

    public void addConnections(Set<Edge> edges) {
        this.cnetwork.addConnections(edges);
    }

    public void addOperation(Node op) {
        this.cnetwork.addNode(op);
    }


    public Operand getOperandByLabel(String label) {
        Node n = this.cnetwork.getOperandByLabel(label);

        if(n == null)
            return null;

        assert(n instanceof Operand);

        return (Operand)n;
    }

    public Operation getOperationByLabel(String label) {
        Node n = this.cnetwork.getOperationByLabel(label);

        if(n == null)
            return null;

        assert(n instanceof Operation);

        return (Operation)n;
    }


    public Operation getOperationByName(String name) throws ParserException {
        if(!opLookup.containsKey(name))
            throw new ParserException("Operation " + name + " has not been declared");

        return new Operation(opLookup.get(name));
    }

    public Operation addConstraint(BasicConstraint con) {
        return this.cnetwork.addConstraint(con.opKind, con.nodes.toArray(new
                Node [con.nodes.size()]));
    }



    @Override
    public String toString() {
        return cnetwork.toString();
    }

    public ConstraintNetwork getCnetwork() {
        return this.cnetwork;
    }


}
