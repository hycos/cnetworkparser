package org.snt.cnetworkparser.lang.sol;

import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.OperationKind;

import java.util.LinkedHashSet;
import java.util.Set;

public class BasicConstraint {
    protected Set<Node> nodes = new LinkedHashSet<Node>();
    protected OperationKind opKind;
    protected Node term1;

    public BasicConstraint(Node term0, OperationKind kind, Node term1) {
        opKind = kind;
        nodes.add(term0);
        nodes.add(term1);
    }
    public BasicConstraint() {this(null,null,null);}

    public void addNode(Node term) {
        nodes.add(term);
    }

    public void setOpKind(OperationKind kind) {
        this.opKind = kind;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("KIND " + opKind.toString() + "\n");
        nodes.forEach(
                v -> sb.append("n: " + v.getLabel())
        );
        return sb.toString();
    }

    public void clear() {
        opKind = null;
        nodes.clear();;
    }

    public boolean isBoolean() {
        return this.nodes.stream().filter( v -> v.isBoolean()).count() ==
                nodes.size();
    }

    public boolean isString() {
        return this.nodes.stream().filter( v -> v.isString()).count() ==
                nodes.size();
    }

    public boolean isNumeric() {
        return this.nodes.stream().filter( v -> v.isNumeric()).count() ==
                nodes.size();
    }
}