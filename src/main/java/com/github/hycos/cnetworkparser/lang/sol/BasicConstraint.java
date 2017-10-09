package com.github.hycos.cnetworkparser.lang.sol;

import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;

import java.util.List;
import java.util.Vector;

public class BasicConstraint {
    protected List<Node> nodes = new Vector<>();
    protected NodeKind opKind;

    public BasicConstraint(Node term0, NodeKind kind, Node term1) {
        opKind = kind;
        nodes.add(term0);
        nodes.add(term1);
    }
    public BasicConstraint() {this(null,null,null);}

    public void addNode(Node term) {
        nodes.add(term);
    }

    public void setOpKind(NodeKind kind) {
        this.opKind = kind;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("KIND " + opKind.toString() + "\n");
        nodes.forEach(
                v -> {
                    sb.append("n:" + v.getLabel());
                    sb.append("kind:" + v.getKind());
                }
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
        return this.nodes.stream().filter( v -> v.isString() || v.isRegex()
                || v.getKind() == NodeKind.STRLIT).count() ==
                nodes.size();
    }

    public boolean isNumeric() {
        return this.nodes.stream().filter( v -> v.isNumeric()).count() ==
                nodes.size();
    }
}