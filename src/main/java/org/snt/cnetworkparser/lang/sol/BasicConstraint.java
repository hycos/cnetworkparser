package org.snt.cnetworkparser.lang.sol;

import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.OperationKind;

public class BasicConstraint {
    protected Node term0;
    protected OperationKind opKind;
    protected Node term1;

    public BasicConstraint(Node term0, OperationKind kind, Node term1) {
        this.term0 = term0;
        this.opKind = kind;
        this.term1 = term1;
    }
    public BasicConstraint() {this(null,null,null);}

    public void addNode(Node term) {
        if(this.term1 == null) {
            this.term1 = term;
        } else {
            this.term0 = term;
        }
    }

    public void setOpKind(OperationKind kind) {
        this.opKind = kind;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(term0.toString() + "\n");
        sb.append("KIND " + opKind.toString() + "\n");
        sb.append(term1.toString());
        return sb.toString();
    }

    public void revert() {
        Node tmp = term0;
        term0 = term1;
        term1 = tmp;
    }
    public void clear() {
        term0 = null;
        term1 = null;
        opKind = null;
    }

    public boolean isBoolean() {
        if(this.term0 == null || this.term1 == null)
            return false;
        return (this.term0.isBoolean() && this.term1.isBoolean());
    }

    public boolean isString() {
        if(this.term0 == null || this.term1 == null)
            return false;
        return (this.term0.isString() && this.term1.isString());
    }

    public boolean isNumeric() {
        if(this.term0 == null || this.term1 == null)
            return false;
        return (this.term0.isNumeric() && this.term1.isNumeric());
    }

}