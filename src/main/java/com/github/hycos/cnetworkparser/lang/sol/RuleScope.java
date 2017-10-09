package com.github.hycos.cnetworkparser.lang.sol;

import com.github.hycos.cnetwork.core.graph.Node;

import java.util.LinkedList;
import java.util.Stack;


public class RuleScope {

    Stack<StringPair> ctx;
    LinkedList<Node> nods;

    RuleScope backref = null;

    public RuleScope(RuleScope backref) {
        this.backref = backref;
        this.ctx = new Stack<StringPair>();
        this.nods = new LinkedList<Node>();
    }

    public int size(){
        return this.ctx.size();
    }

    public RuleScope getBackref () {
        return this.backref;
    }

    public String getName() {
        assert(this.size() > 0);
        return this.ctx.get(0).key;
    }

    public void push(StringPair pair) {
        this.ctx.push(pair);
    }

    public StringPair pop() {
        return this.ctx.pop();
    }

    public StringPair peek() {
        return this.ctx.peek();
    }

    public void addNode(Node n) {
        this.nods.add(n);
    }

    public LinkedList<Node>getNodes() {
        return this.nods;
    }

}