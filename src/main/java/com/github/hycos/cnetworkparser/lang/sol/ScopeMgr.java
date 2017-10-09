package com.github.hycos.cnetworkparser.lang.sol;

import com.github.hycos.cnetwork.core.graph.Node;

import java.util.LinkedList;
import java.util.Stack;

public class ScopeMgr {

    Stack<RuleScope> ctx = null;
    public ScopeMgr() {
        this.ctx = new Stack<RuleScope>();
    }

    public void enterNewCtx() {
        RuleScope sc = new RuleScope(this.ctx.peek());
        this.ctx.push(sc);
    }
    public void leaveOldCtx() {
        assert(!this.ctx.isEmpty());
        this.ctx.pop();
    }

    public void push(Node n) {
        this.ctx.peek().addNode(n);
    }

    public LinkedList<Node> getNodesForCtx() {
        return this.ctx.peek().getNodes();
    }

    public void push(String key, String value) {
        push(new StringPair(key,value));
    }

    public void push(StringPair pair) {
        if(this.ctx.isEmpty()) {
            RuleScope sc = new RuleScope(null);
            sc.push(pair);
            this.ctx.push(sc);
        }
        this.ctx.peek().push(pair);
    }

    public StringPair pop() {
        assert(!this.ctx.isEmpty());
        if(this.ctx.lastElement().size() > 0)
            return this.ctx.lastElement().pop();
        else
            return null;
    }

    public RuleScope getRecentCxt() {
        return this.ctx.peek();
    }

    public String getRecentCtxId() {
        assert(this.ctx.size()>0);
        return this.ctx.peek().getName();
    }

    public int size() {
        return this.ctx.size();
    }
}