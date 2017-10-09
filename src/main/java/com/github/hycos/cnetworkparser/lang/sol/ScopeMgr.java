/*
 * cnetworkparser - generate constraint network from different formats
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetworkparser is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

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