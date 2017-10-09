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