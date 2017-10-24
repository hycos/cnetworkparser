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
import com.github.hycos.cnetwork.core.graph.DefaultNodeKind;

import java.util.List;
import java.util.Vector;

public class BasicConstraint {
    protected List<Node> nodes = new Vector<>();
    protected DefaultNodeKind opKind;

    public BasicConstraint(Node term0, DefaultNodeKind kind, Node term1) {
        opKind = kind;
        nodes.add(term0);
        nodes.add(term1);
    }
    public BasicConstraint() {this(null,null,null);}

    public void addNode(Node term) {
        nodes.add(term);
    }

    public void setOpKind(DefaultNodeKind kind) {
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
                || v.getKind() == DefaultNodeKind.STRLIT).count() ==
                nodes.size();
    }

    public boolean isNumeric() {
        return this.nodes.stream().filter( v -> v.isNumeric()).count() ==
                nodes.size();
    }
}