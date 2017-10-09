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

package com.github.hycos.cnetworkparser.lang.logic;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;
import org.snt.inmemantlr.exceptions.ParseTreeProcessorException;
import org.snt.inmemantlr.tree.ParseTree;
import org.snt.inmemantlr.tree.ParseTreeNode;
import org.snt.inmemantlr.tree.ParseTreeProcessor;


public class LogicBuilder extends
        ParseTreeProcessor<ConstraintNetworkBuilder, Node> {


    protected ConstraintNetworkBuilder cn = null;

    final static Logger LOGGER = LoggerFactory.getLogger(LogicBuilder.class);

    /**
     * constructor
     *
     * @param ast abstract syntax tree to process
     */
    public LogicBuilder(ParseTree ast) {
        super(ast);

        LOGGER.debug(ast.toDot());
        cn = new ConstraintNetworkBuilder();
    }


    @Override
    public ConstraintNetworkBuilder getResult() {
        return cn;
    }

    @Override
    protected void initialize() {

    }

    @Override
    protected void process(ParseTreeNode n) throws ParseTreeProcessorException {
        LOGGER.info("ID " + n.getId() + " " + n.getRule() + " " + n.getLabel());

        try {
            switch (n.getRule()) {
                case "expression":
                    NodeKind kind = NodeKind.UNKNOWN;

                    if(n.getChildren().size() == 3) {
                        // we have a boolean expression

                        assert smap.containsKey(n.getChild(0));
                        assert smap.containsKey(n.getChild(2));

                        switch(n.getChild(1).getLabel()) {
                            case "and":
                                kind = NodeKind.AND;
                                break;
                            case "or":
                                kind = NodeKind.OR;
                                break;
                            case "xor":
                                kind = NodeKind.XOR;
                                break;
                            case "implies":
                                kind = NodeKind.IMPLIES;
                                break;
                        }

                        Node op = cn.addOperation(kind, smap.get(n.getChild
                                (0)), smap.get(n.getChild(2)));

                        smap.put(n, op);
                    } else if(n.getChildren().size() == 2) {

                        assert smap.containsKey(n.getChild(1));

                        switch(n.getChild(0).getLabel()) {
                            case "not":
                                kind = NodeKind.NOT;
                                break;
                        }

                        Node op = cn.addOperation(kind, smap.get(n.getChild
                                (1)));

                        smap.put(n, op);

                    } else {
                        assert n.getChildren().size() == 1;
                        simpleProp(n);
                    }
                    break;
                case "atom":
                    Node bool = this.cn.addOperand(NodeKind.BOOLVAR,n
                            .getLabel());
                    this.smap.put(n, bool);
                    break;
                case "s":
                    simpleProp(n);
                    break;


            }
        } catch (EUFInconsistencyException e) {
            throw new ParseTreeProcessorException(e.getMessage());
        }
    }
}
