package org.snt.cnetworkparser.lang.logic;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.Node;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.core.Operand;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.inmemantlr.exceptions.AstProcessorException;
import org.snt.inmemantlr.tree.Ast;
import org.snt.inmemantlr.tree.AstNode;
import org.snt.inmemantlr.tree.AstProcessor;


public class SimpleLogic extends
        AstProcessor<ConstraintNetworkBuilder, Node> {


    protected ConstraintNetworkBuilder cn = null;

    final static Logger LOGGER = LoggerFactory.getLogger(SimpleLogic.class);

    /**
     * constructor
     *
     * @param ast abstract syntax tree to process
     */
    public SimpleLogic(Ast ast, boolean eufEnabled) {
        super(ast);

        LOGGER.debug(ast.toDot());
        cn = new ConstraintNetworkBuilder(eufEnabled);
    }


    @Override
    public ConstraintNetworkBuilder getResult() {
        return cn;
    }

    @Override
    protected void initialize() {

    }

    @Override
    protected void process(AstNode n) throws AstProcessorException {
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
                    Node bool = this.cn.addNode(new Operand(n.getLabel(),
                            NodeKind.BOOLVAR));
                    this.smap.put(n, bool);
                    break;
                case "s":
                    simpleProp(n);
                    break;


            }
        } catch (EUFInconsistencyException e) {
            throw new AstProcessorException(e.getMessage());
        }
    }
}
