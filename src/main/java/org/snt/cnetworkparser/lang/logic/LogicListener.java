package org.snt.cnetworkparser.lang.logic;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.graph.ConstraintNetwork;
import org.snt.cnetwork.core.graph.ConstraintNetworkBuilder;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetworkparser.core.ConstraintNetworkCreator;
import org.snt.inmemantlr.exceptions.AstProcessorException;
import org.snt.inmemantlr.listener.DefaultListener;
import org.snt.inmemantlr.tree.Ast;

/**
 * Created by julian on 24/02/2017.
 */
public class LogicListener extends ConstraintNetworkCreator {

    final static Logger LOGGER = LoggerFactory.getLogger(LogicListener.class);




    public LogicListener(){
        super();
    }

    @Override
    public DefaultListener getListener() {
        return this;
    }

    @Override
    public ConstraintNetwork getConstraintNetwork() throws EUFInconsistencyException {
        Ast ast = this.getAst();
        LogicBuilder builder = new LogicBuilder(ast);
        try {
            return builder.process().getConstraintNetwork();
        } catch (AstProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }

    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() throws EUFInconsistencyException {
        Ast ast = this.getAst();
        LogicBuilder builder = new LogicBuilder(ast);
        try {
            return builder.process();
        } catch (AstProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }
}
