package com.github.hycos.cnetworkparser.lang.logic;

import com.github.hycos.cnetworkparser.core.ConstraintNetworkCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;
import org.snt.inmemantlr.exceptions.ParseTreeProcessorException;
import org.snt.inmemantlr.listener.DefaultListener;
import org.snt.inmemantlr.tree.ParseTree;

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
        ParseTree ast = this.getParseTree();
        LogicBuilder builder = new LogicBuilder(ast);
        try {
            return builder.process().getConstraintNetwork();
        } catch (ParseTreeProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }

    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() throws EUFInconsistencyException {
        ParseTree ast = this.getParseTree();
        LogicBuilder builder = new LogicBuilder(ast);
        try {
            return builder.process();
        } catch (ParseTreeProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }
}
