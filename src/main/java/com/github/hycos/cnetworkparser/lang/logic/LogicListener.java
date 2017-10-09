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
