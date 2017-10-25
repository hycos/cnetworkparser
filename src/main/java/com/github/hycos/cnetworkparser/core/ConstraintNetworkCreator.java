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

package com.github.hycos.cnetworkparser.core;

import com.github.hycos.cnetwork.api.NodeKindFactoryInterface;
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetworkparser.lang.smt.SmtCnetworkBuilder;
import org.snt.inmemantlr.exceptions.ParseTreeProcessorException;
import org.snt.inmemantlr.listener.DefaultTreeListener;
import org.snt.inmemantlr.tree.ParseTree;

import java.util.Objects;


public abstract class ConstraintNetworkCreator extends DefaultTreeListener implements
        ConstraintNetworkGenerator {

    private ConstraintNetworkBuilderFactoryInterface bld = null;
    protected static NodeKindFactoryInterface ni = null;

    public abstract SmtCnetworkBuilder.TransMap getTransMap();

    public ConstraintNetworkCreator(ConstraintNetworkBuilderFactoryInterface bld) {
        this.bld = bld;
        this.ni = this.bld.getNodeKindFactory();
    }

    public ConstraintNetworkCreator() {
        this.bld = new DefaultConstraintNetworkBuilderFactory();
        this.ni = this.bld.getNodeKindFactory();
    }

    @Override
    public ConstraintNetwork getConstraintNetwork() throws InconsistencyException {
        ParseTree ast = this.getParseTree();

        Objects.nonNull(bld);

        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,getTransMap(),bld);
        try {
            return builder.process().getConstraintNetwork();
        } catch (ParseTreeProcessorException e) {
            throw new InconsistencyException(e.getMessage());
        }
    }

    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() throws InconsistencyException {
        ParseTree ast = this.getParseTree();

        Objects.nonNull(bld);

        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,getTransMap(),bld);
        try {
            return builder.process();
        } catch (ParseTreeProcessorException e) {
            throw new InconsistencyException(e.getMessage());
        }
    }


}
