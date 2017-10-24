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

package com.github.hycos.cnetworkparser.lang.smt;

import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.DefaultNodeKind;
import com.github.hycos.cnetworkparser.core.ConstraintNetworkCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.exceptions.ParseTreeProcessorException;
import org.snt.inmemantlr.listener.DefaultListener;
import org.snt.inmemantlr.tree.ParseTree;


public class S3Listener extends ConstraintNetworkCreator {

    public static final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder.TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "Star", "*", DefaultNodeKind.UNKNOWN);
        put(LanguageElements.KPLUS, "", "", DefaultNodeKind.UNKNOWN); // not available
        put(LanguageElements.UNION, "Union", "", DefaultNodeKind.UNKNOWN);
        put(LanguageElements.RCONCAT, "Concat", "", DefaultNodeKind.CONCAT);

        put(LanguageElements.OPT, "", "", DefaultNodeKind.UNKNOWN); // not available
        put(LanguageElements.RAN, "", "", DefaultNodeKind.UNKNOWN); // not available
        put(LanguageElements.CONV, "", "", DefaultNodeKind.UNKNOWN); // not available
        put(LanguageElements.MATCHES, "In", DefaultNodeKind.MATCHES.getValue(), DefaultNodeKind.MATCHES);
        // for string operations
        put(LanguageElements.LEN, "Length", DefaultNodeKind.LEN.getValue(), DefaultNodeKind.LEN);
        put(LanguageElements.CONTAINS, "Contains", DefaultNodeKind.CONTAINS.getValue(), DefaultNodeKind.CONTAINS);
        put(LanguageElements.INDEXOF, "IndexOf", DefaultNodeKind.INDEXOF.getValue(), DefaultNodeKind.INDEXOF);
        put(LanguageElements.INDEXOF, "LastIndexof", DefaultNodeKind.INDEXOF.getValue
                        (),
                DefaultNodeKind.INDEXOF);
        put(LanguageElements.PREFIXOF, "StartsWith", DefaultNodeKind.STARTSWITH.getValue(), DefaultNodeKind.STARTSWITH);
        put(LanguageElements.SUFFIXOF, "EndsWith", DefaultNodeKind.ENDSWITH.getValue(), DefaultNodeKind.ENDSWITH);
        put(LanguageElements.SCONCAT, "Concat", DefaultNodeKind.CONCAT.getValue(), DefaultNodeKind.CONCAT);
        // others
        put(LanguageElements.NEQ, "!=", DefaultNodeKind.NEQUALS.toString(), DefaultNodeKind.NEQUALS);
        put(LanguageElements.EQ, "=", DefaultNodeKind.EQUALS.toString(), DefaultNodeKind.EQUALS);
        put(LanguageElements.SMALLEREQ, "<=", DefaultNodeKind.SMALLEREQ.toString(), DefaultNodeKind.SMALLEREQ);
        put(LanguageElements.GREATEREQ, ">=", DefaultNodeKind.GREATEREQ.toString(), DefaultNodeKind.GREATEREQ);
        put(LanguageElements.SMALLER, "<", DefaultNodeKind.SMALLER.toString(), DefaultNodeKind.SMALLER);
        put(LanguageElements.GREATER, ">", DefaultNodeKind.GREATER.toString(), DefaultNodeKind.GREATER);
        put(LanguageElements.PLUS, "+", DefaultNodeKind.ADD.toString(), DefaultNodeKind.ADD);
        put(LanguageElements.MINUS, "-", DefaultNodeKind.SUB.toString(), DefaultNodeKind.SUB);
        // variables
        put(LanguageElements.DTSTRING, "String", DefaultNodeKind.STRVAR.toString(), DefaultNodeKind.STRVAR);
        put(LanguageElements.DTINT, "Int", DefaultNodeKind.NUMVAR.toString(), DefaultNodeKind.NUMVAR);
        put(LanguageElements.DTBOOLEAN, "Bool", DefaultNodeKind.BOOLVAR.toString(), DefaultNodeKind.BOOLVAR);
        // boolean operations
        put(LanguageElements.AND, "and", DefaultNodeKind.AND.toString(), DefaultNodeKind.AND);
        put(LanguageElements.OR, "or", DefaultNodeKind.OR.toString(), DefaultNodeKind.OR);
        put(LanguageElements.NOT, "not", DefaultNodeKind.NOT.toString(), DefaultNodeKind.NOT);
    }};

    final static Logger LOGGER = LoggerFactory.getLogger(S3Listener.class);

    public S3Listener() {

    }

    @Override
    public DefaultListener getListener() {
        return this;
    }

    @Override
    public ConstraintNetwork getConstraintNetwork() throws InconsistencyException {
        ParseTree ast = this.getParseTree();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,tm);
        try {
            return builder.process().getConstraintNetwork();
        } catch (ParseTreeProcessorException e) {
            throw new InconsistencyException(e.getMessage());
        }
    }

    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() throws InconsistencyException {
        ParseTree ast = this.getParseTree();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,tm);
        try {
            return builder.process();
        } catch (ParseTreeProcessorException e) {
            throw new InconsistencyException(e.getMessage());
        }
    }
}
