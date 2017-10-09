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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetwork;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;
import com.github.hycos.cnetworkparser.core.ConstraintNetworkCreator;
import org.snt.inmemantlr.exceptions.ParseTreeProcessorException;
import org.snt.inmemantlr.listener.DefaultListener;
import org.snt.inmemantlr.tree.ParseTree;


public class S3Listener extends ConstraintNetworkCreator {

    public static final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder.TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "Star", "*", NodeKind.UNKNOWN);
        put(LanguageElements.KPLUS, "", "", NodeKind.UNKNOWN); // not available
        put(LanguageElements.UNION, "Union", "", NodeKind.UNKNOWN);
        put(LanguageElements.RCONCAT, "Concat", "", NodeKind.CONCAT);

        put(LanguageElements.OPT, "", "", NodeKind.UNKNOWN); // not available
        put(LanguageElements.RAN, "", "", NodeKind.UNKNOWN); // not available
        put(LanguageElements.CONV, "", "", NodeKind.UNKNOWN); // not available
        put(LanguageElements.MATCHES, "In", NodeKind.MATCHES.getValue(), NodeKind.MATCHES);
        // for string operations
        put(LanguageElements.LEN, "Length", NodeKind.LEN.getValue(), NodeKind.LEN);
        put(LanguageElements.CONTAINS, "Contains", NodeKind.CONTAINS.getValue(), NodeKind.CONTAINS);
        put(LanguageElements.INDEXOF, "IndexOf", NodeKind.INDEXOF.getValue(), NodeKind.INDEXOF);
        put(LanguageElements.INDEXOF, "LastIndexof", NodeKind.INDEXOF.getValue
                        (),
                NodeKind.INDEXOF);
        put(LanguageElements.PREFIXOF, "StartsWith", NodeKind.STARTSWITH.getValue(), NodeKind.STARTSWITH);
        put(LanguageElements.SUFFIXOF, "EndsWith", NodeKind.ENDSWITH.getValue(), NodeKind.ENDSWITH);
        put(LanguageElements.SCONCAT, "Concat", NodeKind.CONCAT.getValue(), NodeKind.CONCAT);
        // others
        put(LanguageElements.NEQ, "!=", NodeKind.NEQUALS.toString(), NodeKind.NEQUALS);
        put(LanguageElements.EQ, "=", NodeKind.EQUALS.toString(), NodeKind.EQUALS);
        put(LanguageElements.SMALLEREQ, "<=", NodeKind.SMALLEREQ.toString(), NodeKind.SMALLEREQ);
        put(LanguageElements.GREATEREQ, ">=", NodeKind.GREATEREQ.toString(), NodeKind.GREATEREQ);
        put(LanguageElements.SMALLER, "<", NodeKind.SMALLER.toString(), NodeKind.SMALLER);
        put(LanguageElements.GREATER, ">", NodeKind.GREATER.toString(), NodeKind.GREATER);
        put(LanguageElements.PLUS, "+", NodeKind.ADD.toString(), NodeKind.ADD);
        put(LanguageElements.MINUS, "-", NodeKind.SUB.toString(), NodeKind.SUB);
        // variables
        put(LanguageElements.DTSTRING, "String", NodeKind.STRVAR.toString(), NodeKind.STRVAR);
        put(LanguageElements.DTINT, "Int", NodeKind.NUMVAR.toString(), NodeKind.NUMVAR);
        put(LanguageElements.DTBOOLEAN, "Bool", NodeKind.BOOLVAR.toString(), NodeKind.BOOLVAR);
        // boolean operations
        put(LanguageElements.AND, "and", NodeKind.AND.toString(), NodeKind.AND);
        put(LanguageElements.OR, "or", NodeKind.OR.toString(), NodeKind.OR);
        put(LanguageElements.NOT, "not", NodeKind.NOT.toString(), NodeKind.NOT);
    }};

    final static Logger LOGGER = LoggerFactory.getLogger(S3Listener.class);

    public S3Listener() {

    }

    @Override
    public DefaultListener getListener() {
        return this;
    }

    @Override
    public ConstraintNetwork getConstraintNetwork() throws EUFInconsistencyException {
        ParseTree ast = this.getParseTree();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,tm);
        try {
            return builder.process().getConstraintNetwork();
        } catch (ParseTreeProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }

    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() throws EUFInconsistencyException {
        ParseTree ast = this.getParseTree();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,tm);
        try {
            return builder.process();
        } catch (ParseTreeProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }
}
