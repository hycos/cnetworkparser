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


public class Z3Str2Listener extends ConstraintNetworkCreator {


    public static final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder.TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "RegexStar", "*", DefaultNodeKind.UNKNOWN);
        put(LanguageElements.KPLUS, "RegexPlus", "+", DefaultNodeKind.UNKNOWN);
        put(LanguageElements.UNION, "RegexUnion", "|", DefaultNodeKind.UNKNOWN);
        put(LanguageElements.RCONCAT, "RegexConcat", "", DefaultNodeKind.CONCAT);
        put(LanguageElements.OPT, "", "", DefaultNodeKind.UNKNOWN); // not available
        put(LanguageElements.RAN, "RegexCharRange", "", DefaultNodeKind.UNKNOWN);
        put(LanguageElements.CONV, "Str2Reg", "", DefaultNodeKind.UNKNOWN); // not available
        put(LanguageElements.MATCHES, "RegexIn", DefaultNodeKind.MATCHES.getValue(), DefaultNodeKind.MATCHES);
        // for string operations
        put(LanguageElements.LEN, "Length", DefaultNodeKind.LEN.getValue(), DefaultNodeKind.LEN);
        put(LanguageElements.CONTAINS, "Contains", DefaultNodeKind.CONTAINS.getValue(), DefaultNodeKind.CONTAINS);
        put(LanguageElements.INDEXOF, "Indexof", DefaultNodeKind.INDEXOF
                .getValue(), DefaultNodeKind.INDEXOF);
        put(LanguageElements.INDEXOF, "Indexof2", DefaultNodeKind.INDEXOF
                .getValue(), DefaultNodeKind.INDEXOF);
        put(LanguageElements.LASTINDEXOF, "LastIndexof", DefaultNodeKind.LASTINDEXOF.getValue
                        (), DefaultNodeKind.LASTINDEXOF);
        put(LanguageElements.LASTINDEXOF, "LastIndexOf", DefaultNodeKind.LASTINDEXOF.getValue(), DefaultNodeKind.LASTINDEXOF);
        put(LanguageElements.PREFIXOF, "StartsWith", DefaultNodeKind.STARTSWITH.getValue(), DefaultNodeKind.STARTSWITH);
        put(LanguageElements.SUFFIXOF, "EndsWith", DefaultNodeKind.ENDSWITH.getValue(), DefaultNodeKind.ENDSWITH);
        put(LanguageElements.SCONCAT, "Concat", DefaultNodeKind.CONCAT.getValue(), DefaultNodeKind.CONCAT);
        put(LanguageElements.SUBSTRNG, "Substring", DefaultNodeKind.SUBSTR.getValue(), DefaultNodeKind.SUBSTR);
        put(LanguageElements.REPLACE, "Replace", DefaultNodeKind.REPLACE.getValue(),
                DefaultNodeKind.REPLACE);
        // others
        put(LanguageElements.NEQ, "!=", DefaultNodeKind.NEQUALS.toString(), DefaultNodeKind.NEQUALS);
        put(LanguageElements.EQ, "=", DefaultNodeKind.EQUALS.toString(), DefaultNodeKind.EQUALS);
        put(LanguageElements.SMALLEREQ, "<=", DefaultNodeKind.SMALLEREQ.toString(), DefaultNodeKind.SMALLEREQ);
        put(LanguageElements.GREATEREQ, ">=", DefaultNodeKind.GREATEREQ.toString(), DefaultNodeKind.GREATEREQ);
        put(LanguageElements.SMALLER, "<", DefaultNodeKind.SMALLER.toString(), DefaultNodeKind.SMALLER);
        put(LanguageElements.GREATER, ">", DefaultNodeKind.GREATER.toString(), DefaultNodeKind.GREATER);
        put(LanguageElements.PLUS, "+", DefaultNodeKind.ADD.toString(), DefaultNodeKind.ADD);
        put(LanguageElements.MINUS, "-", DefaultNodeKind.SUB.toString(), DefaultNodeKind.SUB);
        //variables
        put(LanguageElements.DTSTRING, "String", DefaultNodeKind.STRVAR.toString(), DefaultNodeKind.STRVAR);
        put(LanguageElements.DTINT, "Int", DefaultNodeKind.NUMVAR.toString(), DefaultNodeKind.NUMVAR);
        put(LanguageElements.DTBOOLEAN, "Bool", DefaultNodeKind.BOOLVAR.toString(), DefaultNodeKind.BOOLVAR);
        // boolean operations
        put(LanguageElements.AND, "and", DefaultNodeKind.AND.toString(), DefaultNodeKind.AND);
        put(LanguageElements.OR, "or", DefaultNodeKind.OR.toString(), DefaultNodeKind.OR);
        put(LanguageElements.NOT, "not", DefaultNodeKind.NOT.toString(), DefaultNodeKind.NOT);
        put(LanguageElements.IMPLIES, "implies", DefaultNodeKind.IMPLIES.toString(),
                DefaultNodeKind.IMPLIES);
        // if-then-else
        put(LanguageElements.ITE, "ite", DefaultNodeKind.ITE.toString(), DefaultNodeKind.ITE);
        // division
        put(LanguageElements.DIV, "div", DefaultNodeKind.DIV.toString(), DefaultNodeKind.DIV);
    }};

    final static Logger LOGGER = LoggerFactory.getLogger(Z3Str2Listener.class);

    public Z3Str2Listener() {

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
