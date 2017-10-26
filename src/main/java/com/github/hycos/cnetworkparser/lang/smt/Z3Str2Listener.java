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

import com.github.hycos.cnetworkparser.core.ConstraintNetworkBuilderFactoryInterface;
import com.github.hycos.cnetworkparser.core.ConstraintNetworkCreator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.listener.DefaultListener;


public class Z3Str2Listener extends ConstraintNetworkCreator {


    final static Logger LOGGER = LoggerFactory.getLogger(Z3Str2Listener.class);


    public final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder
            .TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "RegexStar", "*",ni.getNodeKindFromString("unknown"));
        put(LanguageElements.KPLUS, "RegexPlus", "+", ni.getNodeKindFromString("unknown"));
        put(LanguageElements.UNION, "RegexUnion", "|", ni.getNodeKindFromString("unknown"));
        put(LanguageElements.RCONCAT, "RegexConcat", "", ni.getNodeKindFromString("concat"));
        put(LanguageElements.OPT, "", "", ni.getNodeKindFromString("unknown")); // not available
        put(LanguageElements.RAN, "RegexCharRange", "", ni.getNodeKindFromString("unknown"));
        put(LanguageElements.CONV, "Str2Reg", "", ni.getNodeKindFromString("unknown")); // not available
        put(LanguageElements.MATCHES, "RegexIn", ni.getNodeKindFromString("matches").getValue(), ni.getNodeKindFromString("matches"));
        // for string operations
        put(LanguageElements.LEN, "Length", ni.getNodeKindFromString("len").getValue(), ni.getNodeKindFromString("len"));
        put(LanguageElements.CONTAINS, "Contains", ni.getNodeKindFromString("contains").getValue(), ni.getNodeKindFromString("contains"));
        put(LanguageElements.INDEXOF, "Indexof", ni.getNodeKindFromString("indexof")
                .getValue(), ni.getNodeKindFromString("indexof"));
        put(LanguageElements.INDEXOF, "Indexof2", ni.getNodeKindFromString("indexof")
                .getValue(), ni.getNodeKindFromString("indexof"));
        put(LanguageElements.LASTINDEXOF, "LastIndexof",  ni
                .getNodeKindFromString("lastindexof").getValue
                        (),  ni.getNodeKindFromString("lastindexof"));
        put(LanguageElements.PREFIXOF, "StartsWith", ni.getNodeKindFromString("startswith").getValue(), ni.getNodeKindFromString("startswith"));
        put(LanguageElements.SUFFIXOF, "EndsWith", ni.getNodeKindFromString("endswith").getValue(), ni.getNodeKindFromString("endswith"));
        put(LanguageElements.SCONCAT, "Concat", ni.getNodeKindFromString("concat").getValue(), ni.getNodeKindFromString("concat"));
        put(LanguageElements.SUBSTRNG, "Substring", ni.getNodeKindFromString
                ("substr").getValue(), ni.getNodeKindFromString("substr"));
        put(LanguageElements.REPLACE, "Replace", ni.getNodeKindFromString
                        ("replace").getValue(),
                ni.getNodeKindFromString("replace"));
        // others
        put(LanguageElements.NEQ, "!=", ni.getNodeKindFromString("!=")
                .toString(), ni.getNodeKindFromString("!="));
        put(LanguageElements.EQ, "=", ni.getNodeKindFromString("==")
                .toString(), ni.getNodeKindFromString("=="));
        put(LanguageElements.SMALLEREQ, "<=", ni.getNodeKindFromString("<=")
                .toString(), ni.getNodeKindFromString("<="));
        put(LanguageElements.GREATEREQ, ">=", ni.getNodeKindFromString(">=").toString(), ni.getNodeKindFromString(">="));
        put(LanguageElements.SMALLER, "<", ni.getNodeKindFromString("<").toString(), ni.getNodeKindFromString("<"));
        put(LanguageElements.GREATER, ">", ni.getNodeKindFromString(">").toString(), ni.getNodeKindFromString(">"));
        put(LanguageElements.PLUS, "+", ni.getNodeKindFromString("add")
                .toString(), ni.getNodeKindFromString("add"));
        put(LanguageElements.MINUS, "-", ni.getNodeKindFromString("sub")
                .toString(), ni.getNodeKindFromString("sub"));
        //variables
        put(LanguageElements.DTSTRING, "String", ni.getNodeKindFromString("strvar").toString(), ni.getNodeKindFromString("strvar"));
        put(LanguageElements.DTINT, "Int", ni.getNodeKindFromString("numar").toString(), ni.getNodeKindFromString("numar"));
        put(LanguageElements.DTBOOLEAN, "Bool", ni.getNodeKindFromString("boolvar").toString(), ni.getNodeKindFromString("boolvar"));
        // boolean operations
        put(LanguageElements.AND, "and", ni.getNodeKindFromString("and").toString(), ni.getNodeKindFromString("and"));
        put(LanguageElements.OR, "or", ni.getNodeKindFromString("or").toString(), ni.getNodeKindFromString("or"));
        put(LanguageElements.NOT, "not", ni.getNodeKindFromString("not").toString(), ni.getNodeKindFromString("not"));
        put(LanguageElements.IMPLIES, "implies",ni.getNodeKindFromString
                ("implies").toString(), ni.getNodeKindFromString
                ("implies"));
        // if-then-else
        put(LanguageElements.ITE, "ite", ni.getNodeKindFromString
                ("ite").toString(),ni.getNodeKindFromString
                ("ite"));
        // division
        put(LanguageElements.DIV, "div", ni.getNodeKindFromString
                ("div").toString(), ni.getNodeKindFromString
                ("div"));
    }};


    public Z3Str2Listener() {
        super();
    }

    public Z3Str2Listener(ConstraintNetworkBuilderFactoryInterface bld) {
        super(bld);
    }

    @Override
    public SmtCnetworkBuilder.TransMap getTransMap() {
        return tm;
    }

    @Override
    public DefaultListener getListener() {
        return this;
    }

}
