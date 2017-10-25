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


public class S3Listener extends ConstraintNetworkCreator {

    final static Logger LOGGER = LoggerFactory.getLogger(S3Listener.class);

    public final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder.TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "Star", "*", ni.getNodeKindFromString("unknown"));
        put(LanguageElements.KPLUS, "", "", ni.getNodeKindFromString("unknown")); // not available
        put(LanguageElements.UNION, "Union", "", ni.getNodeKindFromString("unknown"));
        put(LanguageElements.RCONCAT, "Concat", ni
                .getNodeKindFromString("concat").getValue(), ni
                .getNodeKindFromString("concat"));

        put(LanguageElements.OPT, "", "", ni.getNodeKindFromString("unknown")); // not available
        put(LanguageElements.RAN, "", "", ni.getNodeKindFromString("unknown")); // not available
        put(LanguageElements.CONV, "", "", ni.getNodeKindFromString("unknown")); // not available
        put(LanguageElements.MATCHES, "In", ni.getNodeKindFromString("matches").getValue(), ni.getNodeKindFromString("matches"));
        // for string operations
        put(LanguageElements.LEN, "Length", ni.getNodeKindFromString("len").getValue(), ni.getNodeKindFromString("len"));
        put(LanguageElements.CONTAINS, "Contains", ni.getNodeKindFromString("contains").getValue(), ni.getNodeKindFromString("contains"));
        put(LanguageElements.INDEXOF, "IndexOf", ni.getNodeKindFromString("indexof").getValue(), ni.getNodeKindFromString("indexof"));
        put(LanguageElements.INDEXOF, "LastIndexof", ni.getNodeKindFromString("indexof").getValue
                        (),
                ni.getNodeKindFromString("indexof"));
        put(LanguageElements.PREFIXOF, "StartsWith", ni.getNodeKindFromString("startswith").getValue(), ni.getNodeKindFromString("startswith"));
        put(LanguageElements.SUFFIXOF, "EndsWith", ni.getNodeKindFromString("endswith").getValue(), ni.getNodeKindFromString("endswith"));
        put(LanguageElements.SCONCAT, "Concat", ni.getNodeKindFromString("concat").getValue(), ni.getNodeKindFromString("concat"));
        // others
        put(LanguageElements.NEQ, "!=", ni.getNodeKindFromString("!=")
                .toString(), ni.getNodeKindFromString("!="));
        put(LanguageElements.EQ, "=", ni.getNodeKindFromString("==")
                .toString(), ni.getNodeKindFromString("=="));
        put(LanguageElements.SMALLEREQ, "<=", ni.getNodeKindFromString("<=")
                .toString(), ni.getNodeKindFromString(">="));
        put(LanguageElements.GREATEREQ, ">=", ni.getNodeKindFromString(">=").toString(), ni.getNodeKindFromString(">="));
        put(LanguageElements.SMALLER, "<", ni.getNodeKindFromString("<").toString(), ni.getNodeKindFromString("<"));
        put(LanguageElements.GREATER, ">", ni.getNodeKindFromString(">").toString(), ni.getNodeKindFromString(">"));
        put(LanguageElements.PLUS, "+", ni.getNodeKindFromString("add")
                .toString(), ni.getNodeKindFromString("add"));
        put(LanguageElements.MINUS, "-", ni.getNodeKindFromString("sub")
                .toString(), ni.getNodeKindFromString("sub"));
        // variables
        put(LanguageElements.DTSTRING, "String", ni.getNodeKindFromString("strvar").toString(), ni.getNodeKindFromString("strvar"));
        put(LanguageElements.DTINT, "Int", ni.getNodeKindFromString("numar").toString(), ni.getNodeKindFromString("numar"));
        put(LanguageElements.DTBOOLEAN, "Bool", ni.getNodeKindFromString("boolvar").toString(), ni.getNodeKindFromString("boolvar"));
        // boolean operations
        put(LanguageElements.AND, "and", ni.getNodeKindFromString("and").toString(), ni.getNodeKindFromString("and"));
        put(LanguageElements.OR, "or", ni.getNodeKindFromString("or").toString(), ni.getNodeKindFromString("or"));
        put(LanguageElements.NOT, "not", ni.getNodeKindFromString("not").toString(), ni.getNodeKindFromString("not"));
    }};


    public S3Listener() {
        super();
    }


    public S3Listener(ConstraintNetworkBuilderFactoryInterface bld) {
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

