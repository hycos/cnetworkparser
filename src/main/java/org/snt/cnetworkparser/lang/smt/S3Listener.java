package org.snt.cnetworkparser.lang.smt;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetworkparser.core.CnetworkCreator;
import org.snt.inmemantlr.listener.DefaultListener;
import org.snt.inmemantlr.tree.Ast;


public class S3Listener extends CnetworkCreator {

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

    public S3Listener(boolean eufEnabled) {
        super(eufEnabled);
    }

    @Override
    public DefaultListener getListener() {
        return this;
    }

    @Override
    public ConstraintNetwork getConstraintNetwork() {
        Ast ast = this.getAst();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,eufEnabled,tm);
        return builder.process().getConstraintNetwork();
    }

    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() {
        Ast ast = this.getAst();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,eufEnabled,tm);
        return builder.process();
    }
}
