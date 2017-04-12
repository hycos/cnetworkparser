package org.snt.cnetworkparser.lang.smt;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.ConstraintNetworkBuilder;
import org.snt.cnetwork.core.NodeKind;
import org.snt.cnetwork.exception.EUFInconsistencyException;
import org.snt.cnetworkparser.core.ConstraintNetworkCreator;
import org.snt.inmemantlr.exceptions.AstProcessorException;
import org.snt.inmemantlr.listener.DefaultListener;
import org.snt.inmemantlr.tree.Ast;


public class Z3Str2Listener extends ConstraintNetworkCreator {


    public static final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder.TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "RegexStar", "*", NodeKind.UNKNOWN);
        put(LanguageElements.KPLUS, "RegexPlus", "+", NodeKind.UNKNOWN);
        put(LanguageElements.UNION, "RegexUnion", "|", NodeKind.UNKNOWN);
        put(LanguageElements.RCONCAT, "RegexConcat", "", NodeKind.CONCAT);
        put(LanguageElements.OPT, "", "", NodeKind.UNKNOWN); // not available
        put(LanguageElements.RAN, "RegexCharRange", "", NodeKind.UNKNOWN);
        put(LanguageElements.CONV, "Str2Reg", "", NodeKind.UNKNOWN); // not available
        put(LanguageElements.MATCHES, "RegexIn", NodeKind.MATCHES.getValue(), NodeKind.MATCHES);
        // for string operations
        put(LanguageElements.LEN, "Length", NodeKind.LEN.getValue(), NodeKind.LEN);
        put(LanguageElements.CONTAINS, "Contains", NodeKind.CONTAINS.getValue(), NodeKind.CONTAINS);
        put(LanguageElements.INDEXOF, "Indexof", NodeKind.INDEXOF
                .getValue(), NodeKind.INDEXOF);
        put(LanguageElements.INDEXOF, "Indexof2", NodeKind.INDEXOF
                .getValue(), NodeKind.INDEXOF);
        put(LanguageElements.LASTINDEXOF, "LastIndexof", NodeKind.LASTINDEXOF.getValue
                        (), NodeKind.LASTINDEXOF);
        put(LanguageElements.LASTINDEXOF, "LastIndexOf", NodeKind.LASTINDEXOF.getValue(), NodeKind.LASTINDEXOF);
        put(LanguageElements.PREFIXOF, "StartsWith", NodeKind.STARTSWITH.getValue(), NodeKind.STARTSWITH);
        put(LanguageElements.SUFFIXOF, "EndsWith", NodeKind.ENDSWITH.getValue(), NodeKind.ENDSWITH);
        put(LanguageElements.SCONCAT, "Concat", NodeKind.CONCAT.getValue(), NodeKind.CONCAT);
        put(LanguageElements.SUBSTRNG, "Substring", NodeKind.SUBSTR.getValue(), NodeKind.SUBSTR);
        put(LanguageElements.REPLACE, "Replace", NodeKind.REPLACE.getValue(),
                NodeKind.REPLACE);
        // others
        put(LanguageElements.NEQ, "!=", NodeKind.NEQUALS.toString(), NodeKind.NEQUALS);
        put(LanguageElements.EQ, "=", NodeKind.EQUALS.toString(), NodeKind.EQUALS);
        put(LanguageElements.SMALLEREQ, "<=", NodeKind.SMALLEREQ.toString(), NodeKind.SMALLEREQ);
        put(LanguageElements.GREATEREQ, ">=", NodeKind.GREATEREQ.toString(), NodeKind.GREATEREQ);
        put(LanguageElements.SMALLER, "<", NodeKind.SMALLER.toString(), NodeKind.SMALLER);
        put(LanguageElements.GREATER, ">", NodeKind.GREATER.toString(), NodeKind.GREATER);
        put(LanguageElements.PLUS, "+", NodeKind.ADD.toString(), NodeKind.ADD);
        put(LanguageElements.MINUS, "-", NodeKind.SUB.toString(), NodeKind.SUB);
        //variables
        put(LanguageElements.DTSTRING, "String", NodeKind.STRVAR.toString(), NodeKind.STRVAR);
        put(LanguageElements.DTINT, "Int", NodeKind.NUMVAR.toString(), NodeKind.NUMVAR);
        put(LanguageElements.DTBOOLEAN, "Bool", NodeKind.BOOLVAR.toString(), NodeKind.BOOLVAR);
        // boolean operations
        put(LanguageElements.AND, "and", NodeKind.AND.toString(), NodeKind.AND);
        put(LanguageElements.OR, "or", NodeKind.OR.toString(), NodeKind.OR);
        put(LanguageElements.NOT, "not", NodeKind.NOT.toString(), NodeKind.NOT);
        put(LanguageElements.IMPLIES, "implies", NodeKind.IMPLIES.toString(),
                NodeKind.IMPLIES);
        // if-then-else
        put(LanguageElements.ITE, "ite", NodeKind.ITE.toString(), NodeKind.ITE);
        // division
        put(LanguageElements.DIV, "div", NodeKind.DIV.toString(), NodeKind.DIV);
    }};

    final static Logger LOGGER = LoggerFactory.getLogger(Z3Str2Listener.class);

    public Z3Str2Listener() {

    }

    @Override
    public DefaultListener getListener() {
        return this;
    }

    @Override
    public ConstraintNetwork getConstraintNetwork() throws EUFInconsistencyException {
        Ast ast = this.getAst();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,tm);
        try {
            return builder.process().getConstraintNetwork();
        } catch (AstProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }

    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() throws EUFInconsistencyException {
        Ast ast = this.getAst();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,tm);
        try {
            return builder.process();
        } catch (AstProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }
}
