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


public class CVC4Listener extends ConstraintNetworkCreator {

    final static Logger LOGGER = LoggerFactory.getLogger(CVC4Listener.class);

    public static final SmtCnetworkBuilder.TransMap tm = new SmtCnetworkBuilder.TransMap() {{
        // for regular expressions
        put(LanguageElements.KSTAR, "re.*", "*", NodeKind.UNKNOWN);
        put(LanguageElements.KPLUS, "re.+", "+", NodeKind.UNKNOWN);
        put(LanguageElements.UNION, "re.union", "|", NodeKind.UNKNOWN);
        put(LanguageElements.RCONCAT, "re.++", "", NodeKind.CONCAT);
        put(LanguageElements.OPT, "re.opt", "?", NodeKind.UNKNOWN);
        put(LanguageElements.RAN, "re.range", "", NodeKind.UNKNOWN);
        put(LanguageElements.CONV, "str.to.re", "", NodeKind.UNKNOWN);
        put(LanguageElements.MATCHES, "str.in.re", NodeKind.MATCHES.getValue(), NodeKind.MATCHES);
        // for string operations
        put(LanguageElements.LEN, "str.len", NodeKind.LEN.getValue(), NodeKind.LEN);
        put(LanguageElements.CONTAINS, "str.contains", NodeKind.CONTAINS.getValue(), NodeKind.CONTAINS);
        put(LanguageElements.INDEXOF, "str.indexof", NodeKind.INDEXOF.getValue(), NodeKind.INDEXOF);
        put(LanguageElements.PREFIXOF, "str.prefixof", NodeKind.STARTSWITH.getValue(), NodeKind.STARTSWITH);
        put(LanguageElements.SUFFIXOF, "str.suffixof", NodeKind.ENDSWITH.getValue(), NodeKind.ENDSWITH);
        put(LanguageElements.SCONCAT, "str.++", NodeKind.CONCAT.getValue(), NodeKind.CONCAT);
        put(LanguageElements.SUBSTRNG, "str.substr", NodeKind.SUBSTR.getValue(), NodeKind.SUBSTR);
        // others
        put(LanguageElements.NEQ, "!=", NodeKind.NEQUALS.toString(), NodeKind.NEQUALS);
        put(LanguageElements.EQ, "=", NodeKind.EQUALS.toString(), NodeKind.EQUALS);
        put(LanguageElements.SMALLEREQ, "<=", NodeKind.SMALLEREQ.toString(), NodeKind.SMALLEREQ);
        put(LanguageElements.GREATEREQ, ">=", NodeKind.GREATEREQ.toString(), NodeKind.GREATEREQ);
        put(LanguageElements.SMALLER, "<", NodeKind.SMALLER.toString(), NodeKind.SMALLER);
        put(LanguageElements.GREATER, ">", NodeKind.GREATER.toString(), NodeKind.GREATER);
        put(LanguageElements.PLUS, "+", NodeKind.ADD.toString(), NodeKind.ADD);
        put(LanguageElements.MINUS, "-", NodeKind.SUB.toString(), NodeKind.SUB);
        //v ariables
        put(LanguageElements.DTSTRING, "String", NodeKind.STRVAR.toString(), NodeKind.STRVAR);
        put(LanguageElements.DTINT, "Int", NodeKind.NUMVAR.toString(), NodeKind.NUMVAR);
        put(LanguageElements.DTBOOLEAN, "Bool", NodeKind.BOOLVAR.toString(), NodeKind.BOOLVAR);
        // boolean operations
        put(LanguageElements.AND, "and", NodeKind.AND.toString(), NodeKind.AND);
        put(LanguageElements.OR, "or", NodeKind.OR.toString(), NodeKind.OR);
        put(LanguageElements.NOT, "not", NodeKind.NOT.toString(), NodeKind.NOT);
    }};


    public CVC4Listener(boolean eufEnabled) {
        super(eufEnabled);
    }

    @Override
    public DefaultListener getListener() {
        return this;
    }


    @Override
    public ConstraintNetwork getConstraintNetwork() throws EUFInconsistencyException {
        Ast ast = this.getAst();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,eufEnabled,tm);
        try {
            return builder.process().getConstraintNetwork();
        } catch (AstProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }

    @Override
    public ConstraintNetworkBuilder getConstraintNetworkBuilder() throws EUFInconsistencyException {
        Ast ast = this.getAst();
        SmtCnetworkBuilder builder = new SmtCnetworkBuilder(ast,eufEnabled,tm);
        try {
            return builder.process();
        } catch (AstProcessorException e) {
            throw new EUFInconsistencyException(e.getMessage());
        }
    }
}
