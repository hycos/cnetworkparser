package org.snt.cnetworkparser.core;

import org.snt.inmemantlr.tree.Ast;
import org.snt.inmemantlr.tree.AstNode;
import org.snt.inmemantlr.tree.AstProcessor;


public abstract class RegexAstProcessor
        extends AstProcessor<String,String> {

    public RegexAstProcessor(Ast ast) {
        super(ast);
    }

    @Override
    protected void initialize() {
        for(AstNode n : this.ast.getNodes()) {
            this.smap.put(n, "");
        }
    }

}
