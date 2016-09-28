package org.snt.cnetworkparser.lang.smt;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.core.range.BooleanRange;
import org.snt.cnetworkparser.utils.QuadrupleMap;
import org.snt.cnetworkparser.utils.StringUtils;
import org.snt.inmemantlr.tree.Ast;
import org.snt.inmemantlr.tree.AstNode;
import org.snt.inmemantlr.tree.AstProcessor;

import java.util.List;
import java.util.Set;
import java.util.Vector;

public class SmtCnetworkBuilder extends AstProcessor<ConstraintNetwork, Node> {


    final static Logger logger = LoggerFactory.getLogger(SmtCnetworkBuilder.class);

    // data struture that contains the mapping between language elements of
    // source language to Constraint network constructs
    public static class TransMap extends QuadrupleMap<LanguageElements,String,String,NetworkEntity.NetworkEntityKind>{

        public String getNameOfElement(LanguageElements e) {
            return this.getSecondByFirst(e);
        }

        public String getTranslationByElement(LanguageElements e) {
            return this.getThirdByFirst(e);
        }

        public NetworkEntity.NetworkEntityKind  getOperationKindByLabel(String label) {
            return this.getFourthBySecond(label);
        }

    };


    protected static TransMap TRANSMAP;

    protected ConstraintNetwork cn = new ConstraintNetwork();

    public SmtCnetworkBuilder(Ast ast, final TransMap tm) {
        super(ast);
        TRANSMAP = tm;
    }


    /**
     * Preprocesing of Regular expressions (if present)
     */
    private void preprocessRegex() {
        for(Ast subtree : this.ast.getSubtrees(n -> n.getRule().equals("regexoperation"))) {
            SmtRegexBuilder rbuilder = new SmtRegexBuilder(subtree,TRANSMAP);
        }
    }

    @Override
    public ConstraintNetwork process() {

        /**
         * Get regular expression subtrees and translate them
         * into the standard form. Replace the subtree roots
         * with the tranlation.
         */
        Set<Ast> regex = ast.getDominatingSubtrees(n -> n.getRule().equals("regexoperation"));
        for(Ast r : regex) {
            //logger.info(r.toDot());
            SmtRegexBuilder regexbuilder = new SmtRegexBuilder(r,TRANSMAP);
            String sregex = regexbuilder.process();
            //logger.info("REGEX " + sregex);
            // print changed tree
            Ast replacement = new Ast("rlit", sregex);
            ast.replaceSubtree(r, replacement);
        }

        /**
         * get variable declarations
         */
        Set<Ast> vdef = ast.getDominatingSubtrees(n -> n.getRule().matches("(funcdecl|vardecl)"));
        for(Ast r : vdef) {
            assert(r.getRoot().getChildren().size() == 2);

            AstNode varname = r.getRoot().getFirstChild();
            AstNode vartype = r.getRoot().getLastChild();
            OperandKind kind = OperandKind.UNKNOWN;

            //logger.info("KIND " + kind.toString());
            switch (vartype.getLabel()) {
                case "String":
                    kind = OperandKind.STRVAR;
                    break;
                case "Bool":
                    kind = OperandKind.BOOLVAR;
                    break;
                case "Int":
                    kind = OperandKind.NUMVAR;
                    break;
            }
            Operand op = new Operand(varname.getLabel(), kind);
            //logger.info("add op " + op + " " + kind);
            this.cn.addVertex(op);
            ast.removeSubtree(r);
        }

        //logger.info(this.ast.toDot());
        // process the remaining tree and build constraint network
        ConstraintNetwork cn = super.process();
        //logger.info(debug());
        return cn;
    }


    public ConstraintNetwork getResult() {
        return this.cn;
    }

    @Override
    protected void initialize() {

        logger.info(ast.toDot());
        for(AstNode n : this.ast.getNodes()) {
            this.smap.put(n, null);
        }
    }

    @Override
    protected void process(AstNode n) {

        logger.info("ID " + n.getId() + " " + n.getRule() + " " + n.getLabel());
        switch(n.getRule()) {

            case "booloperation":
            case "stroperation":
            case "numoperation":
            case "binoperation":

                logger.info("OP " + n.getId());
                assert(n.hasChildren());
                StringBuilder out = new StringBuilder();

                AstNode fchild = n.getFirstChild();
                assert(fchild.getRule().matches("(boolop|strop|numop|binop)"));

                List<Node> params = new Vector<Node>();

                logger.info("LBL " + fchild.getLabel());

                NetworkEntity.NetworkEntityKind kind = TRANSMAP.getOperationKindByLabel(fchild.getLabel());

                assert(kind != null);
                assert((kind instanceof OperationKind));

                logger.info("FHILD " + fchild.getLabel());

                n.getChildren().stream().filter(c -> !c.equals(fchild)).forEach(
                        e ->  {
                            assert(this.smap.containsKey(e));
                            logger.info("get " + e.getLabel());
                            assert(this.smap.get(e) != null);
                            params.add(this.smap.get(e));
                        }
                );
                Node op = null;


                if(n.getRule().equals("numoperation") && params.size() == 1) {
                    // unary operation
                    assert(fchild.getLabel().matches("(\\-|\\+)"));
                    String no = fchild.getLabel();
                    for(Node p : params) {
                        no += p.getLabel();
                    }
                    op = this.cn.addNode(new Operand(no,OperandKind.NUMLIT));
                    logger.info("add transformed node " + op.getLabel());
                } else {
                    op = this.cn.addOperation((OperationKind)kind, params);
                    logger.info("add Operation " + op.getLabel());
                }

                this.smap.put(n,op);
                break;
            case "boollit":
                Node blit = this.cn.addNode(new Operand(n.getLabel(),OperandKind.BOOLLIT));
                this.smap.put(n, blit);
                break;
            case "varname":
                Node v = this.cn.getOperandByLabel(n.getLabel());
                assert(v != null);
                this.smap.put(n, v);
                break;
            case "rlit":
                //logger.info("rlit " + n.getLabel());
                Node r = this.cn.addNode(new Operand(n.getLabel(),OperandKind.STRREXP));
                this.smap.put(n, r);
                break;
            case "strlit":
                assert(n.getLabel().length() >= 2);
                String lbl = n.getLabel().substring(1,n.getLabel().length()-1);
                lbl = StringUtils.unescapeSpecialCharacters(lbl);
                this.smap.put(n, cn.addNode(new Operand(StringUtils.escapeSpecialCharacters(lbl),OperandKind.STRLIT)));
                break;
            case "number":
                logger.info("nunber "+ n.getLabel());
                Node nlit = this.cn.addNode(new Operand(n.getLabel(),OperandKind.NUMLIT));
                this.smap.put(n, nlit);
                break;
            case "operation":
            case "param":
            case "s":
                simpleProp(n);
                break;
            case "assertion":
                Node c = this.smap.get(n.getFirstChild());
                c.setRange(BooleanRange.TRUE);
                break;
        }
    }


}
