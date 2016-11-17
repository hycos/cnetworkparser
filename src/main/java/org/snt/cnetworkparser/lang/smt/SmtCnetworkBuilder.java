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


    final static Logger LOGGER = LoggerFactory.getLogger(SmtCnetworkBuilder.class);

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
            //LOGGER.info(r.toDot());
            SmtRegexBuilder regexbuilder = new SmtRegexBuilder(r,TRANSMAP);
            String sregex = regexbuilder.process();
            //LOGGER.info("REGEX " + sregex);
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

            //LOGGER.info("KIND " + kind.toString());
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
            //LOGGER.info("add op " + op + " " + kind);
            this.cn.addVertex(op);
            ast.removeSubtree(r);
        }

        //LOGGER.info(this.ast.toDot());
        // process the remaining tree and build constraint network
        ConstraintNetwork cn = super.process();
        //LOGGER.info(debug());
        return cn;
    }


    public ConstraintNetwork getResult() {
        return this.cn;
    }

    @Override
    protected void initialize() {

        LOGGER.info(ast.toDot());
        for(AstNode n : this.ast.getNodes()) {
            this.smap.put(n, null);
        }
    }

    @Override
    protected void process(AstNode n) {

        LOGGER.info("ID " + n.getId() + " " + n.getRule() + " " + n.getLabel());

        switch(n.getRule()) {

            case "booloperation":
            case "stroperation":
            case "numoperation":
            case "binoperation":

                LOGGER.info("OP " + n.getId());
                assert(n.hasChildren());
                StringBuilder out = new StringBuilder();

                AstNode fchild = n.getFirstChild();
                assert(fchild.getRule().matches("(boolop|strop|numop|binop)"));

                List<Node> params = new Vector<Node>();

                LOGGER.info("LBL " + fchild.getLabel());

                NetworkEntity.NetworkEntityKind kind = TRANSMAP.getOperationKindByLabel(fchild.getLabel());

                assert(kind != null);
                assert((kind instanceof OperationKind));

                LOGGER.info("FHILD " + fchild.getLabel());

                n.getChildren().stream().filter(c -> !c.equals(fchild)).forEach(
                        e ->  {
                            assert(this.smap.containsKey(e));
                            LOGGER.info("get " + e.getLabel());
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
                    LOGGER.info("add transformed node " + op.getLabel());
                } else {
                    op = this.cn.addOperation((OperationKind)kind, params);
                    LOGGER.info("add Operation " + op.getLabel());
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
                //LOGGER.info("rlit " + n.getLabel());
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
                LOGGER.info("nunber "+ n.getLabel());
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

                assert((c instanceof Operation) || (c instanceof Operand));
                Operand top = new Operand("true", OperandKind.BOOLLIT);
                Node constraint = cn.addConstraint(OperationKind.BOOL_EQUALS, top, c);

                this.smap.put(n, constraint);
                break;
            case "copoperation":
                NetworkEntity.NetworkEntityKind copkind = TRANSMAP
                        .getOperationKindByLabel(n.getFirstChild().getLabel());

                if(copkind == OperationKind.ITE) {

                    assert n.getChildren().size() == 4;

                    Node par1 = this.smap.get(n.getChild(1));
                    Node par2 = this.smap.get(n.getChild(2));
                    Node par3 = this.smap.get(n.getChild(3));

                    assert (par1.isNumeric() && par2.isNumeric()) ||
                            (par1.isString() && par2.isString()) ||
                            (par1.isBoolean() && par2.isBoolean());

                    assert par2.isBoolean();

                    Operation parop = null;

                    if (par1.isOperation())
                        parop = (Operation) par1;
                    else if (par2.isOperation())
                        parop = (Operation) par2;

                    Operation ite = cn.addOperation(OperationKind.ITE,
                            par1, par2, par3);

                    if (parop != null) {
                        OperationReturnType ret = ((Operation) par1).getKind
                                ().getReturnType();
                        ite.getKind().setReturnType(ret);
                        ite.init();
                    } else {
                        if (par1.isBoolean()) {
                            ite.getKind().setReturnType(OperationReturnType
                                    .BOOLEAN);
                        } else if (par1.isString()) {
                            ite.getKind().setReturnType(OperationReturnType
                                    .STRING);
                        } else if (par1.isNumeric()) {
                            ite.getKind().setReturnType(OperationReturnType
                                    .NUMERIC_N);
                        }
                    }

                    this.smap.put(n,ite);
                }


        }
    }


}
