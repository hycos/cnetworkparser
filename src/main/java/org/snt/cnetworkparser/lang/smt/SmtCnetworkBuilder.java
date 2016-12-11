package org.snt.cnetworkparser.lang.smt;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.core.domain.DomainKind;
import org.snt.cnetwork.core.domain.NodeDomain;
import org.snt.cnetwork.core.domain.NodeDomainFactory;
import org.snt.cnetwork.exception.IllegalDomainException;
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
    public static class TransMap extends QuadrupleMap<LanguageElements,
            String,String,NodeKind>{

        public String getNameOfElement(LanguageElements e) {
            return this.getSecondByFirst(e);
        }

        public String getTranslationByElement(LanguageElements e) {
            return this.getThirdByFirst(e);
        }

        public NodeKind getNodeKindByLabel(String label) {
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
            NodeKind kind = NodeKind.UNKNOWN;

            //LOGGER.info("KIND " + kind.toString());
            switch (vartype.getLabel()) {
                case "String":
                    kind = NodeKind.STRVAR;
                    break;
                case "Bool":
                    kind = NodeKind.BOOLVAR;
                    break;
                case "Int":
                    kind = NodeKind.NUMVAR;
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

                NodeKind kind = TRANSMAP.getNodeKindByLabel(fchild.getLabel());

                assert(kind != null);
                assert((kind instanceof NodeKind));

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
                    op = this.cn.addNode(new Operand(no,NodeKind.NUMLIT));
                    LOGGER.info("add transformed node " + op.getLabel());
                } else {
                    // sometimes the indexof operator assumes a startin index
                    // of 0 implicitly
                    if(kind == NodeKind.INDEXOF && params.size() == 2) {
                        params.add(new Operand("0", NodeKind.NUMLIT));
                    }

                    op = this.cn.addOperation((NodeKind)kind, params);
                    LOGGER.info("add Operation " + op.getLabel());
                }

                this.smap.put(n,op);
                break;
            case "boollit":
                Node blit = this.cn.addNode(new Operand(n.getLabel(),NodeKind.BOOLLIT));
                this.smap.put(n, blit);
                break;
            case "varname":
                Node v = this.cn.getOperandByLabel(n.getLabel());
                assert(v != null);
                this.smap.put(n, v);
                break;
            case "rlit":
                //LOGGER.info("rlit " + n.getLabel());
                Node r = this.cn.addNode(new Operand(n.getLabel(),NodeKind.STRREXP));
                this.smap.put(n, r);
                break;
            case "strlit":
                assert(n.getLabel().length() >= 2);
                String lbl = n.getLabel().substring(1,n.getLabel().length()-1);
                lbl = StringUtils.unescapeSpecialCharacters(lbl);
                this.smap.put(n, cn.addNode(new Operand(StringUtils.escapeSpecialCharacters(lbl),NodeKind.STRLIT)));
                break;
            case "number":
                LOGGER.info("nunber "+ n.getLabel());
                Node nlit = this.cn.addNode(new Operand(n.getLabel(),NodeKind.NUMLIT));
                this.smap.put(n, nlit);
                break;
            case "operation":
            case "param":
            case "s":
                simpleProp(n);
                break;
            case "assertion":
                Node c = this.smap.get(n.getFirstChild());
                // ite is a special case ... the return value remains
                // parametrized
                //if(c.getKind() != NodeKind.ITE) {
                    c.setDomain(NodeDomainFactory.INSTANCE.getDomain
                            (NodeKind.BOOLLIT, "true"));

                    assert((c instanceof Operation) || (c instanceof Operand));
                    Operand top = new Operand("true", NodeKind.BOOLLIT);

                    // if then else is an exception the outcome of ITE is
                    // determined by the predicate
                    LOGGER.debug("ASSERTION " + c.getLabel());

                    Node constraint = cn.addConstraint(NodeKind.BOOL_EQUALS, top, c);
                    this.smap.put(n, constraint);
                //} else {
                //    simpleProp(n);
                //}
                break;
            case "copoperation":
                NodeKind copkind = TRANSMAP
                        .getNodeKindByLabel(n.getFirstChild().getLabel());

                if(copkind == NodeKind.ITE) {

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

                    Operation ite = cn.addOperation(NodeKind.ITE,
                            par1, par2, par3);

                    if (parop != null) {
                        DomainKind ret = ((Operation) parop).getKind
                                ().getDomainKind();
                        alterDomain(ite, ret);
                    } else {
                        if (par1.isBoolean()) {
                            alterDomain(ite, DomainKind.BOOLEAN);
                        } else if (par1.isString()) {
                            alterDomain(ite, DomainKind.STRING);
                        } else if (par1.isNumeric()) {
                            alterDomain(ite, DomainKind.NUMERIC_N);
                        }
                    }

                    this.smap.put(n,ite);
                }


        }
    }

    private void alterDomain(Node n, DomainKind newKind) {
        NodeKind kind = n.getKind();
        try {
            kind.setDomainKind(newKind);
        } catch (IllegalDomainException e) {
            LOGGER.error(e.getMessage());
            System.exit(-1);
        }
        NodeDomain dkind = NodeDomainFactory.INSTANCE.getDomain
                (kind);
        dkind.setDomain(dkind);
    }


}
