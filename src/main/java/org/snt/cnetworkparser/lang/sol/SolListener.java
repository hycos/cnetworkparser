package org.snt.cnetworkparser.lang.sol;

import java.util.*;

import dk.brics.automaton.Automaton;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetworkparser.core.CnetworkProvider;
import org.snt.cnetworkparser.threatmodels.ThreatModelFactory;
import org.snt.cnetwork.core.range.BooleanRange;
import org.snt.cnetwork.core.range.NumRange;
import org.snt.inmemantlr.DefaultListener;
import org.snt.cnetworkparser.exception.UnknownException;
import org.snt.cnetworkparser.utils.StringUtils;
import org.snt.cnetwork.utils.AutomatonUtils;

public class SolListener extends DefaultListener implements CnetworkProvider {

    final static Logger logger = LoggerFactory.getLogger(SolListener.class);

	private Map<String,Integer> rmap;

    private BasicConstraint constraint = new BasicConstraint();
    private ScopeMgr ctx = new ScopeMgr();

    private ConfigReader configReader = null;

	public SolListener() {
        this.configReader = new ConfigReader();
	}


    public ConfigReader getConfigReader() {
        return this.configReader;
    }

    @Override
    public DefaultListener getListener() {
        return this;
    }

    @Override
    public ConstraintNetwork getConstraintNetwork() {
        return this.configReader.getCnetwork();
    }


    @Override
    public void visitTerminal(TerminalNode terminalNode) {}

    @Override
    public void visitErrorNode(ErrorNode errorNode) {}

    @Override
	public void enterEveryRule(ParserRuleContext ctx) {

        //logger.info(">>> " + this.getRuleByKey(ctx.getRuleIndex()));
		switch(this.getRuleByKey(ctx.getRuleIndex())) {
            case "s":
                this.ctx.push(this.getRuleByKey(ctx.getRuleIndex()), ctx.getText());
                break;

            case "searchop":
            case "constraint":
                constraint.clear();

            case "funccall":
            case "vardecl":
            case "funcdecl":
            case "assignment":
            case "link":
                this.ctx.enterNewCtx();

            case "string":
            case "number":
            case "boollit":
            case "vartype":
            case "videntifier":
            case "op":
            case "boolop":
            case "esc":
            case "comp":
            case "userdef":
            case "bytecodesig":
            case "tmodeltype":
            case "trans":
                this.ctx.push(this.getRuleByKey(ctx.getRuleIndex()), ctx.getText());
                break;

		}
	}


    @Override
	public void exitEveryRule(ParserRuleContext ctx) {


        //logger.info("CONTEX >>> " +  this.getRuleByKey(ctx.getRuleIndex()));

        //logger.info("+RECENT " + this.ctx.getRecentCtxId());
		switch(this.getRuleByKey(ctx.getRuleIndex())) {
            case "vardecl":
                handleVardecl();
                this.ctx.leaveOldCtx();
                break;
            case "funcdecl":
                handleFuncdecl();
                break;
            case "videntifier":
                //logger.info("-- " + this.ctx.getRecentCtxId());
                if(!this.ctx.getRecentCtxId().equals("vardecl") &&
                        !this.ctx.getRecentCtxId().equals("funcdecl") &&
                        !this.ctx.getRecentCtxId().equals("link")) {
                    handleVidentifer(this.ctx.getRecentCxt().pop().value);
                }
                break;
            case "string":
                //logger.info( "SSSS " + this.ctx.getRecentCxt().peek());
                handleString(this.ctx.getRecentCxt().pop().value);
                break;
            case "number":
                //logger.info(" NNNN " + this.ctx.getRecentCxt().peek());
                handleNumber(this.ctx.getRecentCxt().pop().value);
                break;
            case "boollit":
                //logger.info(" BBBB " + this.ctx.getRecentCxt().peek());
                handleBoollit(this.ctx.getRecentCxt().pop().value);
                break;
            case "funccall":
                handleOp();
                this.ctx.leaveOldCtx();
                break;
            case "constraint":
                OperationKind okind = OperationKind.KindFromString(this.ctx.pop().value);
                constraint.setOpKind(okind);
                Operation constraint = handleConstraint();


                //logger.info("OPNODE " + constraint.getLabel());
                this.ctx.leaveOldCtx();
                break;
            case "assignment":
                handleAssignment();
                this.ctx.leaveOldCtx();
                break;
            case "link":
                handleLink();
                this.ctx.leaveOldCtx();
                break;
		}

        //logger.info("CONTEXT  " + this.ctx.size());

	}

    private void handleVidentifer(String name) {
        //logger.info("get " + name);
        Operand op  = this.configReader.getOperandByLabel(name);
        assert(op != null);
        this.ctx.push(op);
    }


    private void handleLink() {
        RuleScope cctx = this.ctx.getRecentCxt();
        StringPair rpoint = cctx.pop(); // videntifier
        StringPair label = cctx.pop(); // vatype
        Node n = null;

        ConstraintNetwork cn = this.configReader.getCnetwork();

        //logger.info("Tmodel " + rpoint);
        //logger.info("Label " + label);

        if((n = cn.getNodeByLabel(label.value)) == null) {
            logger.error("Parser Error: cannot link " + label.value + " to a threat model");
            System.exit(-1);
        }

        assert(!n.isNumeric());

        //Set<Edge> cons = cn.getAllConnectedEdges(n);
        // hash value changed
        //cn.removeNode(n);
        //n.setKind(OperandKind.KindFromString(rpoint.value));
        //cn.addNode(n);

        OperandKind tmodeltype = OperandKind.KindFromString(rpoint.value);

        ConstraintNetwork subnet = null;

        try {
            subnet = ThreatModelFactory.getInstance().getCNforVulnerability(tmodeltype);
        } catch (UnknownException e) {
            e.printStackTrace();
        }

        cn.join(OperationKind.MATCHES, n, subnet);
        //cn.addConstraint(OperationKind.MATCHES, n, subnet.getStartNode());

        // join threat model subnetwork with reference point
    }


    private void handleFuncdecl() {
        RuleScope cctx = this.ctx.getRecentCxt();
        StringPair id = cctx.pop(); // fidentifier
        StringPair bsig = cctx.pop(); // bsig
        this.configReader.getCnetwork().registerExtOperation(StringUtils.trimQuotesFromString(bsig.value),
                    StringUtils.trimQuotesFromString(id.value));

    }

    private void handleVardecl() {
        RuleScope cctx = this.ctx.getRecentCxt();
        StringPair id = cctx.pop(); // videntifier
        StringPair type = cctx.pop(); // vatype
        //logger.info("ADD VAR " + id +  " " + type);
        this.configReader.addVariable(type.value, id.value);
    }

    private void handleString(String name) {

        String fctx = this.ctx.getRecentCxt().peek().value;

        boolean rexp = false;

        String nname = StringUtils.trimQuotesFromString(name);
        //logger.info("NNAME 1 " + nname);
        nname = nname.replaceAll("\\\\\"","\\\"");
        //logger.info("NNAME 2 " + nname);
        nname = StringUtils.trimQuotesFromString(nname);
        //logger.info("NNAME 3 " + nname);

        //logger.info("FCTX " + fctx);
        if(fctx.equals("tolit")) {
            this.ctx.getRecentCxt().pop();
            nname = AutomatonUtils.escapeSpecialCharacters(nname);
        }

        //logger.info("NAME  " + name);
        //logger.info("NNAME  " + nname);
        Operand string = this.configReader.getOperandByLabel(name);


        if(string == null) {
            Automaton a = new dk.brics.automaton.RegExp(nname).toAutomaton();

            if(a.getSingleton() != null) {
                string = new Operand(nname, OperandKind.STRLIT);
            } else {
                string = new Operand(nname, OperandKind.STRREXP);
            }
            string.setAutomaton(a);
            string.setRange(new NumRange(AutomatonUtils.getApproxLenRange(a)));
            this.configReader.addNode(string);
        }

        this.ctx.push(string);

    }

    private void handleNumber(String name) {
        //logger.info("NUMBER " + name);

        Operand number = this.configReader.getOperandByLabel(name);
        if (number == null) {
            number = new Operand(name, OperandKind.NUMLIT);
            this.configReader.addNode(number);
        }
        this.ctx.push(number);
    }

    private void handleBoollit(String name) {

        //logger.info("BOOL " + name);

        Operand boollit = this.configReader.getOperandByLabel(name);
        if (boollit == null) {
            boollit = new Operand(name, OperandKind.BOOLLIT);

            //logger.info("ADD NODE  " + boollit.toString());
            this.configReader.addNode(boollit);
        }
        this.ctx.push(boollit);

    }

    public boolean areString(Collection<Node> nset) {
        for(Node n : nset) {
            if(n.isString())
                continue;

            return false;
        }
        return true;
    }

    public boolean areBool(Collection<Node> nset) {
        for(Node n : nset) {
            if(n.isBoolean())
                continue;

            return false;
        }
        return true;
    }


    public boolean areNum(Collection<Node> nset) {
        for(Node n : nset) {
            if(n.isNumeric())
                continue;

            return false;
        }
        return true;
    }

    private void handleOp() {

        LinkedList<Node> params = this.ctx.getNodesForCtx();

        String sctx = this.ctx.getRecentCxt().getBackref().getName();

        //logger.info("CALLING CTX " + sctx);

        StringPair op = this.ctx.pop(); //op

        //logger.info("OP " + op);

        String type = op.key; // funccall
        String skind = op.value;


        //logger.info("TYPE " + type);
        //logger.info("Kind " + skind);

        String fcall = this.ctx.pop().value; //

        //String fcall = id;

        //logger.info("FCALL " + fcall);
        //logger.info("OP " + op);


        //logger.info("PARAMS " + params.size());

        OperationKind okind = OperationKind.KindFromString(skind);

        //logger.info("OKIND " + okind);
        Node newop = null;

        // infer the types for overloaded operators (==)

        if(okind == OperationKind.EQUALS){
            if(areString(params))
                okind = OperationKind.STR_EQUALS;
            else if(areNum(params))
                okind = OperationKind.NUM_EQUALS;
            else if(areBool(params))
                okind = OperationKind.BOOL_EQUALS;
        } else if (okind == OperationKind.NEQUALS){
            if(areString(params))
                okind = OperationKind.STR_NEQUALS;
            else if(areNum(params))
                okind = OperationKind.NUM_NEQUALS;
            else if(areBool(params))
                okind = OperationKind.BOOL_NEQUALS;
        }

        if(okind == OperationKind.UNKNOWN) {
            newop = this.configReader.getCnetwork().addExtOperation(skind,params);
        } else {
            newop = this.configReader.getCnetwork().addOperation(okind,params);
        }




        // safe node (context can be either term or funccall
        this.ctx.getRecentCxt().getBackref().addNode(newop);
    }


    private Operation handleAssignment() {
        List<Node> nods = this.ctx.getNodesForCtx();
        assert(nods.size() == 2);
        constraint.addNode(nods.get(1));
        constraint.addNode(nods.get(0));
        assert(constraint.term1.isLiteral());
        ConstraintNetwork cn = this.configReader.getCnetwork();
        Operation op = cn.addOperation(OperationKind.ASSIGNMENT, nods.get(0), nods.get(1));
        op.setRange(BooleanRange.TRUE.clone());
        return op;
    }


    private Operation handleConstraint() {
        //logger.info("HANDLE " + this.ctx.getRecentCxt().getName());
        List<Node> nods = this.ctx.getNodesForCtx();
        //logger.info("SS " + nods.size());
        //assert(nods.size() == 2);

        if(nods.size() == 2) {
            constraint.addNode(nods.get(0));
            constraint.addNode(nods.get(1));

            boolean negate = (constraint.opKind == OperationKind.NEQUALS);

            //logger.info("+++" + constraint.term0.getKind() + " " + constraint.term1.getKind());
            //logger.info("KKK " + constraint.toString());
            //logger.info("OPKIND " + constraint.opKind.getId());
            // type inference
            if(constraint.opKind == OperationKind.EQUALS || negate) {
                if(constraint.isNumeric()) {
                    constraint.setOpKind(OperationKind.NUM_EQUALS);
                } else if (constraint.isString()) {
                    constraint.setOpKind(OperationKind.STR_EQUALS);
                } else if (constraint.isBoolean()) {
                    constraint.setOpKind(OperationKind.BOOL_EQUALS);
                } else {
                    assert(false);
                }
            }
            assert(constraint.opKind != null);
            Operation c = this.configReader.addConstraint(constraint);
            if(negate)
                c.setRange(new BooleanRange(BooleanRange.BooleanValue.FALSE));
            return c;
        } else if (nods.size() == 1) {

            switch (constraint.opKind) {
                case EMTPY: return this.configReader.getCnetwork().addConstraint(OperationKind.EMTPY, nods.get(0));
            }

        }
        assert(false);
        return null;

    }

}