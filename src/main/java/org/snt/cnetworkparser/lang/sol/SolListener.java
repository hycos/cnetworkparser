package org.snt.cnetworkparser.lang.sol;

import dk.brics.automaton.Automaton;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetwork.core.*;
import org.snt.cnetwork.core.range.BooleanRange;
import org.snt.cnetwork.core.range.NumRange;
import org.snt.cnetwork.utils.AutomatonUtils;
import org.snt.cnetworkparser.core.CnetworkProvider;
import org.snt.cnetworkparser.exception.UnknownException;
import org.snt.cnetworkparser.threatmodels.ThreatModelFactory;
import org.snt.cnetworkparser.utils.StringUtils;
import org.snt.inmemantlr.DefaultListener;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class SolListener extends DefaultListener implements CnetworkProvider {

    final static Logger LOGGER = LoggerFactory.getLogger(SolListener.class);

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

        LOGGER.info(">>> " + this.getRuleByKey(ctx.getRuleIndex()) + " " +
                ctx.getText());
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


        //LOGGER.info("CONTEX >>> " +  this.getRuleByKey(ctx.getRuleIndex()));

        //LOGGER.info("+RECENT " + this.ctx.getRecentCtxId());
		switch(this.getRuleByKey(ctx.getRuleIndex())) {
            case "vardecl":
                handleVardecl();
                this.ctx.leaveOldCtx();
                break;
            case "funcdecl":
                handleFuncdecl();
                break;
            case "videntifier":
                //LOGGER.info("-- " + this.ctx.getRecentCtxId());
                if(!this.ctx.getRecentCtxId().equals("vardecl") &&
                        !this.ctx.getRecentCtxId().equals("funcdecl") &&
                        !this.ctx.getRecentCtxId().equals("link")) {
                    handleVidentifer(this.ctx.getRecentCxt().pop().value);
                }
                break;
            case "string":
                handleString(this.ctx.getRecentCxt().pop().value);
                break;
            case "number":
                handleNumber(this.ctx.getRecentCxt().pop().value);
                break;
            case "boollit":
                handleBoollit(this.ctx.getRecentCxt().pop().value);
                break;
            case "funccall":
                handleOp();
                this.ctx.leaveOldCtx();
                break;
            case "constraint":
                OperationKind okind = OperationKind.KindFromString(this.ctx.pop().value);
                constraint.setOpKind(okind);
                handleConstraint();
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
	}

    private void handleVidentifer(String name) {
        //LOGGER.info("get " + name);
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

        //LOGGER.info("Tmodel " + rpoint);
        //LOGGER.info("Label " + label);

        if((n = cn.getNodeByLabel(label.value)) == null) {
            LOGGER.error("Parser Error: cannot link " + label.value + " to a threat model");
            System.exit(-1);
        }

        assert(!n.isNumeric());

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
        //LOGGER.info("ADD VAR " + id +  " " + type);
        this.configReader.addVariable(type.value, id.value);
    }

    private void handleString(String name) {

        String fctx = this.ctx.getRecentCxt().peek().value;

        boolean rexp = false;

        String nname = StringUtils.trimQuotesFromString(name);
        //LOGGER.info("NNAME 1 " + nname);
        nname = nname.replaceAll("\\\\\"","\\\"");
        //LOGGER.info("NNAME 2 " + nname);
        nname = StringUtils.trimQuotesFromString(nname);
        //LOGGER.info("NNAME 3 " + nname);

        //LOGGER.info("FCTX " + fctx);
        if(fctx.equals("tolit")) {
            this.ctx.getRecentCxt().pop();
            nname = AutomatonUtils.escapeSpecialCharacters(nname);
        }

        //LOGGER.info("NAME  " + name);
        //LOGGER.info("NNAME  " + nname);
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
        //LOGGER.info("NUMBER " + name);

        Operand number = this.configReader.getOperandByLabel(name);
        if (number == null) {
            number = new Operand(name, OperandKind.NUMLIT);
            this.configReader.addNode(number);
        }
        this.ctx.push(number);
    }

    private void handleBoollit(String name) {

        //LOGGER.info("BOOL " + name);

        Operand boollit = this.configReader.getOperandByLabel(name);
        if (boollit == null) {
            boollit = new Operand(name, OperandKind.BOOLLIT);

            //LOGGER.info("ADD NODE  " + boollit.toString());
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

        //LOGGER.info("CALLING CTX " + sctx);

        StringPair op = this.ctx.pop(); //op

        //LOGGER.info("OP " + op);

        String type = op.key; // funccall
        String skind = op.value;


        //LOGGER.info("TYPE " + type);
        //LOGGER.info("Kind " + skind);

        String fcall = this.ctx.pop().value; //

        //String fcall = id;

        //LOGGER.info("FCALL " + fcall);
        //LOGGER.info("OP " + op);


        //LOGGER.info("PARAMS " + params.size());

        OperationKind okind = OperationKind.KindFromString(skind);

        //LOGGER.info("OKIND " + okind);
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
        constraint.addNode(nods.get(0));
        constraint.addNode(nods.get(1));
        assert(constraint.term1.isLiteral());
        ConstraintNetwork cn = this.configReader.getCnetwork();
        Operation op = cn.addOperation(OperationKind.ASSIGNMENT, nods.get(0), nods.get(1));
        op.setRange(BooleanRange.TRUE.clone());
        return op;
    }


    private Operation handleConstraint() {
        //LOGGER.info("HANDLE " + this.ctx.getRecentCxt().getName());
        List<Node> nods = this.ctx.getNodesForCtx();
        //LOGGER.info("SS " + nods.size());
        //assert(nods.size() == 2);

        if(nods.size() == 2) {
            constraint.addNode(nods.get(0));
            constraint.addNode(nods.get(1));

            boolean negate = (constraint.opKind == OperationKind.NEQUALS);
            //LOGGER.debug("+++" + constraint.term0.getKind() + " " + constraint
            //        .term1.getKind());

            //LOGGER.info("OPKIND " + constraint.opKind.getId());
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
            assert constraint.opKind != null;

            Operation c = this.configReader.addConstraint(constraint);

            if(negate) {
                c.setRange(new BooleanRange(BooleanRange.BooleanValue.FALSE));
            }

            return c;

        } else if (nods.size() == 1) {
            switch (constraint.opKind) {
                case EMTPY: return this.configReader.getCnetwork().addConstraint(OperationKind.EMTPY, nods.get(0));
                case NOT: return this.configReader.getCnetwork()
                        .addConstraint(OperationKind.NOT, nods.get(0));
            }
        }

        LOGGER.info("KKK 1" + constraint.toString());

        assert false;
        return null;
    }

}