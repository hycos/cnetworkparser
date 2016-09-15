package org.snt.cnetworkparser.threatmodels;
import org.snt.cnetwork.core.OperandKind;
import org.snt.cnetworkparser.exception.UnknownException;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.NetworkEntity;

import java.util.HashMap;

public class ThreatModelFactory {

    private static ThreatModelFactory instance = null;

    private HashMap<OperandKind, ThreatModel> tmodel = null;


    public static ThreatModelFactory getInstance() {
        if(instance == null)
            instance = new ThreatModelFactory();
        return instance;
    }

    private ThreatModelFactory() {
        this.tmodel = new HashMap<OperandKind, ThreatModel>();
        this.tmodel.putAll(new Ldapi().getThreatModels());
        this.tmodel.putAll(new Sqli().getThreatModels());
        this.tmodel.putAll(new Xmli().getThreatModels());
        this.tmodel.putAll(new Xpathi().getThreatModels());
        this.tmodel.putAll(new Xss().getThreatModels());
        this.tmodel.putAll(new Urli().getThreatModels());
    }

    public ConstraintNetwork getCNforVulnerability(NetworkEntity.NetworkEntityKind kind) throws UnknownException {

        assert(kind instanceof OperandKind);

        if(!tmodel.containsKey((OperandKind)kind))
            throw new UnknownException("Threat model " + kind + " is not known");


        return tmodel.get(kind).delegate((OperandKind)kind);
    }

}
