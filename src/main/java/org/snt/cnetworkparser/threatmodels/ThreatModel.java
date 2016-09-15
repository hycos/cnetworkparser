package org.snt.cnetworkparser.threatmodels;

import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.OperandKind;

import java.util.HashMap;
import java.util.Map;


public abstract class ThreatModel {

    public HashMap<OperandKind, ThreatModel> tmodel = null;
    public ThreatModel() {
        this.tmodel = new HashMap<OperandKind, ThreatModel> ();
    }
    public abstract Map<OperandKind,ThreatModel> getThreatModels();
    public abstract ConstraintNetwork delegate(OperandKind kind);
}
