/*
 * cnetworkparser - generate constraint network from different formats
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetworkparser is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetworkparser.threatmodels;

import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetworkparser.exception.UnknownException;

import java.util.HashMap;

public class ThreatModelFactory {

    private static ThreatModelFactory instance = null;

    private HashMap<NodeKind, ThreatModel> tmodel = null;


    public static ThreatModelFactory getInstance() {
        if(instance == null)
            instance = new ThreatModelFactory();
        return instance;
    }

    private ThreatModelFactory() {
        this.tmodel = new HashMap<>();
        this.tmodel.putAll(new Ldapi().getThreatModels());
        this.tmodel.putAll(new Sqli().getThreatModels());
        this.tmodel.putAll(new Xmli().getThreatModels());
        this.tmodel.putAll(new Xpathi().getThreatModels());
        this.tmodel.putAll(new Xss().getThreatModels());
        this.tmodel.putAll(new Urli().getThreatModels());
    }

    public ConstraintNetworkBuilder getCNforVulnerability(NodeKind kind) throws
            UnknownException {

        if(!tmodel.containsKey(kind))
            throw new UnknownException("Threat model " + kind + " is not known");


        return tmodel.get(kind).delegate(kind);
    }

}
