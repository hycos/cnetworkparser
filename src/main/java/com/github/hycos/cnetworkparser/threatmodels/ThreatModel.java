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

import com.github.hycos.cnetwork.api.NodeKindFactoryInterface;
import com.github.hycos.cnetwork.api.NodeKindInterface;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;

import java.util.HashMap;
import java.util.Map;


public abstract class ThreatModel {

    protected HashMap<String, ThreatModel> tmodel = null;
    protected NodeKindFactoryInterface ni = null;

    public ThreatModel(NodeKindFactoryInterface ni) {
        this.ni = ni;
        this.tmodel = new HashMap<> ();
    }
    public abstract Map<String,ThreatModel> getThreatModels();
    public abstract ConstraintNetworkBuilder delegate(NodeKindInterface kind);
}
