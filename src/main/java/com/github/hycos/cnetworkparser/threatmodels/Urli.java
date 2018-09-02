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
import com.github.hycos.cnetwork.core.graph.Node;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

public class Urli extends ThreatModel {

    final static Logger LOGGER = LoggerFactory.getLogger(Urli.class);

    private static String urlBlacklist = "[a-zA-Z0-9]+=[a-zA-Z0-9]+(&[a-zA-Z0-9]+=[a-zA-Z0-9])*";

    public Urli(NodeKindFactoryInterface ni) {
        super(ni);
        tmodel.put("urli", this);
    }

    @Override
    public ConstraintNetworkBuilder delegate(NodeKindInterface type) {
        switch(type.getValue().toUpperCase()) {
            case "URLI":
                return getURLiThreatModel();
        }
        return null;
    }

    @Override
    public Map<String, ThreatModel> getThreatModels() {
        return this.tmodel;
    }

    private ConstraintNetworkBuilder getURLiThreatModel() {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        cn.addOperand(ni.getNodeKindFromString("strexp"), urlBlacklist);
        Node op = cn.addOperand(ni.getNodeKindFromString("strexp"),urlBlacklist);
        cn.setStartNode(op);
        return cn;
    }

}
