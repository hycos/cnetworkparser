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
import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

public class Sqli extends ThreatModel {


    final static Logger LOGGER = LoggerFactory.getLogger(Sqli.class);

    final static String cdomain = "[\\x00-\\x1F\\x80-\\x9F]";
    //final static String cdomain = "[a-z]";

    public Sqli(NodeKindFactoryInterface ni) {
        super(ni);
        tmodel.put("sqlinum", this);
        tmodel.put("sqlistr", this);
    }

    @Override
    public ConstraintNetworkBuilder delegate(NodeKindInterface type) {
        switch (type.getValue().toUpperCase()) {
            case "SQLINUM":
                try {
                    return getNumTautology();
                } catch (InconsistencyException e) {
                    assert false;
                }
            case "SQLISTR":
                try {
                    return getStrTautology();
                } catch (InconsistencyException e) {
                    assert false;
                }
        }
        return null;
    }

    @Override
    public Map<String, ThreatModel> getThreatModels() {
        return this.tmodel;
    }


    private ConstraintNetworkBuilder getStrTautology() throws InconsistencyException {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        Node or = cn.addOperand(
                ni.getNodeKindFromString("strexp"),".*' +[Oo][Rr] +'");
        Node v1 = cn.addOperand(ni.getNodeKindFromString("strvar"),"sv1");
        Node orv1 = cn.addOperation(ni.getNodeKindFromString("concat"), or,
                v1);
        Node eq = cn.addOperand(ni.getNodeKindFromString("strexp"),"'.*=.*'");
        Node v2 = cn.addOperand(ni.getNodeKindFromString("strvar"),"sv2");
        Node orv1comp = cn.addOperation(ni.getNodeKindFromString("concat"),
                eq, v2);
        Node orv1compv2 = cn.addOperation(ni.getNodeKindFromString("concat"),
                orv1, orv1comp);
        cn.addConstraint(ni.getNodeKindFromString("=="), v1, v2);
        String scomment = "' *(\\-\\-|#)";
        Node comment = cn.addOperand(ni.getNodeKindFromString("strexp"),
                scomment);
        Node start = cn.addOperation(ni.getNodeKindFromString("concat"),
                orv1compv2, comment);
        cn.setStartNode(start);
        return cn;
    }

    private ConstraintNetworkBuilder getNumTautology() throws InconsistencyException {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        String sor = "[0-9]+ +[Oo][Rr] +";

        LOGGER.debug("BOO " + ni.getNodeKindFromString("strexp"));
        Node or = cn.addOperand(ni.getNodeKindFromString("strexp"),sor);
        Node v1 = cn.addOperand(ni.getNodeKindFromString("numvar"),"sv7");
        Node toStrV1 = cn.addOperation(ni.getNodeKindFromString("tostr"), v1);
        Node orv1 = cn.addOperation(ni.getNodeKindFromString("concat"), or, toStrV1);
        Node eq = cn.addOperand(ni.getNodeKindFromString("strexp")," +\\>= +");
        Node orv1comp = cn.addOperation(ni.getNodeKindFromString("concat"), orv1, eq);
        Node v2 = cn.addOperand( ni.getNodeKindFromString("numvar"),"sv8");
        Node toStrV2 = cn.addOperation(ni.getNodeKindFromString("tostr"), v2);
        Node orv1compv2 = cn.addOperation(ni.getNodeKindFromString("concat"), orv1comp, toStrV2);
        cn.addConstraint(ni.getNodeKindFromString(">="), v1, v2);
        cn.setStartNode(orv1compv2);
        return cn;

    }


}
