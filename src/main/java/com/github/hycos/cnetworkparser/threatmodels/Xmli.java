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

public class Xmli extends ThreatModel {


    private static String xmlInjection = ".*(\\<((! *- *-)?|( *- *-)?\\>)|\\< *CDATA\\[\\[.*\\]\\] *\\>).*";


    final static Logger LOGGER = LoggerFactory.getLogger(Xmli.class);

    public Xmli(NodeKindFactoryInterface ni) {
        super(ni);
        tmodel.put("xmli", this);
    }


    @Override
    public ConstraintNetworkBuilder delegate(NodeKindInterface type) {
        switch (type.getValue().toUpperCase()) {
            case "XMLI":
                try {
                    return getXMLIThreatModel();
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


    private ConstraintNetworkBuilder getXMLIThreatModel()
            throws InconsistencyException {

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();


        Node strvar = cn.addOperand(ni.getNodeKindFromString("strvar"),"sv1");
        //Node op = new Operand(xmlInjection, NodeKind.STREXP);

        //Node matches1 = cn.addOperation(NodeKind.MATCHES, strvar, op);

        Node content = cn.addOperand(ni.getNodeKindFromString("strvar"),"content");


        Node startag = cn.addOperand(ni.getNodeKindFromString("strvar"),"stag");
        Node open1 = cn.addOperand(ni.getNodeKindFromString("strlit"),"\\<");
        Node close1 = cn.addOperand( ni.getNodeKindFromString("strlit"),"\\>");

        Node s1 = cn.addOperation(ni.getNodeKindFromString("concat"), open1, startag);
        Node s2 = cn.addOperation(ni.getNodeKindFromString("concat"), s1, close1);

        Node endtag = cn.addOperand(ni.getNodeKindFromString("strvar"),"etag");
        Node open2 = cn.addOperand(ni.getNodeKindFromString("strlit"),"\\<\\/");
        Node close2 = cn.addOperand( ni.getNodeKindFromString("strlit"),"\\>");

        Node e1 = cn.addOperation(ni.getNodeKindFromString("concat"), open2, endtag);
        Node e2 = cn.addOperation(ni.getNodeKindFromString("concat"), e1, close2);

        Node regex = cn.addOperand( ni.getNodeKindFromString("strexp"),"[a-zA" +
                "-Z0-9]+");

        cn.addConstraint(ni.getNodeKindFromString("=="), startag, endtag);
        cn.addConstraint(ni.getNodeKindFromString("matches"), startag, regex);
        cn.addConstraint(ni.getNodeKindFromString("matches"), endtag, regex);

        Node r1 = cn.addOperation(ni.getNodeKindFromString("concat"), s2, content);
        Node r2 = cn.addOperation(ni.getNodeKindFromString("concat"), r1, e2);

        //Node matches2 = cn.addConstraint(NodeKind.MATCHES, strvar, con);

        Node matches2 = cn.addConstraint(ni.getNodeKindFromString("matches"), strvar, r2);

        assert matches2.isConstraint();


        cn.setStartNode(strvar);

        return cn;
    }

}
