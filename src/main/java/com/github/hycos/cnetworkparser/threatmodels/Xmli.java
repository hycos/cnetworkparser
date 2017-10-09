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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.NodeKind;
import com.github.hycos.cnetwork.core.graph.Operand;
import com.github.hycos.cnetwork.exception.EUFInconsistencyException;

import java.util.Map;

public class Xmli extends ThreatModel {


    private static String xmlInjection = ".*(\\<((! *- *-)?|( *- *-)?\\>)|\\< *CDATA\\[\\[.*\\]\\] *\\>).*";


    final static Logger LOGGER = LoggerFactory.getLogger(Xmli.class);

    public Xmli() {
        super();
        tmodel.put(NodeKind.XMLI, this);
    }


    @Override
    public ConstraintNetworkBuilder delegate(NodeKind type) {
        switch (type) {
            case XMLI:
                try {
                    return getXMLIThreatModel();
                } catch (EUFInconsistencyException e) {
                    assert false;
                }
        }
        return null;
    }

    @Override
    public Map<NodeKind, ThreatModel> getThreatModels() {
        return this.tmodel;
    }


    private ConstraintNetworkBuilder getXMLIThreatModel()
            throws EUFInconsistencyException {

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();


        Node strvar = new Operand("sv1", NodeKind.STRVAR);
        //Node op = new Operand(xmlInjection, NodeKind.STRREXP);

        //Node matches1 = cn.addOperation(NodeKind.MATCHES, strvar, op);

        Node content = new Operand("content", NodeKind.STRVAR);


        Node startag = new Operand("stag", NodeKind.STRVAR);
        Node open1 = new Operand("\\<", NodeKind.STRLIT);
        Node close1 = new Operand("\\>", NodeKind.STRLIT);

        Node s1 = cn.addOperation(NodeKind.CONCAT, open1, startag);
        Node s2 = cn.addOperation(NodeKind.CONCAT, s1, close1);

        Node endtag = new Operand("etag", NodeKind.STRVAR);
        Node open2 = new Operand("\\<\\/", NodeKind.STRLIT);
        Node close2 = new Operand("\\>", NodeKind.STRLIT);

        Node e1 = cn.addOperation(NodeKind.CONCAT, open2, endtag);
        Node e2 = cn.addOperation(NodeKind.CONCAT, e1, close2);

        Node regex = new Operand("[a-zA-Z0-9]+", NodeKind.STRREXP);

        cn.addConstraint(NodeKind.STR_EQUALS, startag, endtag);
        cn.addConstraint(NodeKind.MATCHES, startag, regex);
        cn.addConstraint(NodeKind.MATCHES, endtag, regex);

        Node r1 = cn.addOperation(NodeKind.CONCAT, s2, content);
        Node r2 = cn.addOperation(NodeKind.CONCAT, r1, e2);

        //Node matches2 = cn.addConstraint(NodeKind.MATCHES, strvar, con);

        Node matches2 = cn.addConstraint(NodeKind.MATCHES, strvar, r2);

        //cn.addConstraint(NodeKind.OR, matches1, matches2);

        cn.setStartNode(strvar);
        return cn;
    }

}
