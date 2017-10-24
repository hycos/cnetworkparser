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

import com.github.hycos.cnetwork.api.labelmgr.exception.InconsistencyException;
import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.DefaultNodeKind;
import com.github.hycos.cnetwork.core.graph.Node;
import com.github.hycos.cnetwork.core.graph.Operand;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

public class Xpathi extends ThreatModel {


    final static Logger LOGGER = LoggerFactory.getLogger(Xpathi.class);

    public Xpathi() {
        super();
        tmodel.put(DefaultNodeKind.XPATHNUM, this);
        tmodel.put(DefaultNodeKind.XPATHSTR, this);
    }

    @Override
    public ConstraintNetworkBuilder delegate(DefaultNodeKind type) {
        switch (type) {
            case XPATHNUM:
                try {
                    return getNumTautology();
                } catch (InconsistencyException e) {
                    assert false;
                }
            case XPATHSTR:
                try {
                    return getStrTautology();
                } catch (InconsistencyException e) {
                    assert false;
                }
        }
        return null;
    }

    @Override
    public Map<DefaultNodeKind, ThreatModel> getThreatModels() {
        return this.tmodel;
    }


    private ConstraintNetworkBuilder getStrTautology() throws InconsistencyException {

        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, DefaultNodeKind.STRREXP);

        String item = "sv1";
        Node v1 = new Operand(item, DefaultNodeKind.STRVAR);

        Node orv1 = cn.addOperation(DefaultNodeKind.CONCAT, or, v1);

        String seq = "'.*=.*'";
        Node eq = new Operand(seq, DefaultNodeKind.STRREXP);

        Node v2 = new Operand("sv2", DefaultNodeKind.STRVAR);

        Node orv1comp = cn.addOperation(DefaultNodeKind.CONCAT, eq, v2);

        Node orv1compv2 = cn.addOperation(DefaultNodeKind.CONCAT, orv1, orv1comp);

        cn.addConstraint(DefaultNodeKind.STR_EQUALS, v1, v2);

        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, DefaultNodeKind.STRREXP);

        cn.addOperation(DefaultNodeKind.CONCAT, orv1compv2, comment);

        cn.setStartNode(orv1compv2);

        return cn;
    }

    private ConstraintNetworkBuilder getNumTautology() throws InconsistencyException {

        /**ConstraintNetwork cn = new ConstraintNetwork();

         Node n0 = getEqNumTautology(cn);
         Node n1 = getGtNumTautology(cn);
         Node n2 = getStNumTautology(cn);

         Node tm = new Operand("tm", NodeKind.STRVAR);

         Node m1 = cn.addOperation(NodeKind.MATCHES, tm, n0);
         Node m2 = cn.addOperation(NodeKind.MATCHES, tm, n1);
         Node m3 = cn.addOperation(NodeKind.MATCHES, tm, n2);

         Node or1 = cn.addOperation(NodeKind.OR, m1, m2);

         Node or2 = cn.addConstraint(NodeKind.OR, m3, or1);

         cn.setStartNode(tm);**/


        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        Node n = null;

        n = getGeqNumTautology(cn);

        cn.setStartNode(n);

        return cn;

    }

    private Node getEqNumTautology(ConstraintNetworkBuilder cn) throws InconsistencyException {

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, DefaultNodeKind.STRREXP);

        Node v1 = new Operand("sv1", DefaultNodeKind.NUMVAR);

        Node toStrV1 = cn.addOperation(DefaultNodeKind.TOSTR, v1);

        Node orv1 = cn.addOperation(DefaultNodeKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" *= *", DefaultNodeKind.STRREXP);

        Node orv1comp = cn.addOperation(DefaultNodeKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv2", DefaultNodeKind.NUMVAR);

        Node toStrV2 = cn.addOperation(DefaultNodeKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(DefaultNodeKind.CONCAT, orv1comp, toStrV2);

        cn.addOperation(DefaultNodeKind.EQUALS, v1, v2);

        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, DefaultNodeKind.STRREXP);

        cn.addOperation(DefaultNodeKind.CONCAT, orv1compv2, comment);

        return orv1compv2;
    }

    private Node getGtNumTautology(ConstraintNetworkBuilder cn) throws InconsistencyException {

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, DefaultNodeKind.STRREXP);

        Node v1 = new Operand("sv3", DefaultNodeKind.NUMVAR);

        Node toStrV1 = cn.addOperation(DefaultNodeKind.TOSTR, v1);

        Node orv1 = cn.addOperation(DefaultNodeKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" *\\> *", DefaultNodeKind.STRREXP);

        Node orv1comp = cn.addOperation(DefaultNodeKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv4", DefaultNodeKind.NUMVAR);

        Node toStrV2 = cn.addOperation(DefaultNodeKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(DefaultNodeKind.CONCAT, orv1comp, toStrV2);

        cn.addOperation(DefaultNodeKind.GREATER, v1, v2);

        String scomment = "(\\<!\\-\\-|#)";
        Node comment = new Operand(scomment, DefaultNodeKind.STRREXP);

        cn.addOperation(DefaultNodeKind.CONCAT, orv1compv2, comment);

        return orv1compv2;
    }

    private Node getStNumTautology(ConstraintNetworkBuilder cn) throws InconsistencyException {

        String sor = ".*' +[Oo][Rr] +' +";
        Node or = new Operand(sor, DefaultNodeKind.STRREXP);

        Node v1 = new Operand("sv5", DefaultNodeKind.NUMVAR);

        Node toStrV1 = cn.addOperation(DefaultNodeKind.TOSTR, v1);

        Node orv1 = cn.addOperation(DefaultNodeKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" *\\> *", DefaultNodeKind.STRREXP);

        Node orv1comp = cn.addOperation(DefaultNodeKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv6", DefaultNodeKind.NUMVAR);

        Node toStrV2 = cn.addOperation(DefaultNodeKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(DefaultNodeKind.CONCAT, orv1comp, toStrV2);

        cn.addOperation(DefaultNodeKind.SMALLER, v1, v2);

        return orv1compv2;
    }

    private Node getGeqNumTautology(ConstraintNetworkBuilder cn) throws InconsistencyException {

        String sor = ".*' +[Oo][Rr] +'";
        Node or = new Operand(sor, DefaultNodeKind.STRREXP);

        Node v1 = new Operand("sv7", DefaultNodeKind.NUMVAR);

        Node toStrV1 = cn.addOperation(DefaultNodeKind.TOSTR, v1);

        Node orv1 = cn.addOperation(DefaultNodeKind.CONCAT, or, toStrV1);

        Node eq = new Operand(" +\\>= +", DefaultNodeKind.STRREXP);

        Node orv1comp = cn.addOperation(DefaultNodeKind.CONCAT, orv1, eq);

        Node v2 = new Operand("sv8", DefaultNodeKind.NUMVAR);

        Node toStrV2 = cn.addOperation(DefaultNodeKind.TOSTR, v2);

        Node orv1compv2 = cn.addOperation(DefaultNodeKind.CONCAT, orv1comp, toStrV2);

        cn.addConstraint(DefaultNodeKind.GREATEREQ, v1, v2);

        return orv1compv2;
    }

}
