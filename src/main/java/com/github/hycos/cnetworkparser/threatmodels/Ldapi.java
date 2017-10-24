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

public class Ldapi extends ThreatModel {

    final static Logger LOGGER = LoggerFactory.getLogger(Ldapi.class);

    private static String ldapBlacklist = "['\" /\\#\\<\\>,;\\+\\*\\)\\(\0\\|\\&]";

    public Ldapi() {
        super();
        tmodel.put(DefaultNodeKind.LDAPI, this);
    }

    @Override
    public ConstraintNetworkBuilder delegate(DefaultNodeKind type) {
        switch(type) {
            case LDAPI:
                try {
                    return getLDAPIThreatModel();
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

    private ConstraintNetworkBuilder getLDAPIThreatModel() throws InconsistencyException {
        ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
        Node op = new Operand(ldapBlacklist, DefaultNodeKind.STRREXP);
        cn.addOperand(DefaultNodeKind.STRREXP, ldapBlacklist);
        cn.setStartNode(op);
        return cn;
    }

}
