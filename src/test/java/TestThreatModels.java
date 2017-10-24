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

import com.github.hycos.cnetwork.core.graph.ConstraintNetworkBuilder;
import com.github.hycos.cnetwork.core.graph.DefaultNodeKind;
import com.github.hycos.cnetworkparser.exception.UnknownException;
import com.github.hycos.cnetworkparser.threatmodels.ThreatModelFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class TestThreatModels {

    final static Logger LOGGER = LoggerFactory.getLogger(TestThreatModels.class);


    private static String xmlInjection = ".*(\\<((! *- *-)?|( *- *-)?\\>)|\\< *CDATA\\[\\[.*\\]\\] *\\>).*";


    public ConstraintNetworkBuilder getCNFor(DefaultNodeKind kind) {
        ThreatModelFactory tf = ThreatModelFactory.getInstance();
        ConstraintNetworkBuilder cn = null;

        try {
            cn = tf.getCNforVulnerability(kind);
            assert(cn != null);
            //LOGGER.info(cn.toDot());

        } catch (UnknownException e) {
            e.printStackTrace();
        }

        return cn;
    }

    @Test
    public void testThreatModels() {
        assert(getCNFor(DefaultNodeKind.SQLINUM) != null);

        ConstraintNetworkBuilder cn = getCNFor(DefaultNodeKind.SQLISTR);

        //ConstraintNetworkBuilderLOGGER.info(cn.toDot());
        assert(getCNFor(DefaultNodeKind.SQLISTR) != null);
        assert(getCNFor(DefaultNodeKind.XPATHNUM) != null);
        assert(getCNFor(DefaultNodeKind.XPATHSTR) != null);
        assert(getCNFor(DefaultNodeKind.LDAPI) != null);
        assert(getCNFor(DefaultNodeKind.XSS) != null);
        assert(getCNFor(DefaultNodeKind.XMLI) != null);
    }

    @Test
    public void test() {
        ConstraintNetworkBuilder cn = getCNFor(DefaultNodeKind.XMLI);

        LOGGER.debug(cn.getConstraintNetwork().toDot());
    }


}