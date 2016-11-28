import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetworkparser.exception.UnknownException;
import org.snt.cnetworkparser.threatmodels.ThreatModelFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.NodeKind;


public class TestThreatModels {

    final static Logger LOGGER = LoggerFactory.getLogger(TestThreatModels.class);


    private static String xmlInjection = ".*(\\<((! *- *-)?|( *- *-)?\\>)|\\< *CDATA\\[\\[.*\\]\\] *\\>).*";


    public ConstraintNetwork getCNFor(NodeKind kind) {
        ThreatModelFactory tf = ThreatModelFactory.getInstance();
        ConstraintNetwork cn = null;

        try {
            cn = tf.getCNforVulnerability(kind);
            assert(cn != null);
            LOGGER.info(cn.toDot());

        } catch (UnknownException e) {
            e.printStackTrace();
        }

        return cn;
    }

    @Test
    public void testThreatModels() {
        assert(getCNFor(NodeKind.SQLINUM) != null);

        ConstraintNetwork cn = getCNFor(NodeKind.SQLISTR);

        LOGGER.info(cn.toDot());
        assert(getCNFor(NodeKind.SQLISTR) != null);
        assert(getCNFor(NodeKind.XPATHNUM) != null);
        assert(getCNFor(NodeKind.XPATHSTR) != null);
        assert(getCNFor(NodeKind.LDAPI) != null);
        assert(getCNFor(NodeKind.XSS) != null);
        assert(getCNFor(NodeKind.XMLI) != null);
    }

    @Test
    public void test() {
        ConstraintNetwork cn = getCNFor(NodeKind.XMLI);

        LOGGER.info(cn.toDot());
    }


}