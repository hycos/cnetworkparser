import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetworkparser.exception.UnknownException;
import org.snt.cnetworkparser.threatmodels.ThreatModelFactory;
import org.snt.cnetwork.core.ConstraintNetwork;
import org.snt.cnetwork.core.OperandKind;


public class TestThreatModels {

    final static Logger logger = LoggerFactory.getLogger(TestThreatModels.class);


    private static String xmlInjection = ".*(\\<((! *- *-)?|( *- *-)?\\>)|\\< *CDATA\\[\\[.*\\]\\] *\\>).*";


    public ConstraintNetwork getCNFor(OperandKind kind) {
        ThreatModelFactory tf = ThreatModelFactory.getInstance();
        ConstraintNetwork cn = null;

        try {
            cn = tf.getCNforVulnerability(kind);
            assert(cn != null);
            logger.info(cn.toDot());

        } catch (UnknownException e) {
            e.printStackTrace();
        }

        return cn;
    }

    @Test
    public void testThreatModels() {
        assert(getCNFor(OperandKind.SQLINUM) != null);

        ConstraintNetwork cn = getCNFor(OperandKind.SQLISTR);

        logger.info(cn.toDot());
        assert(getCNFor(OperandKind.SQLISTR) != null);
        assert(getCNFor(OperandKind.XPATHNUM) != null);
        assert(getCNFor(OperandKind.XPATHSTR) != null);
        assert(getCNFor(OperandKind.LDAPI) != null);
        assert(getCNFor(OperandKind.XSS) != null);
        assert(getCNFor(OperandKind.XMLI) != null);
    }

    @Test
    public void test() {
        ConstraintNetwork cn = getCNFor(OperandKind.XMLI);

        logger.info(cn.toDot());
    }


}