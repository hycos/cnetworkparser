import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetworkparser.core.CnetworkParser;
import org.snt.cnetworkparser.core.InputFormat;
import org.snt.cnetwork.core.ConstraintNetwork;

import java.io.File;

public class TestParser {

    final static Logger logger = LoggerFactory.getLogger(TestParser.class);

    private String getPath(String f){
        ClassLoader classLoader = getClass().getClassLoader();
        File sfile = new File(classLoader.getResource(f).getFile());
        return sfile.getAbsolutePath();
    }

    @Test
    public void testSol() {

        ConstraintNetwork cn  = new CnetworkParser(InputFormat.SOL).
                getCNfromFile(getPath("1.sol"));

        Assert.assertTrue(cn != null);

        logger.info(cn.toDot());

    }

    @Test
    public void testCVC() {

        ConstraintNetwork cn  = new CnetworkParser(InputFormat.CVC4).
                getCNfromFile(getPath("1.cvc"));
        Assert.assertTrue(cn != null);

        logger.info(cn.toConfig());
    }

    @Test
    public void testS3() {

        ConstraintNetwork cn  = new CnetworkParser(InputFormat.S3).
                getCNfromFile(getPath("1.s3"));


        Assert.assertTrue(cn != null);

    }

    @Test
    public void testZ3() {

        ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
                getCNfromFile(getPath("1.z3"));

        Assert.assertTrue(cn != null);

    }

    @Test
    public void testSamples() {

        ConstraintNetwork cn  = new CnetworkParser(InputFormat.SOL).
                getCNfromFile(getPath("simple01.sol"));

        logger.info(cn.toDot());

        Assert.assertTrue(cn != null);

    }

    @Test
    public void testZ3Num(){
        ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
                getCNfromFile(getPath("z3num.z3"));
        logger.info(cn.toDot());
    }

    @Test
    public void testKaluza() {
        ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
                getCNfromFile(getPath("kaluza3.z3"));
        logger.info(cn.toDot());
    }
}