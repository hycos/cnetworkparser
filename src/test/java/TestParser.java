import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetworkparser.core.CnetworkParser;
import org.snt.cnetworkparser.core.InputFormat;
import org.snt.cnetwork.core.ConstraintNetwork;

import java.io.File;

public class TestParser {

    final static Logger LOGGER = LoggerFactory.getLogger(TestParser.class);

    private String getPath(String f){
        ClassLoader classLoader = getClass().getClassLoader();
        File sfile = new File(classLoader.getResource(f).getFile());
        return sfile.getAbsolutePath();
    }

    @Test
    public void testSol() {

        //ConstraintNetwork cn  = new CnetworkParser(InputFormat.SOL).
        //        getCNfromFile(getPath("1.sol"));

        //Assert.assertTrue(cn != null);

        ConstraintNetwork cn  = new CnetworkParser(InputFormat.SOL).
                getCNfromFile(getPath("2.sol"));

        LOGGER.info(cn.toDot());

    }

    @Test
    public void testCVC() {

        ConstraintNetwork cn  = new CnetworkParser(InputFormat.CVC4).
                getCNfromFile(getPath("1.cvc"));
        Assert.assertTrue(cn != null);

        LOGGER.info(cn.toConfig());
    }

    @Test
    public void testS3() {

        ConstraintNetwork cn  = new CnetworkParser(InputFormat.S3).
                getCNfromFile(getPath("1.s3"));


        Assert.assertTrue(cn != null);

    }

    @Test
    public void testZ3() {

        /**ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
                getCNfromFile(getPath("2.z3"));

        LOGGER.debug(cn.toDot());

        String t = "\\";**/

        //ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
        //        getCNfromFile(getPath("1.z3"));

        //Assert.assertTrue(cn != null);
        //ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
        //        getCNfromFile(getPath("beasties10.z3"));

        //Assert.assertNotNull(cn);

        //ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
        //        getCNfromFile(getPath("htmlCleaner11.z3"));

        //ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
        //       getCNfromFile(getPath("mathParser1.z3"));

        //Assert.assertNotNull(cn);

        ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
                getCNfromFile(getPath("t01.z3"));
        LOGGER.debug(cn.toDot());


    }

    @Test
    public void testSamples() {

        ConstraintNetwork cn  = new CnetworkParser(InputFormat.SOL).
                getCNfromFile(getPath("simple01.sol"));

        LOGGER.info(cn.toDot());

        Assert.assertTrue(cn != null);

    }

    @Test
    public void testZ3Num(){
        ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
                getCNfromFile(getPath("z3num.z3"));
        LOGGER.info(cn.toDot());
    }

    @Test
    public void testKaluza() {
        ConstraintNetwork cn  = new CnetworkParser(InputFormat.Z3STR2).
                getCNfromFile(getPath("kaluza4.z3"));
        LOGGER.info(cn.toDot());
    }
}