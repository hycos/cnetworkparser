import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.cnetworkparser.utils.StringUtils;


public class TestEscape {

    final static Logger LOGGER = LoggerFactory.getLogger(TestEscape.class);



    @Test
    public void testThreatModels() {

        String s = "\"SELECT \\* FROM salaries WHERE userid = '\"";
        LOGGER.info(s);
        String out = StringUtils.trimQuotesFromString(s);

        LOGGER.info(out.toString());

    }



}