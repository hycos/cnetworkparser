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

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.hycos.cnetworkparser.utils.StringUtils;


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