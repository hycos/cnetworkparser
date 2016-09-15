package org.snt.cnetworkparser.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class StringUtils {

    final static Logger logger = LoggerFactory.getLogger(StringUtils.class);

    private static Character[] sarray = new Character[]{'\"'};
    private static Set<Character> special = new HashSet<Character>(Arrays.asList(sarray));

    public static String trimQuotesFromString(String s) {
        if(s == null || s.isEmpty())
            return "";

        StringBuilder out = new StringBuilder();

        char carr[] = s.toCharArray();

        for(int k = 0; k < carr.length; k ++) {
            char c = carr[k];
            if((k == 0 || k == carr.length-1)
                    && special.contains(c)) {
                continue;
            }
            out.append(c);
        }
        return out.toString();
    }

}
