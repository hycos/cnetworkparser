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

package com.github.hycos.cnetworkparser.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class StringUtils {

    final static Logger LOGGER = LoggerFactory.getLogger(StringUtils.class);

    private static Character[] arr0 = new Character[]{'\"'};
    private static Set<Character> special0 = new HashSet<Character>(Arrays.asList(arr0));

    private static Character[] arr1 = new Character[]{'+', '{', '}', '(', ')',
            '[', ']', '&', '^', '-', '?', '*', '\"',
            '$', '<', '>', '.', '|', '#' , '~' , '@', '\\'};

    private static Set<Character> special1 = new HashSet<Character>(Arrays.asList(arr1));


    public static String trimQuotesFromString(String s) {
        if(s == null || s.isEmpty())
            return "";

        StringBuilder out = new StringBuilder();

        char carr[] = s.toCharArray();

        for(int k = 0; k < carr.length; k ++) {
            char c = carr[k];
            if((k == 0 || k == carr.length-1)
                    && special0.contains(c)) {
                continue;
            }
            out.append(c);
        }
        return out.toString();
    }

    /**
     * escape special character in a string with a backslash
     * @param s string to be escaped
     * @return escaped string
     */
    public static String escapeSpecialCharacters(String s) {
        if (s == null)
            return "";

        StringBuilder out = new StringBuilder();
        char pred = ' ';
        for (char c : s.toCharArray()) {
            if (special1.contains(c)) {
                out.append("\\" + c);
            } else {
                out.append(c);
            }
            pred = c;
        }
        return out.toString();
    }

    /**
     * unescape special character in a string
     * @param s string to be unescaped
     * @return unescaped string
     */
    public static String unescapeSpecialCharacters(String s) {
        if (s == null)
            return "";

        StringBuilder out = new StringBuilder();
        char pred = ' ';
        for (char c : s.toCharArray()) {
            if (pred == '\\' && special1.contains(c)) {
                out.deleteCharAt(out.length() - 1);
                out.append(c);
            } else {
                out.append(c);
            }
            pred = c;
        }
        return out.toString();
    }


}
