package org.snt.cnetworkparser.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class StringUtils {

    final static Logger LOGGER = LoggerFactory.getLogger(StringUtils.class);

    private static Character[] arr0 = new Character[]{'\"'};
    private static Set<Character> special0 = new HashSet<Character>(Arrays.asList(arr0));

    private static Character[] arr1 = new Character[]{'+', '{', '}', '(', ')', '[', ']', '&', '^', '-', '?', '*', '\"', '$', '<', '>', '.', '|', '#'};
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
