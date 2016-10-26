package borg.edtrading.util;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;

/**
 * MiscUtil
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public abstract class MiscUtil {

    static final Logger logger = LogManager.getLogger(MiscUtil.class);

    public static String systemNameFromFilename(File screenshotFile) {
        String systemName = null;
        // Remove date and file extension
        if (screenshotFile.getName().matches("\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}\\-\\d{2}\\-\\d{2} .+\\.png")) {
            systemName = screenshotFile.getName().substring("0000-00-00 00-00-00 ".length()).replace(".png", "");
        }
        // Remove numbers like (1) or highres markers like (Highres)
        if (systemName.contains("(") && systemName.contains(")")) {
            systemName = systemName.substring(0, systemName.indexOf("("));
        }
        return systemName.trim();
    }

    public static float levenshteinError(CharSequence mostLikelyCorrectReferenceString, CharSequence someOtherString) {
        float dist = StringUtils.getLevenshteinDistance(mostLikelyCorrectReferenceString.toString().toLowerCase(), someOtherString.toString().toLowerCase());
        float err = dist / mostLikelyCorrectReferenceString.length();

        return err;
    }

    public static String getAsString(Object o) {
        return MiscUtil.getAsString(o, null);
    }

    public static String getAsString(Object o, String defaultValue) {
        if (o == null) {
            return defaultValue;
        } else {
            String s = o.toString();

            if (StringUtils.isEmpty(s)) {
                return defaultValue;
            } else {
                return s;
            }
        }
    }

    public static boolean getAsBoolean(Object o) {
        return MiscUtil.getAsBoolean(o, false);
    }

    public static boolean getAsBoolean(Object o, Boolean defaultValue) {
        if (o != null) {
            if (o instanceof Boolean) {
                return (Boolean) o;
            } else {
                final String s = o.toString().toLowerCase();

                if (s.length() > 0) {
                    return "true".equals(s) || "yes".equals(s) || "y".equals(s) || "on".equals(s) || "1".equals(s);
                }
            }
        }

        return Boolean.TRUE.equals(defaultValue);
    }

    public static Number getAsNumber(Object o) {
        return MiscUtil.getAsNumber(o, null);
    }

    public static Number getAsNumber(Object o, Number defaultValue) {
        if (o == null) {
            return null;
        } else if (o instanceof Number) {
            return (Number) o;
        } else {
            try {
                return new BigDecimal(o.toString().replace(',', '.').replaceAll("[^\\d\\-\\.]", ""));
            } catch (Exception e) {
                return null;
            }
        }
    }

    public static Integer getAsInt(Object o) {
        return MiscUtil.getAsInteger(o); // Just an alias
    }

    public static Integer getAsInt(Object o, Integer defaultValue) {
        return MiscUtil.getAsInteger(o, defaultValue); // Just an alias
    }

    public static Integer getAsInteger(Object o) {
        return MiscUtil.getAsInteger(o, null);
    }

    public static Integer getAsInteger(Object o, Integer defaultValue) {
        if (o != null) {
            if (o instanceof Integer) {
                return (Integer) o;
            } else if (o instanceof Number) {
                return ((Number) o).intValue();
            } else {
                Number number = getAsNumber(o, defaultValue);

                if (number != null) {
                    return number.intValue();
                }
            }
        }

        return defaultValue;
    }

    public static Long getAsLong(Object o) {
        return MiscUtil.getAsLong(o, null);
    }

    public static Long getAsLong(Object o, Long defaultValue) {
        if (o != null) {
            if (o instanceof Long) {
                return (Long) o;
            } else if (o instanceof Number) {
                return ((Number) o).longValue();
            } else {
                Number number = getAsNumber(o, defaultValue);

                if (number != null) {
                    return number.longValue();
                }
            }
        }

        return defaultValue;
    }

    public static Float getAsFloat(Object o) {
        return MiscUtil.getAsFloat(o, null);
    }

    public static Float getAsFloat(Object o, Float defaultValue) {
        if (o != null) {
            if (o instanceof Float) {
                return (Float) o;
            } else if (o instanceof Number) {
                return ((Number) o).floatValue();
            } else {
                Number number = getAsNumber(o, defaultValue);

                if (number != null) {
                    return number.floatValue();
                }
            }
        }

        return defaultValue;
    }

    public static Double getAsDouble(Object o) {
        return MiscUtil.getAsDouble(o, null);
    }

    public static Double getAsDouble(Object o, Double defaultValue) {
        if (o != null) {
            if (o instanceof Double) {
                return (Double) o;
            } else if (o instanceof Number) {
                return ((Number) o).doubleValue();
            } else {
                Number number = getAsNumber(o, defaultValue);

                if (number != null) {
                    return number.doubleValue();
                }
            }
        }

        return defaultValue;
    }

    public static BigDecimal getAsBigDecimal(Object o) {
        return MiscUtil.getAsBigDecimal(o, null);
    }

    public static BigDecimal getAsBigDecimal(Object o, BigDecimal defaultValue) {
        if (o != null) {
            if (o instanceof BigDecimal) {
                return (BigDecimal) o;
            } else {
                final String s = o.toString().replace(',', '.');

                if (s.length() > 0) {
                    try {
                        return new BigDecimal(s.replaceAll("[^\\d\\-\\.]", ""));
                    } catch (Exception e) {
                        // Abort
                    }
                }
            }
        }

        return defaultValue;
    }

    public static Date getAsDate(Object o) {
        return MiscUtil.getAsDate(o, null);
    }

    public static Date getAsDate(Object o, Date defaultValue) {
        if (o != null) {
            if (o instanceof Date) {
                return (Date) o;
            } else if (o instanceof Number) {
                return new Date(((Number) o).longValue());
            } else {
                final String s = o.toString();

                if (s.length() > 0) {
                    // Datebase timestamp?
                    try {
                        // Example: 2001-07-04 21:08:56
                        return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(s);
                    } catch (Exception e) { /* Continue */
                    }

                    // Datebase timestamp w/o seconds?
                    try {
                        // Example: 2001-07-04 21:08
                        return new SimpleDateFormat("yyyy-MM-dd HH:mm").parse(s);
                    } catch (Exception e) { /* Continue */
                    }

                    // Datebase timestamp w/o time?
                    try {
                        // Example: 2001-07-04
                        return new SimpleDateFormat("yyyy-MM-dd").parse(s);
                    } catch (Exception e) { /* Continue */
                    }

                    // Timestamp?
                    try {
                        // Example: 2001-07-04T21:08:56.000+0200
                        return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ").parse(s);
                    } catch (Exception e) { /* Continue */
                    }

                    // RFC 822 date?
                    try {
                        // Example: Wed, 4 Jul 2001 21:08:56 +0200
                        return new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z", Locale.US).parse(s);
                    } catch (Exception e) { /* Continue */
                    }

                    // RFC 822 date?
                    try {
                        // Example: Mi, 4 Jul 2001 21:08:56 +0200
                        return new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z", Locale.GERMANY).parse(s);
                    } catch (Exception e) { /* Continue */
                    }

                    // java.util.Date.toString()?
                    try {
                        // Example: Wed Jul 04 21:08:56 CEST 2001
                        return new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy", Locale.US).parse(s);
                    } catch (Exception e) { /* Continue */
                    }

                    // java.util.Date.toString()?
                    try {
                        // Example: Mi Jul 04 21:08:56 MESZ 2001
                        return new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy", Locale.GERMANY).parse(s);
                    } catch (Exception e) { /* Continue */
                    }

                    // More US and GERMANY dates/times...
                    try {
                        // Example: Jul 4, 2001 9:08:56 PM
                        return DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM, Locale.US).parse(s);
                    } catch (Exception e1) {
                        try {
                            // Example: 04.07.2001 21:08:56
                            return DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM, Locale.GERMANY).parse(s);
                        } catch (Exception e2) {
                            try {
                                // Example: Jul 4, 2001 9:08 PM
                                return DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, Locale.US).parse(s);
                            } catch (Exception e3) {
                                try {
                                    // Example: 04.07.2001 21:08
                                    return DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, Locale.GERMANY).parse(s);
                                } catch (Exception e4) {
                                    try {
                                        // Example: Jul 4, 2001
                                        return DateFormat.getDateInstance(DateFormat.MEDIUM, Locale.US).parse(s);
                                    } catch (Exception e5) {
                                        try {
                                            // Example: 04.07.2001
                                            return DateFormat.getDateInstance(DateFormat.MEDIUM, Locale.GERMANY).parse(s);
                                        } catch (Exception e6) {
                                            try {
                                                // Example: 9:08:56 PM
                                                return DateFormat.getTimeInstance(DateFormat.MEDIUM, Locale.US).parse(s);
                                            } catch (Exception e7) {
                                                try {
                                                    // Example: 21:08:56
                                                    return DateFormat.getTimeInstance(DateFormat.MEDIUM, Locale.GERMANY).parse(s);
                                                } catch (Exception e8) {
                                                    try {
                                                        // Example: 9:08 PM
                                                        return DateFormat.getTimeInstance(DateFormat.SHORT, Locale.US).parse(s);
                                                    } catch (Exception e9) {
                                                        try {
                                                            // Example: 21:08
                                                            return DateFormat.getTimeInstance(DateFormat.SHORT, Locale.GERMANY).parse(s);
                                                        } catch (Exception ef) {
                                                            // Continue
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Timestamp using only digits?
                    try {
                        // Example: 20010704210856
                        Date date = new SimpleDateFormat("yyyyMMddHHmmss").parse(s);
                        Calendar cal = Calendar.getInstance();
                        cal.setTime(date);
                        if (cal.get(Calendar.YEAR) >= 1000 && cal.get(Calendar.YEAR) <= 3000) {
                            return date;
                        }
                    } catch (Exception e) { /* Continue */
                    }

                    // Parseable as long? Treat as time in millis
                    try {
                        if (s.matches("\\-?\\d+")) {
                            return new Date(Long.valueOf(s));
                        }
                    } catch (Exception e) { /* Continue */
                    }
                }
            }
        }

        return defaultValue;
    }

    public static void sortMapByValueReverse(LinkedHashMap map) {
        MiscUtil.sortMapByValue(map, null, true);
    }

    public static void sortMapByValue(LinkedHashMap map) {
        MiscUtil.sortMapByValue(map, null, false);
    }

    public static void sortMapByValue(LinkedHashMap map, Comparator comparator) {
        MiscUtil.sortMapByValue(map, null, false);
    }

    /**
     * Sorts a map by its values. The map must be a <code>LinkedHashMap</code>, because only this type of map has a predictable iteration order.
     *
     * @param map
     *            Map to be sorted
     * @param comparator
     *            Comparator for comparing the map's values or <code>null</code> for natural ordering of the values.
     * @since 3.5
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    private static void sortMapByValue(LinkedHashMap map, Comparator comparator, boolean reverse) {
        // Make two copies of the original map: A list of all keys and one of all values
        List mapKeys = new ArrayList(map.keySet());
        List mapValues = new ArrayList(map.values());

        // Sort all values
        List sortedValues = new ArrayList();
        sortedValues.addAll(mapValues);
        Collections.sort(sortedValues, comparator);
        if (reverse) {
            Collections.reverse(sortedValues);
        }

        // Clear the original map
        map.clear();

        // Re-fill the original map in a sorted order
        for (Object value : sortedValues) {
            int index = mapValues.indexOf(value); // Index within the original map
            Object key = mapKeys.get(index); // Key from the original map

            map.put(key, value); // Re-fill

            mapValues.remove(index); // Do not find this key/value again
            mapKeys.remove(index); // Do not find this key/value again
        }
    }

}
