package borg.edtrading.ocr;

import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.data.Body;
import borg.edtrading.data.BodyInfo;
import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.data.ScannedBodyInfo;
import borg.edtrading.util.MatchSorter.MatchGroup;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * ScannedBodyInfoParser
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScannedBodyInfoParser {

    static final Logger logger = LogManager.getLogger(ScannedBodyInfoParser.class);

    private static String currentScreenshotFilename = "";

    public static ScannedBodyInfo fromScannedAndSortedWords(String screenshotFilename, String systemName, List<MatchGroup> bodyNameWords, List<MatchGroup> bodyInfoWords, List<Body> eddbBodies) {
        currentScreenshotFilename = screenshotFilename;

        List<TemplateMatch> bodyInfoMatches = new ArrayList<>();
        for (MatchGroup mg : bodyInfoWords) {
            bodyInfoMatches.addAll(mg.getGroupMatches());
        }

        // The body group (star/planet/belt/rings) determines which labels/values can occur.
        BodyInfo bodyGroup = null;

        // Store the indexes of all labels. This allows us to later extract the values. In most cases we have label, value, label, value, ...
        SortedMap<Integer, String> sortedLabelIndexes = new TreeMap<>();

        // The first expected label is the body mass. This also allows us to decide which group the body belongs to.
        if (findAndRemove("SOLARMASSES:", bodyInfoMatches, sortedLabelIndexes) != null) {
            bodyGroup = BodyInfo.GROUP_STAR;
        } else if (findAndRemove("EARTHMASSES:", bodyInfoMatches, sortedLabelIndexes) != null) {
            bodyGroup = BodyInfo.GROUP_PLANET;
        } else if (findAndRemove("MOONMASSES:", bodyInfoMatches, sortedLabelIndexes) != null) {
            bodyGroup = BodyInfo.GROUP_BELT;
        } else {
            // None of the three above most likely means a scrolled-down screenshot of the rings of a body.
            // Could be a planet, but could also be a dwarf star...
            bodyGroup = BodyInfo.GROUP_RINGS;
        }

        // The other labels depend on the body group.
        if (bodyGroup == BodyInfo.GROUP_STAR) {
            // TODO Parse star class from description text
            findAndRemove("AGE:", bodyInfoMatches, 0, indexOf("SOLARMASSES:", sortedLabelIndexes), "", sortedLabelIndexes); // Age unfortunately is before solar masses, so explicitly search from index 0...
            findAndRemove("SOLARRADIUS:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("SURFACETEMP:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ORBITALPERIOD:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("SEMIMAJORAXIS:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ORBITALECCENTRICITY:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ORBITALINCLINATION:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ARGOFPERIAPSIS:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("STARCATALOGUEID:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("GLIESE:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("HIPP:", bodyInfoMatches, sortedLabelIndexes);
        } else if (bodyGroup == BodyInfo.GROUP_PLANET) {
            for (BodyInfo bi : BodyInfo.byPrefix("RESERVES_")) {
                String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                if (findAndRemove(nameWithoutSpaces, bodyInfoMatches, 0, indexOf("EARTHMASSES:", sortedLabelIndexes), "RESERVES_", sortedLabelIndexes) != null) { // Reserves unfortunately is before earth masses, so explicitly search from index 0...
                    break; // Expect only one hit
                }
            }
            findAndRemove("RADIUS:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("GRAVITY:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("SURFACETEMP:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("SURFACEPRESSURE:", bodyInfoMatches, sortedLabelIndexes);
            if (findAndRemove("VOLCANISM:", bodyInfoMatches, sortedLabelIndexes) != null) {
                for (BodyInfo bi : BodyInfo.byPrefix("VOLCANISM_")) {
                    String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                    if (findAndRemove(nameWithoutSpaces, bodyInfoMatches, "VOLCANISM_", sortedLabelIndexes) != null) {
                        break; // Expect only one hit
                    }
                }
            }
            if (findAndRemove("ATMOSPHERETYPE:", bodyInfoMatches, sortedLabelIndexes) != null) {
                for (BodyInfo bi : BodyInfo.byPrefix("ATMOSPHERE_TYPE_")) {
                    String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                    if (findAndRemove(nameWithoutSpaces, bodyInfoMatches, "ATMOSPHERE_TYPE_", sortedLabelIndexes) != null) {
                        break; // Expect only one hit
                    }
                }
            }
            Integer idxAtmosphere = findAndRemove("ATMOSPHERE:", bodyInfoMatches, sortedLabelIndexes);
            Integer idxComposition = findAndRemove("COMPOSITION:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ORBITALPERIOD:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("SEMIMAJORAXIS:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ORBITALECCENTRICITY:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ORBITALINCLINATION:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ARGOFPERIAPSIS:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ROTATIONALPERIOD:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("(TIDALLYLOCKED)", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("AXIALTILT:", bodyInfoMatches, sortedLabelIndexes);
            Integer idxPlanetMaterials = findAndRemove("PLANETMATERIALS:", bodyInfoMatches, sortedLabelIndexes);
            if (idxAtmosphere != null) {
                int idxAfter = indexAfter(idxAtmosphere, sortedLabelIndexes, bodyInfoMatches.size());
                for (BodyInfo bi : BodyInfo.byPrefix("ATMOSPHERE_COMPONENT_")) {
                    // Expect multiple hits, but each one only once
                    String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                    findAndRemove(nameWithoutSpaces, bodyInfoMatches, idxAtmosphere, idxAfter, "ATMOSPHERE_COMPONENT_", sortedLabelIndexes);
                }
            }
            if (idxComposition != null) {
                int idxAfter = indexAfter(idxComposition, sortedLabelIndexes, bodyInfoMatches.size());
                for (BodyInfo bi : BodyInfo.byPrefix("COMPOSITION_")) {
                    // Expect multiple hits, but each one only once
                    String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                    findAndRemove(nameWithoutSpaces, bodyInfoMatches, idxComposition, idxAfter, "COMPOSITION_", sortedLabelIndexes);
                }
            }
            if (idxPlanetMaterials != null) {
                int idxAfter = indexAfter(idxPlanetMaterials, sortedLabelIndexes, bodyInfoMatches.size());
                for (Item i : Item.byType(ItemType.ELEMENT)) {
                    // Expect multiple hits, but each one only once
                    String nameWithoutSpaces = i.getName().replaceAll("\\s", "");
                    findAndRemove(nameWithoutSpaces, bodyInfoMatches, idxPlanetMaterials, idxAfter, "PLANET_MATERIAL_", sortedLabelIndexes);
                }
            }
        } else if (bodyGroup == BodyInfo.GROUP_BELT) {
            for (BodyInfo bi : BodyInfo.byPrefix("RESERVES_")) {
                String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                if (findAndRemove(nameWithoutSpaces, bodyInfoMatches, 0, indexOf("MOONMASSES:", sortedLabelIndexes), "RESERVES_", sortedLabelIndexes) != null) { // Reserves unfortunately is before moon masses, so explicitly search from index 0...
                    break; // Expect only one hit
                }
            }
            Integer idxRingType = findAndRemove("RINGTYPE:", bodyInfoMatches, 0, indexOf("MOONMASSES:", sortedLabelIndexes), "", sortedLabelIndexes); // Ring type unfortunately is before moon masses, so explicitly search from index 0...
            if (idxRingType != null) {
                for (BodyInfo bi : BodyInfo.byPrefix("RING_TYPE_")) {
                    String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                    if (findAndRemove(nameWithoutSpaces, bodyInfoMatches, idxRingType, indexOf("MOONMASSES:", sortedLabelIndexes), "RING_TYPE_", sortedLabelIndexes) != null) {
                        break; // Expect only one hit
                    }
                }
            }
            findAndRemove("ORBITALPERIOD:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("SEMIMAJORAXIS:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ORBITALECCENTRICITY:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ORBITALINCLINATION:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ARGOFPERIAPSIS:", bodyInfoMatches, sortedLabelIndexes);
        }

        if (bodyGroup == BodyInfo.GROUP_RINGS || bodyGroup == BodyInfo.GROUP_PLANET || bodyGroup == BodyInfo.GROUP_STAR) {
            // Either a rings-only screenshot, or a body which can have rings
            // TODO Scan rings
        }

        // Now that we have all labels we can start to parse the values
        if (bodyGroup == BodyInfo.GROUP_STAR) {
            // TODO ...
        } else if (bodyGroup == BodyInfo.GROUP_PLANET) {
            BigDecimal earthMasses = fixAndRemoveEarthMasses("EARTHMASSES:", bodyInfoMatches, sortedLabelIndexes);
            BigDecimal radius = fixAndRemoveRadius("RADIUS:", bodyInfoMatches, sortedLabelIndexes);
            BigDecimal gravity = fixAndRemoveGravity("GRAVITY:", bodyInfoMatches, sortedLabelIndexes);
            BigDecimal surfaceTemp = fixAndRemoveSurfaceTemp("SURFACETEMP:", bodyInfoMatches, sortedLabelIndexes);
            //            BigDecimal surfacePressure = fixAndRemoveEarthMasses("SURFACEPRESSURE:", bodyInfoMatches, sortedLabelIndexes);
            //            BodyInfo volcanism = fixAndRemoveEarthMasses("VOLCANISM:", bodyInfoMatches, sortedLabelIndexes);
            //            BodyInfo atmosphereType = fixAndRemoveEarthMasses("ATMOSPHERETYPE:", bodyInfoMatches, sortedLabelIndexes);
            //            Object atmosphere = fixAndRemoveEarthMasses("ATMOSPHERE:", bodyInfoMatches, sortedLabelIndexes); // TODO ...
            //            Object composition = fixAndRemoveEarthMasses("COMPOSITION:", bodyInfoMatches, sortedLabelIndexes); // TODO ...
            //            BigDecimal orbitalPeriod = fixAndRemoveEarthMasses("ORBITALPERIOD:", bodyInfoMatches, sortedLabelIndexes);
            //            BigDecimal semiMajorAxis = fixAndRemoveEarthMasses("SEMIMAJORAXIS:", bodyInfoMatches, sortedLabelIndexes);
            //            BigDecimal orbitalEccentricity = fixAndRemoveEarthMasses("ORBITALECCENTRICITY:", bodyInfoMatches, sortedLabelIndexes);
            //            BigDecimal orbitalInclination = fixAndRemoveEarthMasses("ORBITALINCLINATION:", bodyInfoMatches, sortedLabelIndexes);
            //            BigDecimal argOfPeriapsis = fixAndRemoveEarthMasses("ARGOFPERIAPSIS:", bodyInfoMatches, sortedLabelIndexes);
            //            BigDecimal rotationalPeriod = fixAndRemoveEarthMasses("ROTATIONALPERIOD:", bodyInfoMatches, sortedLabelIndexes);
            //            Boolean tidallyLocked = fixAndRemoveEarthMasses("(TIDALLYLOCKED)", bodyInfoMatches, sortedLabelIndexes); // TODO ...
            BigDecimal axialTilt = fixAndRemoveAxialTilt("AXIALTILT:", bodyInfoMatches, sortedLabelIndexes);
            //            Object planetMaterials = fixAndRemoveEarthMasses("PLANETMATERIALS:", bodyInfoMatches, sortedLabelIndexes); // TODO ...
        } else if (bodyGroup == BodyInfo.GROUP_BELT) {
            // TODO ...
        } else if (bodyGroup == BodyInfo.GROUP_RINGS) {
            // TODO ...
        }

        // TODO Remove this debug output
        System.out.print("Scanned: ");
        for (TemplateMatch m : bodyInfoMatches) {
            System.out.print("<" + m.getTemplate().getText() + ">");
        }
        System.out.println();
        System.out.print("Parsed:  ");
        for (TemplateMatch m : bodyInfoMatches) {
            System.out.print("<" + (m.getShouldHaveBeen() == null ? "?" : m.getShouldHaveBeen()) + ">");
        }
        System.out.println();

        return null; // TODO
    }

    private static Integer indexOf(String label, SortedMap<Integer, String> sortedLabelIndexes) {
        for (Integer index : sortedLabelIndexes.keySet()) {
            if (sortedLabelIndexes.get(index).equals(label)) {
                return index;
            }
        }

        return null;
    }

    private static int indexAfter(Integer idx, SortedMap<Integer, String> sortedLabelIndexes, int defaultValue) {
        if (idx == null || sortedLabelIndexes.isEmpty()) {
            return defaultValue;
        } else {
            List<Integer> indexes = new ArrayList<>(sortedLabelIndexes.keySet());
            int idxOfIdx = indexes.indexOf(idx);

            if (idxOfIdx < 0) {
                return defaultValue;
            } else {
                int idxOfIdxAfter = idxOfIdx + 1;

                if (idxOfIdxAfter >= indexes.size()) {
                    return defaultValue;
                } else {
                    return indexes.get(idxOfIdxAfter);
                }
            }
        }
    }

    private static Integer findAndRemove(CharSequence chars, List<TemplateMatch> templateMatches, SortedMap<Integer, String> sortedPreviousIndexes) {
        String storeWithPrefix = "";

        return findAndRemove(chars, templateMatches, storeWithPrefix, sortedPreviousIndexes);
    }

    private static Integer findAndRemove(CharSequence chars, List<TemplateMatch> templateMatches, String storeWithPrefix, SortedMap<Integer, String> sortedPreviousIndexes) {
        int lastKnownIndex = sortedPreviousIndexes.isEmpty() ? 0 : sortedPreviousIndexes.lastKey();
        int veryLastIndex = templateMatches.size();

        return findAndRemove(chars, templateMatches, lastKnownIndex, veryLastIndex, storeWithPrefix, sortedPreviousIndexes);
    }

    private static Integer findAndRemove(CharSequence chars, List<TemplateMatch> templateMatches, final int fromIndex, final int toIndex, String storeWithPrefix, SortedMap<Integer, String> sortedPreviousIndexes) {
        // Search for best start index
        final float maxErr = maxLevenshteinError(chars);
        Integer bestStartIndex = null;
        Float bestStartIndexErr = null;
        for (int startIndex = fromIndex; startIndex < toIndex; startIndex++) {
            CharSequence scannedText = getScannedText(templateMatches, startIndex, chars.length());
            if (scannedText.length() > 0) {
                float err = MiscUtil.levenshteinError(chars.toString().replace("0", "O").replace("l", "I"), scannedText.toString().replace("0", "O").replace("l", "I"));
                if (err <= maxErr && (bestStartIndexErr == null || err < bestStartIndexErr)) {
                    bestStartIndex = startIndex;
                    bestStartIndexErr = err;
                }
            }
        }

        if (bestStartIndex == null) {
            return null; // No good match found
        } else {
            sortedPreviousIndexes.put(bestStartIndex, storeWithPrefix + chars);

            // Remove and learn chars belonging to the searched text.
            // Try to match as large regions of text as possible. Everything that remains must be learned.
            StringBuilder remainingReferenceText = new StringBuilder(chars);
            for (int matchLength = chars.length(); matchLength >= 1; matchLength--) {
                int possibleShifts = chars.length() - matchLength;
                for (int offsetInReferenceText = 0; offsetInReferenceText < possibleShifts; offsetInReferenceText++) {
                    CharSequence referenceText = getReferenceText(remainingReferenceText, offsetInReferenceText, matchLength);
                    if (referenceText != null) {
                        for (int offsetInTemplateMatches = bestStartIndex - 1; offsetInTemplateMatches < bestStartIndex + possibleShifts + 2; offsetInTemplateMatches++) { // -1/+2 to allow jitter
                            CharSequence scannedText = getScannedText(templateMatches, offsetInTemplateMatches, matchLength);
                            if (equalsIgnoreCaseAndDifficultChars(scannedText, referenceText)) {
                                // Insert ? for all matched chars in the remaining reference text.
                                // Set 'shouldHaveBeen' for all matched chars in the scanned text.
                                for (int i = 0; i < matchLength; i++) {
                                    char shouldHaveBeen = remainingReferenceText.charAt(offsetInReferenceText + i);
                                    remainingReferenceText.setCharAt(offsetInReferenceText + i, '?');
                                    templateMatches.get(offsetInTemplateMatches + i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                                }
                                // Once matched do not try to match at other offsets. This would steal chars.
                                break;
                            }
                        }
                    }
                }
            }

            // Now match any unmatched chars
            // referenceText:  CD-35 9019  -> ?D??? ????
            // scannedText:    CO-35 9O19  -> ?O??? ????
            final List<Integer> lookArounds = Arrays.asList(0, 1, -1, 2, -2);
            for (int idxInRef = 0; idxInRef < remainingReferenceText.length(); idxInRef++) {
                if (remainingReferenceText.charAt(idxInRef) != '?') {
                    for (int lookAround : lookArounds) {
                        int idxInScan = idxInRef + lookAround;
                        int k = bestStartIndex + idxInScan;
                        if (k >= 0 && k < templateMatches.size()) {
                            TemplateMatch match = templateMatches.get(k);
                            if (match != null && match.getShouldHaveBeen() == null) {
                                char shouldHaveBeen = remainingReferenceText.charAt(idxInRef);
                                remainingReferenceText.setCharAt(idxInRef, '?');
                                match.setShouldHaveBeen(Character.toString(shouldHaveBeen));
                                break;
                            }
                        }
                    }
                }
            }

            return bestStartIndex;
        }
    }

    private static boolean equalsIgnoreCaseAndDifficultChars(CharSequence s1, CharSequence s2) {
        if (s1.length() == 0 || s2.length() == 0) {
            return false;
        } else if (s1.length() != s2.length()) {
            return false;
        } else {
            // 0 and O are difficult to distinguish. Make everything the letter O.
            // Small l and capital I are difficult to distinguish. Make everything the capital letter I.
            String uppercasedReplaced1 = s1.toString().replace("0", "O").replace("l", "I").toUpperCase();
            String uppercasedReplaced2 = s2.toString().replace("0", "O").replace("l", "I").toUpperCase();

            return uppercasedReplaced1.equals(uppercasedReplaced2);
        }
    }

    private static CharSequence getReferenceText(CharSequence remainingReferenceText, int startIndex, int length) {
        StringBuilder referenceText = new StringBuilder();
        for (int j = 0; j < length; j++) {
            int k = startIndex + j;
            if (k >= 0 && k < remainingReferenceText.length()) {
                char c = remainingReferenceText.charAt(k);
                if (c == '?') {
                    return null; // No text of such length from the start index
                } else {
                    referenceText.append(c);
                }
            }
        }
        return referenceText;
    }

    private static CharSequence getScannedText(List<TemplateMatch> templateMatches, int startIndex, int length) {
        StringBuilder scannedText = new StringBuilder();
        for (int j = 0; j < length; j++) {
            int k = startIndex + j;
            if (k >= 0 && k < templateMatches.size()) {
                TemplateMatch match = templateMatches.get(k);
                if (match == null || match.getShouldHaveBeen() != null) {
                    break;
                } else {
                    scannedText.append(match.getTemplate().getText());
                }
            }
        }
        return scannedText;
    }

    private static float maxLevenshteinError(CharSequence chars) {
        if (chars.length() <= 2) {
            return 0.0f;
        } else if (chars.length() <= 3) {
            return 0.34f; // 1 of 3
        } else if (chars.length() <= 9) {
            return 0.25f; // 1 of 4
        } else {
            return 0.2f; // 1 of 5
        }
    }

    private static String allSeparatorsToThousands(String text) {
        return text.replace(".", ",");
    }

    private static String allSeparatorsToThousandsExceptLast(String text) {
        String fixedText = text.replace(".", ",");
        int lastSeparatorIndex = fixedText.lastIndexOf(",");
        if (lastSeparatorIndex >= 0 && lastSeparatorIndex < fixedText.length()) {
            fixedText = fixedText.substring(0, lastSeparatorIndex) + "." + fixedText.substring(lastSeparatorIndex + 1);
        }
        return fixedText;
    }

    private static String lastCharsToUnit(String text, String unit) {
        String fixedText = text;
        if (fixedText.length() >= unit.length()) {
            fixedText = fixedText.substring(0, fixedText.length() - unit.length()) + unit;
        }
        return fixedText;
    }

    private static BigDecimal fixAndRemoveEarthMasses(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: #,##0.0000
                BigDecimal min = new BigDecimal("0.0001"); // Screenshot: 2016-10-02 07-29-22 Dahan.png
                BigDecimal max = new BigDecimal("3274.6501"); // Screenshot: 2016-09-28 08-00-48 Arque.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                if (!fixedText.matches("(\\d{1,3})(,\\d{3})*\\.\\d{4}")) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String parseableText = fixedText.replace(",", ""); // Remove all thousands separators and units
                    BigDecimal value = new BigDecimal(parseableText);
                    if (value.compareTo(min) < 0) {
                        throw new NumberFormatException("Parsed value " + value + " is below the allowed minimum " + label.toLowerCase().replace(":", "") + " of " + min);
                    } else if (value.compareTo(max) > 0) {
                        throw new NumberFormatException("Parsed value " + value + " is above the allowed maximum " + label.toLowerCase().replace(":", "") + " of " + max);
                    } else {
                        // Set shouldHaveBeen, effectively removing the matches
                        for (int i = startIndex; i < endIndex; i++) {
                            char shouldHaveBeen = fixedText.charAt(i - startIndex);
                            matches.get(i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                        }
                        // Return the result
                        return value;
                    }
                }
            } catch (Exception e) {
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveRadius(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: #,##0KM
                BigDecimal min = new BigDecimal("282"); // Screenshot: 2016-10-02 07-29-22 Dahan.png
                BigDecimal max = new BigDecimal("77543"); // Screenshot: 2016-10-02 11-24-03 LHS 3505.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousands(fixedText);
                fixedText = lastCharsToUnit(fixedText, "KM");
                if (!fixedText.matches("(\\d{1,3},)?(\\d{3},)*\\d{3}KM")) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String parseableText = fixedText.replace(",", "").replace("KM", ""); // Remove all thousands separators and units
                    BigDecimal value = new BigDecimal(parseableText);
                    if (value.compareTo(min) < 0) {
                        throw new NumberFormatException("Parsed value " + value + " is below the allowed minimum " + label.toLowerCase().replace(":", "") + " of " + min);
                    } else if (value.compareTo(max) > 0) {
                        throw new NumberFormatException("Parsed value " + value + " is above the allowed maximum " + label.toLowerCase().replace(":", "") + " of " + max);
                    } else {
                        // Set shouldHaveBeen, effectively removing the matches
                        for (int i = startIndex; i < endIndex; i++) {
                            char shouldHaveBeen = fixedText.charAt(i - startIndex);
                            matches.get(i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                        }
                        // Return the result
                        return value;
                    }
                }
            } catch (Exception e) {
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveGravity(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: 0.00G
                BigDecimal min = new BigDecimal("0.02"); // Screenshot: 2016-09-28 07-43-24 Sol.png
                BigDecimal max = new BigDecimal("17.64"); // Screenshot: 2016-10-01 20-51-15 HIP 30953.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                fixedText = lastCharsToUnit(fixedText, "G");
                if (!fixedText.matches("\\d{1,2}\\.\\d{2}G")) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String parseableText = fixedText.replace(",", "").replace("G", ""); // Remove all thousands separators and units
                    BigDecimal value = new BigDecimal(parseableText);
                    if (value.compareTo(min) < 0) {
                        throw new NumberFormatException("Parsed value " + value + " is below the allowed minimum " + label.toLowerCase().replace(":", "") + " of " + min);
                    } else if (value.compareTo(max) > 0) {
                        throw new NumberFormatException("Parsed value " + value + " is above the allowed maximum " + label.toLowerCase().replace(":", "") + " of " + max);
                    } else {
                        // Set shouldHaveBeen, effectively removing the matches
                        for (int i = startIndex; i < endIndex; i++) {
                            char shouldHaveBeen = fixedText.charAt(i - startIndex);
                            matches.get(i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                        }
                        // Return the result
                        return value;
                    }
                }
            } catch (Exception e) {
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveSurfaceTemp(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: #,##0K
                BigDecimal min = new BigDecimal("29"); // Screenshot: 2016-10-01 22-08-03 Devataru.png
                BigDecimal max = new BigDecimal("3286"); // Screenshot: 2016-10-02 07-15-50 Aulis.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousands(fixedText);
                fixedText = lastCharsToUnit(fixedText, "K");
                if (!fixedText.matches("(\\d{1,3})(,\\d{3})*K")) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String parseableText = fixedText.replace(",", "").replace("K", ""); // Remove all thousands separators and units
                    BigDecimal value = new BigDecimal(parseableText);
                    if (value.compareTo(min) < 0) {
                        throw new NumberFormatException("Parsed value " + value + " is below the allowed minimum " + label.toLowerCase().replace(":", "") + " of " + min);
                    } else if (value.compareTo(max) > 0) {
                        throw new NumberFormatException("Parsed value " + value + " is above the allowed maximum " + label.toLowerCase().replace(":", "") + " of " + max);
                    } else {
                        // Set shouldHaveBeen, effectively removing the matches
                        for (int i = startIndex; i < endIndex; i++) {
                            char shouldHaveBeen = fixedText.charAt(i - startIndex);
                            matches.get(i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                        }
                        // Return the result
                        return value;
                    }
                }
            } catch (Exception e) {
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveAxialTilt(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: 0.00°
                BigDecimal min = new BigDecimal("-100"); // Screenshot: TODO
                BigDecimal max = new BigDecimal("171.06"); // Screenshot: 2016-10-01 22-08-03 Devataru.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                fixedText = lastCharsToUnit(fixedText, "°");
                if (!fixedText.matches("\\-?\\d{1,3}\\.\\d{2}°")) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String parseableText = fixedText.replace(",", "").replace("°", ""); // Remove all thousands separators and units
                    BigDecimal value = new BigDecimal(parseableText);
                    if (value.compareTo(min) < 0) {
                        throw new NumberFormatException("Parsed value " + value + " is below the allowed minimum " + label.toLowerCase().replace(":", "") + " of " + min);
                    } else if (value.compareTo(max) > 0) {
                        throw new NumberFormatException("Parsed value " + value + " is above the allowed maximum " + label.toLowerCase().replace(":", "") + " of " + max);
                    } else {
                        // Set shouldHaveBeen, effectively removing the matches
                        for (int i = startIndex; i < endIndex; i++) {
                            char shouldHaveBeen = fixedText.charAt(i - startIndex);
                            matches.get(i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                        }
                        // Return the result
                        return value;
                    }
                }
            } catch (Exception e) {
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label + " -- " + e);
            }
        }
        return null;
    }

}
