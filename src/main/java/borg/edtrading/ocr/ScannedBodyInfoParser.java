package borg.edtrading.ocr;

import borg.edtrading.Constants;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import borg.edtrading.data.Body;
import borg.edtrading.data.BodyInfo;
import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.data.ScannedBodyInfo;
import borg.edtrading.util.MatchSorter.MatchGroup;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.imageio.ImageIO;

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

        ScannedBodyInfo scannedBodyInfo = new ScannedBodyInfo(screenshotFilename, systemName);

        List<TemplateMatch> bodyInfoMatches = new ArrayList<>();
        for (MatchGroup mg : bodyInfoWords) {
            bodyInfoMatches.addAll(mg.getGroupMatches());
        }

        // Store the indexes of all labels. This allows us to later extract the values. In most cases we have label, value, label, value, ...
        SortedMap<Integer, String> sortedLabelIndexes = new TreeMap<>();

        // The first expected label is the body mass. This also allows us to decide which group the body belongs to.
        // The body group (star/planet/belt/rings) then determines which labels/values can occur.
        if (findAndRemove("SOLARMASSES:", bodyInfoMatches, sortedLabelIndexes) != null) {
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_STAR);
        } else if (findAndRemove("EARTHMASSES:", bodyInfoMatches, sortedLabelIndexes) != null) {
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_PLANET);
        } else if (findAndRemove("MOONMASSES:", bodyInfoMatches, sortedLabelIndexes) != null) {
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_BELT);
        } else {
            // None of the three above most likely means a scrolled-down screenshot of the rings of a body.
            // Could be a planet, but could also be a dwarf star...
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_RINGS);
        }

        // The other labels depend on the body group.
        if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_STAR) {
            // TODO Parse star class from description text
            for (BodyInfo bi : BodyInfo.byPrefix("RESERVES_")) {
                String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                if (findAndRemove(nameWithoutSpaces, bodyInfoMatches, 0, indexOf("SOLARMASSES:", sortedLabelIndexes), "RESERVES_", sortedLabelIndexes) != null) { // Reserves unfortunately is before solar masses, so explicitly search from index 0...
                    scannedBodyInfo.setSystemReserves(bi);
                    break; // Expect only one hit
                }
            }
            if (findAndRemove("AGE:", bodyInfoMatches, 0, indexOf("SOLARMASSES:", sortedLabelIndexes), "", sortedLabelIndexes) != null) { // Age unfortunately is before solar masses, so explicitly search from index 0...
                findAndRemove("MILLIONYEARS", bodyInfoMatches, 0, indexOf("SOLARMASSES:", sortedLabelIndexes), "", sortedLabelIndexes); // Remove the unit of age because it is long
            }
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
        } else if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_PLANET) {
            for (BodyInfo bi : BodyInfo.byPrefix("RESERVES_")) {
                String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                if (findAndRemove(nameWithoutSpaces, bodyInfoMatches, 0, indexOf("EARTHMASSES:", sortedLabelIndexes), "RESERVES_", sortedLabelIndexes) != null) { // Reserves unfortunately is before earth masses, so explicitly search from index 0...
                    scannedBodyInfo.setSystemReserves(bi);
                    break; // Expect only one hit
                }
            }
            findAndRemove("RADIUS:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("GRAVITY:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("SURFACETEMP:", bodyInfoMatches, sortedLabelIndexes);
            if (findAndRemove("SURFACEPRESSURE:", bodyInfoMatches, sortedLabelIndexes) != null) {
                findAndRemove("ATMOSPHERES", bodyInfoMatches, sortedLabelIndexes); // Remove the unit of surface pressure because it is long and may collide with later labels
            }
            findAndRemove("VOLCANISM:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ATMOSPHERETYPE:", bodyInfoMatches, sortedLabelIndexes);
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
        } else if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_BELT) {
            for (BodyInfo bi : BodyInfo.byPrefix("RESERVES_")) {
                String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                if (findAndRemove(nameWithoutSpaces, bodyInfoMatches, 0, indexOf("MOONMASSES:", sortedLabelIndexes), "RESERVES_", sortedLabelIndexes) != null) { // Reserves unfortunately is before moon masses, so explicitly search from index 0...
                    scannedBodyInfo.setSystemReserves(bi);
                    break; // Expect only one hit
                }
            }
            findAndRemove("RINGTYPE:", bodyInfoMatches, 0, indexOf("MOONMASSES:", sortedLabelIndexes), "", sortedLabelIndexes); // Ring type unfortunately is before moon masses, so explicitly search from index 0...
            findAndRemove("ORBITALPERIOD:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("SEMIMAJORAXIS:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ORBITALECCENTRICITY:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ORBITALINCLINATION:", bodyInfoMatches, sortedLabelIndexes);
            findAndRemove("ARGOFPERIAPSIS:", bodyInfoMatches, sortedLabelIndexes);
        }

        if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_RINGS || scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_PLANET || scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_STAR) {
            // Either a rings-only screenshot, or a body which can have rings
            // TODO Scan rings
        }

        // Now that we have all labels we can start to parse the values
        if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_STAR) {
            scannedBodyInfo.setAgeMillionYears(fixAndRemoveAge("AGE:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setSolarMasses(fixAndRemoveSolarMasses("SOLARMASSES:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setSolarRadius(fixAndRemoveSolarRadius("SOLARRADIUS:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setSurfaceTempK(fixAndRemoveSurfaceTemp("SURFACETEMP:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setOrbitalPeriodD(fixAndRemoveOrbitalPeriod("ORBITALPERIOD:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setSemiMajorAxisAU(fixAndRemoveSemiMajorAxis("SEMIMAJORAXIS:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setOrbitalEccentricity(fixAndRemoveOrbitalEccentricity("ORBITALECCENTRICITY:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setOrbitalInclinationDeg(fixAndRemoveOrbitalInclination("ORBITALINCLINATION:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setArgOfPeriapsisDeg(fixAndRemoveArgOfPeriapsis("ARGOFPERIAPSIS:", bodyInfoMatches, sortedLabelIndexes));
            // TODO Catalogue IDs...
        } else if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_PLANET) {
            scannedBodyInfo.setEarthMasses(fixAndRemoveEarthMasses("EARTHMASSES:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setRadiusKm(fixAndRemoveRadius("RADIUS:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setGravityG(fixAndRemoveGravity("GRAVITY:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setSurfaceTempK(fixAndRemoveSurfaceTemp("SURFACETEMP:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setSurfacePressureAtmospheres(fixAndRemoveSurfacePressure("SURFACEPRESSURE:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setVolcanism(fixAndRemoveVolcanism("VOLCANISM:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setAtmosphereType(fixAndRemoveAtmosphereType("ATMOSPHERETYPE:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setAtmosphere(fixAndRemoveComposition("ATMOSPHERE:", "ATMOSPHERE_COMPONENT_", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setComposition(fixAndRemoveComposition("COMPOSITION:", "COMPOSITION_", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setOrbitalPeriodD(fixAndRemoveOrbitalPeriod("ORBITALPERIOD:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setSemiMajorAxisAU(fixAndRemoveSemiMajorAxis("SEMIMAJORAXIS:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setOrbitalEccentricity(fixAndRemoveOrbitalEccentricity("ORBITALECCENTRICITY:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setOrbitalInclinationDeg(fixAndRemoveOrbitalInclination("ORBITALINCLINATION:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setArgOfPeriapsisDeg(fixAndRemoveArgOfPeriapsis("ARGOFPERIAPSIS:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setRotationalPeriodD(fixAndRemoveRotationalPeriod("ROTATIONALPERIOD:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setTidallyLocked(scannedBodyInfo.getRotationalPeriodD() == null ? null : (indexOf("(TIDALLYLOCKED)", sortedLabelIndexes) != null));
            scannedBodyInfo.setAxialTiltDeg(fixAndRemoveAxialTilt("AXIALTILT:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setPlanetMaterials(fixAndRemovePlanetMaterials("PLANETMATERIALS:", "PLANET_MATERIAL_", bodyInfoMatches, sortedLabelIndexes));
        } else if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_BELT) {
            scannedBodyInfo.setRingType(fixAndRemoveRingType("RINGTYPE:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setMoonMasses(fixAndRemoveMoonMasses("MOONMASSES:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setOrbitalPeriodD(fixAndRemoveOrbitalPeriod("ORBITALPERIOD:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setSemiMajorAxisAU(fixAndRemoveSemiMajorAxis("SEMIMAJORAXIS:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setOrbitalEccentricity(fixAndRemoveOrbitalEccentricity("ORBITALECCENTRICITY:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setOrbitalInclinationDeg(fixAndRemoveOrbitalInclination("ORBITALINCLINATION:", bodyInfoMatches, sortedLabelIndexes));
            scannedBodyInfo.setArgOfPeriapsisDeg(fixAndRemoveArgOfPeriapsis("ARGOFPERIAPSIS:", bodyInfoMatches, sortedLabelIndexes));
        } else if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_RINGS) {
            // TODO ...
        }

        // TODO Remove this debug output
        //        System.out.print("Scanned: ");
        //        for (TemplateMatch m : bodyInfoMatches) {
        //            System.out.print("<" + m.getTemplate().getText() + ">");
        //        }
        //        System.out.println();
        //        System.out.print("Parsed:  ");
        //        for (TemplateMatch m : bodyInfoMatches) {
        //            System.out.print("<" + (m.getShouldHaveBeen() == null ? "?" : m.getShouldHaveBeen()) + ">");
        //        }
        //        System.out.println();

        learnWronglyDetectedChars(bodyInfoMatches);

        return scannedBodyInfo;
    }

    private static void learnWronglyDetectedChars(List<TemplateMatch> fixedMatches) {
        for (TemplateMatch m : fixedMatches) {
            if (m != null && m.getShouldHaveBeen() != null) {
                // We know what it should have been
                if (!m.getShouldHaveBeen().equals(m.getTemplate().getText())) {
                    // It is NOT what it should have been
                    if (!is0vsO(m.getShouldHaveBeen(), m.getTemplate().getText()) || Constants.LEARN_0_VS_O) {
                        // It is totally wrong, or we are allowed to learn difficult chars like 0<->O
                        String folderName = TemplateMatcher.textToFolder(m.getShouldHaveBeen());
                        File autoLearnFolder = new File(Constants.AUTO_LEARNED_DIR, folderName);
                        autoLearnFolder.mkdirs();
                        try {
                            ImageIO.write(m.getSubimage(), "PNG", new File(autoLearnFolder, "LEARNED#" + folderName + "#" + m.getMatch().x + "#" + m.getMatch().y + "#" + currentScreenshotFilename));
                            logger.trace("Learned new '" + m.getShouldHaveBeen() + "' from " + currentScreenshotFilename);
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
        }
    }

    private static boolean is0vsO(String shouldHaveBeen, String actuallyIs) {
        boolean is0vsO = ("0".equals(shouldHaveBeen) && "O".equals(actuallyIs)) || ("O".equals(shouldHaveBeen) && "0".equals(actuallyIs));
        boolean isIvsl = ("I".equals(shouldHaveBeen) && "l".equals(actuallyIs)) || ("l".equals(shouldHaveBeen) && "I".equals(actuallyIs));

        return is0vsO || isIvsl;
    }

    private static Integer indexOf(String label, SortedMap<Integer, String> sortedLabelIndexes) {
        for (Integer index : sortedLabelIndexes.keySet()) {
            if (sortedLabelIndexes.get(index).equals(label)) {
                return index;
            }
        }

        return null;
    }

    private static int indexBefore(Integer idx, SortedMap<Integer, String> sortedLabelIndexes, int defaultValue) {
        if (idx == null || sortedLabelIndexes.isEmpty()) {
            return defaultValue;
        } else {
            List<Integer> indexes = new ArrayList<>(sortedLabelIndexes.keySet());
            int idxOfIdx = indexes.indexOf(idx);

            if (idxOfIdx < 0) {
                return defaultValue;
            } else {
                int idxOfIdxBefore = idxOfIdx - 1;

                if (idxOfIdxBefore < 0) {
                    return defaultValue;
                } else {
                    return indexes.get(idxOfIdxBefore);
                }
            }
        }
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
                    // If the searched sequence ends with : and the found does NOT end with a punctuation, then skip!
                    // Reason: Searching for <NOATMOSPHERE> value might steal <ATMOSPHERE:> label.
                    if (chars.toString().endsWith(":") && !scannedText.toString().matches(".+[:,'°\\.]$")) {
                        // Skip!
                    } else {
                        bestStartIndex = startIndex;
                        bestStartIndexErr = err;
                    }
                }
            }
        }

        if (bestStartIndex == null) {
            return null; // No good match found
        } else {
            sortedPreviousIndexes.put(bestStartIndex, storeWithPrefix + chars);

            // Remove and learn chars belonging to the searched text.
            // Try to match as large regions of text as possible. Everything that remains must be learned.
            // Lengths < 3 chars do not make sense.
            StringBuilder remainingReferenceText = new StringBuilder(chars);
            for (int matchLength = chars.length(); matchLength >= 3; matchLength--) {
                int possibleShifts = chars.length() - matchLength;
                for (int offsetInReferenceText = 0; offsetInReferenceText <= possibleShifts; offsetInReferenceText++) {
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
        if (fixedText.length() > unit.length()) {
            // Only fix if the last scanned chars are not digits
            String scannedUnit = fixedText.substring(fixedText.length() - unit.length());
            if (scannedUnit.matches("[^\\d\\.,]+")) {
                fixedText = fixedText.substring(0, fixedText.length() - unit.length()) + unit;
            }
        }
        return fixedText;
    }

    private static String maybeFirstCharToSign(String text) {
        String fixedText = text;
        if (fixedText.length() > 1) {
            // Only fix if the first scanned char is not a digit
            String scannedSign = fixedText.substring(0, 1);
            if (scannedSign.matches("[^\\d]")) {
                fixedText = "-" + fixedText.substring(1);
            }
        }
        return fixedText;
    }

    private static BigDecimal fixAndRemoveAge(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: #,##0
                BigDecimal min = new BigDecimal("42"); // Screenshot: 2016-10-03 11-50-29 Arare.png
                BigDecimal max = new BigDecimal("12986"); // Screenshot: 2016-09-30 17-10-03 Flech.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousands(fixedText);
                if (!fixedText.matches("(\\d{1,3})(,\\d{3})*")) {
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveSolarMasses(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal min = new BigDecimal("0.0273"); // Screenshot: 2016-10-01 20-50-34 HIP 30953.png
                BigDecimal max = new BigDecimal("1.7891"); // Screenshot: 2016-10-03 13-26-06 76 Sigma Ceti.png
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveSolarRadius(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal min = new BigDecimal("0.1148"); // Screenshot: 2016-10-01 20-50-34 HIP 30953.png
                BigDecimal max = new BigDecimal("1.4673"); // Screenshot: 2016-10-03 13-26-06 76 Sigma Ceti.png
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
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
                BigDecimal max = new BigDecimal("78930"); // Screenshot: 2016-09-30 17-22-30 Deciat.png
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
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
                BigDecimal max = new BigDecimal("20.61"); // Screenshot: 2016-10-03 22-58-36 FT Ceti.png
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
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
                // Pattern: #,##0K (planets) or #,##0.00K (stars)
                BigDecimal min = new BigDecimal("20"); // Screenshot: 2016-10-01 22-22-54 Damocan.png
                BigDecimal max = new BigDecimal("8431"); // Screenshot: 2016-10-03 13-26-06 76 Sigma Ceti.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                // Cannot do this because stars have decimal places... fixedText = allSeparatorsToThousands(fixedText);
                fixedText = lastCharsToUnit(fixedText, "K");
                if (!fixedText.matches("(\\d{1,3})(,\\d{3})*K") && !fixedText.matches("(\\d{1,3})(,\\d{3})*\\.\\d{2}K")) {
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveSurfacePressure(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: #,##0.00
                BigDecimal min = new BigDecimal("0.00"); // Screenshot: 2016-09-28 07-44-10 Sol.png
                BigDecimal max = new BigDecimal("32392.04"); // Screenshot: 2016-09-28 07-46-44 LHS 380.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                if (!fixedText.matches("(\\d{1,3})(,\\d{3})*\\.\\d{2}")) {
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BodyInfo fixAndRemoveVolcanism(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                BodyInfo result = null;
                List<BodyInfo> enums = BodyInfo.byPrefix("VOLCANISM_");
                BodyInfo bestEnum = BodyInfo.findBestMatching(scannedText.toString().replace("0", "O").replace("l", "I"), "VOLCANISM_");
                if (bestEnum != null) {
                    // Try best one first
                    enums.remove(bestEnum);
                    enums.add(0, bestEnum);
                }
                for (BodyInfo bi : enums) {
                    String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                    if (findAndRemove(nameWithoutSpaces, matches, startIndex, endIndex, "VOLCANISM_", sortedLabelIndexes) != null) {
                        result = bi;
                        break; // Expect only one hit
                    }
                }
                if (result == null) {
                    throw new NumberFormatException("Did not find any matching enum");
                } else {
                    String fixedText = result.getName().replaceAll("\\s", "");
                    // Set shouldHaveBeen, effectively removing the matches
                    for (int i = startIndex; i < endIndex; i++) {
                        char shouldHaveBeen = fixedText.charAt(i - startIndex);
                        matches.get(i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                    }
                    // Return the result
                    return result;
                }
            } catch (Exception e) {
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BodyInfo fixAndRemoveAtmosphereType(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                BodyInfo result = null;
                List<BodyInfo> enums = BodyInfo.byPrefix("ATMOSPHERE_TYPE_");
                BodyInfo bestEnum = BodyInfo.findBestMatching(scannedText.toString().replace("0", "O").replace("l", "I"), "ATMOSPHERE_TYPE_");
                if (bestEnum != null) {
                    // Try best one first
                    enums.remove(bestEnum);
                    enums.add(0, bestEnum);
                }
                for (BodyInfo bi : enums) {
                    String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                    if (findAndRemove(nameWithoutSpaces, matches, startIndex, endIndex, "ATMOSPHERE_TYPE_", sortedLabelIndexes) != null) {
                        result = bi;
                        break; // Expect only one hit
                    }
                }
                if (result == null) {
                    throw new NumberFormatException("Did not find any matching enum");
                } else {
                    String fixedText = result.getName().replaceAll("\\s", "");
                    // Set shouldHaveBeen, effectively removing the matches
                    for (int i = startIndex; i < endIndex; i++) {
                        char shouldHaveBeen = fixedText.charAt(i - startIndex);
                        matches.get(i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                    }
                    // Return the result
                    return result;
                }
            } catch (Exception e) {
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveOrbitalPeriod(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: #,##0.0D
                BigDecimal min = new BigDecimal("0.2"); // Screenshot: 2016-09-28 07-58-40 Har Itari.png
                BigDecimal max = new BigDecimal("7350826.0"); // Screenshot: 2016-09-30 16-57-43 HIP 2453.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                fixedText = lastCharsToUnit(fixedText, "D");
                if (!fixedText.matches("(\\d{1,3})(,\\d{3})*\\.\\d{1}D")) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String parseableText = fixedText.replace(",", "").replace("D", ""); // Remove all thousands separators and units
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveSemiMajorAxis(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: 0.00AU
                BigDecimal min = new BigDecimal("0.00"); // Screenshot: 2016-10-02 07-32-11 Wyrd.png
                BigDecimal max = new BigDecimal("564.58"); // Screenshot: 2016-09-30 16-57-43 HIP 2453.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                fixedText = lastCharsToUnit(fixedText, "AU");
                if (!fixedText.matches("\\d{1,3}\\.\\d{2}AU")) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String parseableText = fixedText.replace(",", "").replace("AU", ""); // Remove all thousands separators and units
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveOrbitalEccentricity(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: 0.0000
                BigDecimal min = new BigDecimal("0.0000"); // Screenshot: 2016-10-03 08-40-26 V2689 Orionis.png
                BigDecimal max = new BigDecimal("0.3995"); // Screenshot: 2016-09-28 07-47-11 LHS 380.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                if (!fixedText.matches("\\d{1,3}\\.\\d{4}")) {
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveOrbitalInclination(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: 0.00° (-180°..+180°)
                BigDecimal min = new BigDecimal("-180.00"); // Screenshot: ?
                BigDecimal max = new BigDecimal("180.00"); // Screenshot: ?
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                fixedText = lastCharsToUnit(fixedText, "°");
                fixedText = maybeFirstCharToSign(fixedText);
                if (!fixedText.matches("\\-?\\d{1,3}\\.\\d{2}°?")) {
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveArgOfPeriapsis(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: 0.00° (0°..360°)
                BigDecimal min = new BigDecimal("0.00"); // Screenshot: ?
                BigDecimal max = new BigDecimal("360.00"); // Screenshot: ?
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                fixedText = lastCharsToUnit(fixedText, "°");
                if (!fixedText.matches("\\d{1,3}\\.\\d{2}°?")) {
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveRotationalPeriod(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: 0.0D
                BigDecimal min = new BigDecimal("0.2"); // Screenshot: 2016-09-28 07-58-40 Har Itari.png
                BigDecimal max = new BigDecimal("307.0"); // Screenshot: 2016-10-02 20-10-19 Ross 720.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                fixedText = lastCharsToUnit(fixedText, "D");
                if (!fixedText.matches("\\d{1,3}\\.\\d{1}D")) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String parseableText = fixedText.replace(",", "").replace("D", ""); // Remove all thousands separators and units
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
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
                // Pattern: 0.00° (-180°..+180°)
                BigDecimal min = new BigDecimal("-180.00"); // Screenshot: ?
                BigDecimal max = new BigDecimal("180.00"); // Screenshot: ?
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                fixedText = lastCharsToUnit(fixedText, "°");
                fixedText = maybeFirstCharToSign(fixedText);
                if (!fixedText.matches("\\-?\\d{1,3}\\.\\d{2}°?")) {
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BodyInfo fixAndRemoveRingType(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                BodyInfo result = null;
                List<BodyInfo> enums = BodyInfo.byPrefix("RING_TYPE_");
                BodyInfo bestEnum = BodyInfo.findBestMatching(scannedText.toString().replace("0", "O").replace("l", "I"), "RING_TYPE_");
                if (bestEnum != null) {
                    // Try best one first
                    enums.remove(bestEnum);
                    enums.add(0, bestEnum);
                }
                for (BodyInfo bi : enums) {
                    String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                    if (findAndRemove(nameWithoutSpaces, matches, startIndex, endIndex, "RING_TYPE_", sortedLabelIndexes) != null) {
                        result = bi;
                        break; // Expect only one hit
                    }
                }
                if (result == null) {
                    throw new NumberFormatException("Did not find any matching enum");
                } else {
                    String fixedText = result.getName().replaceAll("\\s", "");
                    // Set shouldHaveBeen, effectively removing the matches
                    for (int i = startIndex; i < endIndex; i++) {
                        char shouldHaveBeen = fixedText.charAt(i - startIndex);
                        matches.get(i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                    }
                    // Return the result
                    return result;
                }
            } catch (Exception e) {
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static BigDecimal fixAndRemoveMoonMasses(String label, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal min = new BigDecimal("0.0000"); // Screenshot: 2016-09-30 17-07-13 Uchaluroja.png
                BigDecimal max = new BigDecimal("2.2658"); // Screenshot: 2016-09-30 16-56-39 HIP 2453.png
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
                logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
            }
        }
        return null;
    }

    private static LinkedHashMap<BodyInfo, BigDecimal> fixAndRemoveComposition(String label, String prefix, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        LinkedHashMap<BodyInfo, BigDecimal> result = new LinkedHashMap<>();
        BigDecimal totalSumPercent = BigDecimal.ZERO;
        if (indexOf(label, sortedLabelIndexes) != null) {
            for (BodyInfo bi : BodyInfo.byPrefix(prefix)) {
                String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                Integer enumIndex = indexOf(prefix + nameWithoutSpaces, sortedLabelIndexes);
                if (enumIndex != null) {
                    Integer startIndex = null;
                    Integer endIndex = null;
                    StringBuilder scannedText = new StringBuilder();
                    for (int i = indexBefore(enumIndex, sortedLabelIndexes, 0); i < enumIndex; i++) {
                        if (matches.get(i).getShouldHaveBeen() == null) {
                            scannedText.append(matches.get(i).getTemplate().getText());
                            if (startIndex == null) {
                                startIndex = i;
                            }
                            endIndex = i + 1;
                        }
                    }
                    try {
                        // Pattern: 0.0%
                        BigDecimal min = new BigDecimal("0.0");
                        BigDecimal max = new BigDecimal("100.0");
                        String fixedText = scannedText.toString();
                        fixedText = fixedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                        fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                        fixedText = lastCharsToUnit(fixedText, "%");
                        if (!fixedText.matches("\\d{1,3}\\.\\d{1}%")) {
                            throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                        } else {
                            String parseableText = fixedText.replace(",", "").replace("%", ""); // Remove all thousands separators and units
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
                                // Add to result
                                result.put(bi, value);
                                // Sum up
                                totalSumPercent = totalSumPercent.add(value);
                            }
                        }
                    } catch (Exception e) {
                        logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
                    }
                }
            }
        }
        MiscUtil.sortMapByValueReverse(result);
        if (totalSumPercent.doubleValue() == 0.0) {
            return null;
        } else if (Math.abs(100.0 - totalSumPercent.doubleValue()) >= 1.0) {
            logger.debug(currentScreenshotFilename + ": Trying to fix sum of " + label.toLowerCase().replace(":", "") + ". Currently is " + totalSumPercent + ": " + result);
            LinkedHashMap<String, BigDecimal> wrongPercentages = new LinkedHashMap<>();
            for (BodyInfo bi : result.keySet()) {
                wrongPercentages.put(bi.getName(), result.get(bi));
            }
            LinkedHashMap<String, BigDecimal> fixedPercentages = fixPercentagesTo100(wrongPercentages);
            if (fixedPercentages == null) {
                return null;
            } else {
                logger.info(
                        currentScreenshotFilename + ": Fixed sum of " + label.toLowerCase().replace(":", "") + " from " + sumOfPercentages(wrongPercentages) + "% " + wrongPercentages + " to " + sumOfPercentages(fixedPercentages) + "% " + fixedPercentages);
                // Find the fixed one
                String fixedOne = null;
                for (String name : fixedPercentages.keySet()) {
                    if (!fixedPercentages.get(name).equals(wrongPercentages.get(name))) {
                        fixedOne = name;
                        break;
                    }
                }
                // Get the wrong and the fixed value
                String wrongValue = wrongPercentages.get(fixedOne).toString() + "%";
                String fixedValue = fixedPercentages.get(fixedOne).toString() + "%";
                // Fix it in the scan
                for (int i = indexOf(label, sortedLabelIndexes); i < matches.size() - wrongValue.length(); i++) {
                    StringBuilder sb = new StringBuilder();
                    for (int j = 0; j < wrongValue.length(); j++) {
                        sb.append(matches.get(i + j).getShouldHaveBeen());
                    }
                    if (sb.toString().equals(wrongValue)) {
                        for (int j = 0; j < wrongValue.length(); j++) {
                            matches.get(i + j).setShouldHaveBeen(Character.toString(fixedValue.charAt(j)));
                        }
                        break;
                    }
                }
                // Fix it in the result
                result = new LinkedHashMap<>();
                for (String name : fixedPercentages.keySet()) {
                    BodyInfo bi = BodyInfo.findBestMatching(name, prefix);
                    BigDecimal v = fixedPercentages.get(name);
                    result.put(bi, v);
                }
                MiscUtil.sortMapByValueReverse(result);
                // Return the fixed result
                return result;
            }
        } else if (Math.abs(100.0 - totalSumPercent.doubleValue()) > 0.5) {
            logger.warn(currentScreenshotFilename + ": Sum of " + label.toLowerCase().replace(":", "") + " is " + totalSumPercent + ": " + result);
            return null;
        } else {
            return result;
        }
    }

    private static LinkedHashMap<Item, BigDecimal> fixAndRemovePlanetMaterials(String label, String prefix, List<TemplateMatch> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        LinkedHashMap<Item, BigDecimal> result = new LinkedHashMap<>();
        BigDecimal totalSumPercent = BigDecimal.ZERO;
        if (indexOf(label, sortedLabelIndexes) != null) {
            for (Item el : Item.byType(ItemType.ELEMENT)) {
                String nameWithoutSpaces = el.getName().replaceAll("\\s", "");
                Integer enumIndex = indexOf(prefix + nameWithoutSpaces, sortedLabelIndexes);
                if (enumIndex != null) {
                    Integer startIndex = null;
                    Integer endIndex = null;
                    StringBuilder scannedText = new StringBuilder();
                    for (int i = enumIndex; i < indexAfter(enumIndex, sortedLabelIndexes, matches.size()); i++) {
                        if (matches.get(i).getShouldHaveBeen() == null) {
                            scannedText.append(matches.get(i).getTemplate().getText());
                            if (startIndex == null) {
                                startIndex = i;
                            }
                            endIndex = i + 1;
                        }
                    }
                    try {
                        // Pattern: (0.0%),
                        BigDecimal min = new BigDecimal("0.0");
                        BigDecimal max = new BigDecimal("100.0");
                        String fixedText = fixPlanetMaterialPercentage(scannedText.toString());
                        if (!fixedText.matches("\\(\\d{1,3}\\.\\d{1}%\\),?")) {
                            throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                        } else {
                            String parseableText = fixedText.replace(",", "").replace("%", "").replace("(", "").replace(")", ""); // Remove all thousands separators and units
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
                                // Add to result
                                result.put(el, value);
                                // Sum up
                                totalSumPercent = totalSumPercent.add(value);
                            }
                        }
                    } catch (Exception e) {
                        logger.warn(currentScreenshotFilename + ": Scanned text '" + scannedText + "' does not look like a valid value for " + label.toLowerCase().replace(":", "") + " -- " + e);
                    }
                }
            }
        }
        MiscUtil.sortMapByValueReverse(result);
        if (totalSumPercent.doubleValue() == 0.0) {
            return null;
        } else if (Math.abs(100.0 - totalSumPercent.doubleValue()) >= 1.0) {
            logger.debug(currentScreenshotFilename + ": Trying to fix sum of " + label.toLowerCase().replace(":", "") + ". Currently is " + totalSumPercent + ": " + result);
            LinkedHashMap<String, BigDecimal> wrongPercentages = new LinkedHashMap<>();
            for (Item el : result.keySet()) {
                wrongPercentages.put(el.getName(), result.get(el));
            }
            LinkedHashMap<String, BigDecimal> fixedPercentages = fixPercentagesTo100(wrongPercentages);
            if (fixedPercentages == null) {
                return null;
            } else {
                logger.info(
                        currentScreenshotFilename + ": Fixed sum of " + label.toLowerCase().replace(":", "") + " from " + sumOfPercentages(wrongPercentages) + "% " + wrongPercentages + " to " + sumOfPercentages(fixedPercentages) + "% " + fixedPercentages);
                // Find the fixed one
                String fixedOne = null;
                for (String name : fixedPercentages.keySet()) {
                    if (!fixedPercentages.get(name).equals(wrongPercentages.get(name))) {
                        fixedOne = name;
                        break;
                    }
                }
                // Get the wrong and the fixed value
                String wrongValue = wrongPercentages.get(fixedOne).toString() + "%";
                String fixedValue = fixedPercentages.get(fixedOne).toString() + "%";
                // Fix it in the scan
                for (int i = indexOf(label, sortedLabelIndexes); i < matches.size() - wrongValue.length(); i++) {
                    StringBuilder sb = new StringBuilder();
                    for (int j = 0; j < wrongValue.length(); j++) {
                        sb.append(matches.get(i + j).getShouldHaveBeen());
                    }
                    if (sb.toString().equals(wrongValue)) {
                        for (int j = 0; j < wrongValue.length(); j++) {
                            matches.get(i + j).setShouldHaveBeen(Character.toString(fixedValue.charAt(j)));
                        }
                        break;
                    }
                }
                // Fix it in the result
                result = new LinkedHashMap<>();
                for (String name : fixedPercentages.keySet()) {
                    Item el = Item.findBestMatching(name, ItemType.ELEMENT);
                    BigDecimal v = fixedPercentages.get(name);
                    result.put(el, v);
                }
                MiscUtil.sortMapByValueReverse(result);
                // Return the fixed result
                return result;
            }
        } else if (Math.abs(100.0 - totalSumPercent.doubleValue()) > 0.5) {
            logger.warn(currentScreenshotFilename + ": Sum of " + label.toLowerCase().replace(":", "") + " is " + totalSumPercent + ": " + result);
            return null;
        } else {
            return result;
        }
    }

    private static String fixPlanetMaterialPercentage(String scannedText) {
        String fixedText = scannedText.replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
        fixedText = fixedText.replace(",", "."); // All to thousands
        boolean removedTrailingComma = false;
        if (fixedText.substring(fixedText.length() - 1, fixedText.length()).equals(".")) {
            fixedText = fixedText.substring(0, fixedText.length() - 1);
            removedTrailingComma = true;
        }
        if (fixedText.substring(0, 1).matches("[^\\d]+")) {
            fixedText = "(" + fixedText.substring(1, fixedText.length()); // Expect first char to be opening bracket
        }
        if (fixedText.substring(fixedText.length() - 2, fixedText.length()).matches("[^\\d]+")) {
            fixedText = fixedText.substring(0, fixedText.length() - 2) + "%)"; // Expect last two chars to be percent sign and closing bracket
        }
        if (removedTrailingComma) {
            fixedText = fixedText + ",";
        }
        return fixedText;
    }

    private static LinkedHashMap<String, BigDecimal> fixPercentagesTo100(LinkedHashMap<String, BigDecimal> wrongPercentages) {
        LinkedHashMap<String, BigDecimal> fixedPercentages = new LinkedHashMap<>(wrongPercentages);

        // 6, 8 and 9 can be messed up.
        // Try to fix the sum by changing ONE (only ONE!) of them.
        for (String name : wrongPercentages.keySet()) {
            BigDecimal scannedPercentage = wrongPercentages.get(name);

            if (scannedPercentage.toString().contains("6")) {
                fixedPercentages.put(name, new BigDecimal(scannedPercentage.toString().replaceFirst("6", "8")));
                if (sumOfPercentages(fixedPercentages).doubleValue() == 100.0) {
                    return fixedPercentages; // I fixed it!
                }
                fixedPercentages.put(name, new BigDecimal(scannedPercentage.toString().replaceFirst("6", "9")));
                if (sumOfPercentages(fixedPercentages).doubleValue() == 100.0) {
                    return fixedPercentages; // I fixed it!
                }
            }

            if (scannedPercentage.toString().contains("8")) {
                fixedPercentages.put(name, new BigDecimal(scannedPercentage.toString().replaceFirst("8", "6")));
                if (sumOfPercentages(fixedPercentages).doubleValue() == 100.0) {
                    return fixedPercentages; // I fixed it!
                }
                fixedPercentages.put(name, new BigDecimal(scannedPercentage.toString().replaceFirst("8", "9")));
                if (sumOfPercentages(fixedPercentages).doubleValue() == 100.0) {
                    return fixedPercentages; // I fixed it!
                }
            }

            if (scannedPercentage.toString().contains("9")) {
                fixedPercentages.put(name, new BigDecimal(scannedPercentage.toString().replaceFirst("9", "6")));
                if (sumOfPercentages(fixedPercentages).doubleValue() == 100.0) {
                    return fixedPercentages; // I fixed it!
                }
                fixedPercentages.put(name, new BigDecimal(scannedPercentage.toString().replaceFirst("9", "9")));
                if (sumOfPercentages(fixedPercentages).doubleValue() == 100.0) {
                    return fixedPercentages; // I fixed it!
                }
            }
        }

        return null; // Cannot fix :-(
    }

    private static BigDecimal sumOfPercentages(LinkedHashMap<String, BigDecimal> percentages) {
        BigDecimal sum = BigDecimal.ZERO;
        for (BigDecimal percentage : percentages.values()) {
            sum = sum.add(percentage);
        }
        return sum;
    }

}
