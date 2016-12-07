package borg.edtrading.bodyscanner;

import borg.edtrading.Constants;
import borg.edtrading.data.BodyInfo;
import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.ocr.TextBuilder;
import borg.edtrading.ocr.TextLine;
import borg.edtrading.ocr.Word;
import borg.edtrading.ocr.fixer.Fixer;
import borg.edtrading.ocr.fixer.FixerException;
import borg.edtrading.ocr.fixer.general.BodyNameFixer;
import borg.edtrading.ocr.fixer.planets.ArgOfPeriapsisFixer;
import borg.edtrading.ocr.fixer.planets.ArrivalPointFixer;
import borg.edtrading.ocr.fixer.planets.AxialTiltFixer;
import borg.edtrading.ocr.fixer.planets.EarthMassesFixer;
import borg.edtrading.ocr.fixer.planets.GravityFixer;
import borg.edtrading.ocr.fixer.planets.OrbitalEccentricityFixer;
import borg.edtrading.ocr.fixer.planets.OrbitalInclinationFixer;
import borg.edtrading.ocr.fixer.planets.OrbitalPeriodFixer;
import borg.edtrading.ocr.fixer.planets.RadiusFixer;
import borg.edtrading.ocr.fixer.planets.RotationalPeriodFixer;
import borg.edtrading.ocr.fixer.planets.SemiMajorAxisFixer;
import borg.edtrading.ocr.fixer.planets.SurfacePressureFixer;
import borg.edtrading.ocr.fixer.planets.SurfaceTempFixer;
import borg.edtrading.ocr.templatematching.Match;
import borg.edtrading.ocr.templatematching.Template;
import borg.edtrading.util.MiscUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * ScannedBodyInfoParser
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyMatchesParser {

    static final Logger logger = LogManager.getLogger(BodyMatchesParser.class);

    private static String currentScreenshotFilename = "";

    public static ScannedBodyInfo fromScannedAndSortedLines(String screenshotFilename, String systemName, List<TextLine> bodyNameLines, List<TextLine> bodyInfoLines) {
        currentScreenshotFilename = screenshotFilename;

        ScannedBodyInfo scannedBodyInfo = new ScannedBodyInfo(screenshotFilename, systemName);

        // >>>> START BODY NAME >>>>

        // Find all lines which start with a label
        Map<String, List<TextLine>> textLinesByLabel = new LinkedHashMap<>();
        textLinesByLabel.put("BODYNAME:", Arrays.asList(bodyNameLines.get(0)));
        findAndFixLabel("ARRIVALPOINT:", bodyNameLines, textLinesByLabel);
        findAndFixLabel("ATMOSPHERETYPE:", bodyNameLines, textLinesByLabel);

        // Find value-only lines
        for (BodyInfo bi : BodyInfo.byPrefix("TYPE_")) {
            findAndFixLabel(bi.getName().replaceAll("\\s", ""), bodyNameLines, textLinesByLabel);
        }
        findAndFixLabel("Select", bodyNameLines, textLinesByLabel);

        // Find all lines which do not have a label and add them to the previous label
        for (TextLine tl : bodyNameLines) {
            maybeAddToPreviousLabel(tl, textLinesByLabel, bodyNameLines);
        }

        // TODO Remove debug output
        for (String label : textLinesByLabel.keySet()) {
            logger.debug(String.format("%-21s ", label) + textLinesByLabel.get(label));
        }

        scannedBodyInfo.setBodyName((String) findAndFixValue("BODYNAME:", textLinesByLabel, new BodyNameFixer()));
        scannedBodyInfo.setDistanceLs((BigDecimal) findAndFixValue("ARRIVALPOINT:", textLinesByLabel, new ArrivalPointFixer()));
        //scannedBodyInfo.setAtmosphereType((BigDecimal) findAndFixValue("ATMOSPHERETYPE:", textLinesByLabel, new GravityFixer()));

        // <<<< END BODY NAME <<<<

        // >>>> START BODY INFO >>>>

        // Find all lines which start with a label
        textLinesByLabel = new LinkedHashMap<>();
        findAndFixLabel("EARTHMASSES:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("RADIUS:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("GRAVITY:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("SURFACETEMP:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("SURFACEPRESSURE:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("ATMOSPHERES", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("VOLCANISM:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("ATMOSPHERETYPE:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("ATMOSPHERE:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("COMPOSITION:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("ORBITALPERIOD:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("SEMIMAJORAXIS:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("ORBITALECCENTRICITY:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("ORBITALINCLINATION:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("ARGOFPERIAPSIS:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("ROTATIONALPERIOD:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("(TIDALLYLOCKED)", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("AXIALTILT:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("PLANETMATERIALS:", bodyInfoLines, textLinesByLabel);

        findAndFixLabel("AGE:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("SOLARMASSES:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("MILLIONYEARS", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("SOLARRADIUS:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("SURFACETEMP:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("ORBITALPERIOD:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("SEMIMAJORAXIS:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("ORBITALECCENTRICITY:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("ORBITALINCLINATION:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("ARGOFPERIAPSIS:", bodyInfoLines, textLinesByLabel);

        findAndFixLabel("MOONMASSES:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("RINGTYPE:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("ORBITALPERIOD:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("SEMIMAJORAXIS:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("ORBITALECCENTRICITY:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("ORBITALINCLINATION:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("ARGOFPERIAPSIS:", bodyInfoLines, textLinesByLabel);

        findAndFixLabel("RINGTYPE:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("MASS:", bodyInfoLines, textLinesByLabel);
        //findAndFixLabel("SEMIMAJORAXIS:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("INNERRADIUS:", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("OUTERRADIUS:", bodyInfoLines, textLinesByLabel);

        // Find value-only lines
        for (BodyInfo bi : BodyInfo.byPrefix("TERRAFORMING_")) {
            findAndFixLabel(bi.getName().replaceAll("\\s", ""), bodyInfoLines, textLinesByLabel);
        }
        for (BodyInfo bi : BodyInfo.byPrefix("RESERVES_")) {
            findAndFixLabel(bi.getName().replaceAll("\\s", ""), bodyInfoLines, textLinesByLabel);
        }
        for (BodyInfo bi : BodyInfo.byPrefix("RESERVES_")) {
            findAndFixLabel(bi.getName().replaceAll("\\s", ""), bodyInfoLines, textLinesByLabel);
        }
        for (BodyInfo bi : BodyInfo.byPrefix("STAR_TYPE_")) {
            findAndFixLabel(bi.getName().replaceAll("\\s", ""), bodyInfoLines, textLinesByLabel);
        }
        findAndFixLabel("BALANCE", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("BACK", bodyInfoLines, textLinesByLabel);
        findAndFixLabel("EXIT", bodyInfoLines, textLinesByLabel);

        // Find all lines which do not have a label and add them to the previous label
        for (TextLine tl : bodyInfoLines) {
            maybeAddToPreviousLabel(tl, textLinesByLabel, bodyInfoLines);
        }

        // Set body group depending on what labels have been found
        if (textLinesByLabel.containsKey("EARTHMASSES:")) {
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_PLANET);
        } else if (textLinesByLabel.containsKey("SOLARMASSES:")) {
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_STAR);
        } else if (textLinesByLabel.containsKey("MOONMASSES:")) {
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_BELT);
        } else {
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_RINGS);
        }

        // TODO Remove debug output
        for (String label : textLinesByLabel.keySet()) {
            logger.debug(String.format("%-21s ", label) + textLinesByLabel.get(label));
        }

        scannedBodyInfo.setEarthMasses((BigDecimal) findAndFixValue("EARTHMASSES:", textLinesByLabel, new EarthMassesFixer()));
        scannedBodyInfo.setRadiusKm((BigDecimal) findAndFixValue("RADIUS:", textLinesByLabel, new RadiusFixer()));
        scannedBodyInfo.setGravityG((BigDecimal) findAndFixValue("GRAVITY:", textLinesByLabel, new GravityFixer()));
        scannedBodyInfo.setSurfaceTempK((BigDecimal) findAndFixValue("SURFACETEMP:", textLinesByLabel, new SurfaceTempFixer()));
        scannedBodyInfo.setSurfacePressureAtmospheres((BigDecimal) findAndFixValue("SURFACEPRESSURE:", textLinesByLabel, new SurfacePressureFixer()));
        //TODO scannedBodyInfo.setVolcanism((BigDecimal) findAndFixValue("VOLCANISM:", textLinesByLabel, new RadiusFixer()));
        //TODO scannedBodyInfo.setAtmosphereType((BigDecimal) findAndFixValue("ATMOSPHERETYPE:", textLinesByLabel, new RadiusFixer()));
        //TODO scannedBodyInfo.setAtmosphere((BigDecimal) findAndFixValue("ATMOSPHERE:", textLinesByLabel, new RadiusFixer()));
        //TODO scannedBodyInfo.setComposition((BigDecimal) findAndFixValue("COMPOSITION:", textLinesByLabel, new RadiusFixer()));
        scannedBodyInfo.setOrbitalPeriodD((BigDecimal) findAndFixValue("ORBITALPERIOD:", textLinesByLabel, new OrbitalPeriodFixer()));
        scannedBodyInfo.setSemiMajorAxisAU((BigDecimal) findAndFixValue("SEMIMAJORAXIS:", textLinesByLabel, new SemiMajorAxisFixer()));
        scannedBodyInfo.setOrbitalEccentricity((BigDecimal) findAndFixValue("ORBITALECCENTRICITY:", textLinesByLabel, new OrbitalEccentricityFixer()));
        scannedBodyInfo.setOrbitalInclinationDeg((BigDecimal) findAndFixValue("ORBITALINCLINATION:", textLinesByLabel, new OrbitalInclinationFixer()));
        scannedBodyInfo.setArgOfPeriapsisDeg((BigDecimal) findAndFixValue("ARGOFPERIAPSIS:", textLinesByLabel, new ArgOfPeriapsisFixer()));
        scannedBodyInfo.setRotationalPeriodD((BigDecimal) findAndFixValue("ROTATIONALPERIOD:", textLinesByLabel, new RotationalPeriodFixer()));
        scannedBodyInfo.setAxialTiltDeg((BigDecimal) findAndFixValue("AXIALTILT:", textLinesByLabel, new AxialTiltFixer()));
        //TODO scannedBodyInfo.setPlanetMaterials((BigDecimal) findAndFixValue("PLANETMATERIALS:", textLinesByLabel, new RadiusFixer()));

        // <<<< END BODY INFO <<<<

        return scannedBodyInfo;
    }

    private static Object findAndFixValue(String label, Map<String, List<TextLine>> textLinesByLabel, Fixer fixer) {
        List<TextLine> textLines = textLinesByLabel.get(label);
        if (textLines == null || textLines.isEmpty()) {
            return null;
        } else {
            List<Match> matches = new ArrayList<>();
            for (TextLine tl : textLines) {
                for (Word w : tl.getSortedWords()) {
                    for (Match m : w.getSortedMatches()) {
                        if (m.getShouldHaveBeen() == null) {
                            matches.add(m);
                        }
                    }
                }
            }
            if (matches.isEmpty()) {
                return null;
            } else {
                try {
                    return fixer.fix(matches);
                } catch (FixerException e) {
                    logger.warn(currentScreenshotFilename + ": Failed to fix value using " + fixer.getClass().getSimpleName() + ": " + e.getMessage());
                    return null;
                }
            }
        }
    }

    private static String maybeAddToPreviousLabel(TextLine currentTextLine, Map<String, List<TextLine>> textLinesByLabel, List<TextLine> sortedTextLines) {
        // Is it already labeled?
        for (List<TextLine> labeledTextLines : textLinesByLabel.values()) {
            if (labeledTextLines.contains(currentTextLine)) {
                return null; // Was already labeled
            }
        }

        // It is not. Search the previous label.
        int currentIndex = sortedTextLines.indexOf(currentTextLine);
        int maxIndex = -1;
        String previousLabel = null;
        for (String label : textLinesByLabel.keySet()) {
            List<TextLine> textLines = textLinesByLabel.get(label);
            for (TextLine tl : textLines) {
                int index = sortedTextLines.indexOf(tl);
                if (index > maxIndex && index < currentIndex) {
                    maxIndex = index;
                    previousLabel = label;
                }
            }
        }

        // Add and return
        if (previousLabel == null) {
            logger.warn(currentScreenshotFilename + ": Did not find a previous label for " + currentTextLine);
        } else {
            logger.debug(currentScreenshotFilename + ": Added " + currentTextLine + " to " + previousLabel);
            textLinesByLabel.get(previousLabel).add(currentTextLine);
        }

        return previousLabel;
    }

    private static TextLine findAndFixLabel(String label, List<TextLine> textLines, Map<String, List<TextLine>> textLinesByLabel) {
        // Find
        final float maxError = maxLevenshteinError(label);

        TextLine bestTextLine = null;
        float bestError = 1.0f;
        int usedWords = -1;
        for (TextLine tl : textLines) {
            List<Word> words = tl.getSortedWords();
            for (int numWords = 1; numWords <= words.size(); numWords++) {
                String scannedText = wordsToText(words, 0, numWords);
                if (scannedText != null && (!label.contains(":") || scannedText.contains(":"))) {
                    float error = MiscUtil.levenshteinError(label, scannedText);
                    if (error <= maxError && error < bestError) {
                        bestTextLine = tl;
                        bestError = error;
                        usedWords = numWords;
                    }
                }
            }
        }

        if (bestTextLine == null) {
            return null;
        } else {
            // Add
            List<TextLine> textLinesForLabel = textLinesByLabel.get(label);
            if (textLinesForLabel == null) {
                textLinesForLabel = new ArrayList<>(1);
                textLinesByLabel.put(label, textLinesForLabel);
            }
            textLinesForLabel.add(bestTextLine);

            // Fix
            List<Match> matches = new ArrayList<>(label.length());
            for (int wordIndex = 0; wordIndex < usedWords; wordIndex++) {
                matches.addAll(bestTextLine.getSortedWords().get(wordIndex).getSortedMatches());
            }
            findAndRemove(label, matches, new TreeMap<>());

            // Return
            return bestTextLine;
        }
    }

    private static String wordsToText(List<Word> words, int firstWordIndex, int numWords) {
        String text = "";
        for (int i = 0; i < numWords; i++) {
            Word w = words.get(firstWordIndex + i);
            for (Match m : w.getSortedMatches()) {
                if (m.getShouldHaveBeen() != null) {
                    return null; // Abort. Contains matches which have already been identified.
                } else {
                    text += m.getTemplate().getText(); // Concat and continue
                }
            }
        }
        return text;
    }

    public static ScannedBodyInfo fromScannedAndSortedMatches(String screenshotFilename, String systemName, List<TextLine> bodyNameLines, List<Match> bodyInfoMatches) {
        currentScreenshotFilename = screenshotFilename;

        ScannedBodyInfo scannedBodyInfo = new ScannedBodyInfo(screenshotFilename, systemName);

        // >>>> START BODY NAME >>>>
        String bodyName = "";
        BigDecimal distanceLs = null;
        BodyInfo bodyType = null;
        if (bodyNameLines.size() >= 1) {
            TextLine nameLine = bodyNameLines.get(0);
            List<Match> nameMatches = nameLine.getMatches();

            bodyName = nameLine.toText();
            bodyName = removePixelErrorsFromBodyName(bodyName);
            bodyName = fixGeneratedBodyName(systemName, bodyName);
            String fixedText = bodyName.toUpperCase().replaceAll("\\s", "");
            if (nameMatches.size() == fixedText.length()) {
                for (int i = 0; i < fixedText.length(); i++) {
                    char shouldHaveBeen = fixedText.charAt(i);
                    nameMatches.get(i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                }
            }
        }
        if (bodyNameLines.size() >= 2) {
            TextLine arrivalLine = bodyNameLines.get(1);
            List<Match> arrivalMatches = arrivalLine.getMatches();

            SortedMap<Integer, String> sortedIndexes = new TreeMap<>();
            if (findAndRemove("ARRIVALPOINT:", arrivalMatches, sortedIndexes) != null) {
                distanceLs = fixAndRemoveDistance("ARRIVALPOINT:", arrivalMatches, sortedIndexes);
            }
        }
        if (bodyNameLines.size() >= 3) {
            TextLine typeLine = bodyNameLines.get(2);
            List<Match> typeMatches = typeLine.getMatches();

            bodyType = BodyInfo.findBestMatching(typeLine.toText().replace("0", "O"), "TYPE_");
            if (bodyType != null) {
                SortedMap<Integer, String> sortedIndexes = new TreeMap<>();
                String nameWithoutSpaces = bodyType.getName().replaceAll("\\s", "");
                findAndRemove(nameWithoutSpaces, typeMatches, "TYPE_", sortedIndexes);
            }
        }
        scannedBodyInfo.setBodyName(bodyName);
        scannedBodyInfo.setDistanceLs(distanceLs);
        scannedBodyInfo.setBodyType(bodyType);
        List<Match> bodyNameMatches = new ArrayList<>();
        for (TextLine tl : bodyNameLines) {
            bodyNameMatches.addAll(tl.getMatches());
        }
        // <<<< END BODY NAME <<<<

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
        int nRings = 0;
        if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_STAR) {
            // TODO Parse star class from description text
            for (BodyInfo bi : BodyInfo.byPrefix("STAR_TYPE_")) {
                String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
                if (findAndRemove(nameWithoutSpaces, bodyInfoMatches, 0, indexOf("SOLARMASSES:", sortedLabelIndexes), "STAR_TYPE_", sortedLabelIndexes) != null) { // Star type unfortunately is before solar masses, so explicitly search from index 0...
                    scannedBodyInfo.setStarType(bi);
                    break; // Expect only one hit
                }
            }
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
        } else if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_RINGS) {
            // Search for rings
            String expectedRingNamePrefix = bodyName.replaceAll("\\s", "");
            while (findAndRemove(expectedRingNamePrefix, bodyInfoMatches, "RING" + (nRings + 1) + "_NAME_", sortedLabelIndexes) != null) {
                nRings++;
                findAndRemove("RINGTYPE:", bodyInfoMatches, "RING" + nRings + "_", sortedLabelIndexes);
                findAndRemove("MASS:", bodyInfoMatches, "RING" + nRings + "_", sortedLabelIndexes);
                findAndRemove("SEMIMAJORAXIS:", bodyInfoMatches, "RING" + nRings + "_", sortedLabelIndexes);
                findAndRemove("INNERRADIUS:", bodyInfoMatches, "RING" + nRings + "_", sortedLabelIndexes);
                findAndRemove("OUTERRADIUS:", bodyInfoMatches, "RING" + nRings + "_", sortedLabelIndexes);
            }

            // Could contain the last lines of a ringed planet or ringed star.
            // Remove/learn them.
            findAndRemove("SOLARRADIUS:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("SURFACETEMP:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ORBITALPERIOD:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("SEMIMAJORAXIS:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ORBITALECCENTRICITY:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ORBITALINCLINATION:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ARGOFPERIAPSIS:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);

            findAndRemove("RADIUS:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("GRAVITY:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("SURFACETEMP:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            if (findAndRemove("SURFACEPRESSURE:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes) != null) {
                findAndRemove("ATMOSPHERES", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes); // Remove the unit of surface pressure because it is long and may collide with later labels
            }
            findAndRemove("VOLCANISM:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ATMOSPHERETYPE:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ATMOSPHERE:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("COMPOSITION:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ORBITALPERIOD:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("SEMIMAJORAXIS:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ORBITALECCENTRICITY:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ORBITALINCLINATION:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ARGOFPERIAPSIS:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("ROTATIONALPERIOD:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("(TIDALLYLOCKED)", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("AXIALTILT:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
            findAndRemove("PLANETMATERIALS:", bodyInfoMatches, 0, sortedLabelIndexes.isEmpty() ? bodyInfoMatches.size() : sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
        }

        // Always search for star catalogue IDs
        findAndRemove("STARCATALOGUEID:", bodyInfoMatches, 0, sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
        findAndRemove("GLIESE:", bodyInfoMatches, 0, sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
        findAndRemove("HIPP:", bodyInfoMatches, 0, sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);
        findAndRemove("HD:", bodyInfoMatches, 0, sortedLabelIndexes.lastKey(), "", sortedLabelIndexes);

        // Reserves again (the one before rings)
        Integer idxRingtype = indexOf("RING1_RINGTYPE:", sortedLabelIndexes);
        if (idxRingtype == null) {
            idxRingtype = bodyInfoMatches.size();
        }
        for (BodyInfo bi : BodyInfo.byPrefix("RESERVES_")) {
            String nameWithoutSpaces = bi.getName().replaceAll("\\s", "");
            if (findAndRemove(nameWithoutSpaces, bodyInfoMatches, 0, idxRingtype, "RING_RESERVES_", sortedLabelIndexes) != null) { // Reserves unfortunately is before moon masses, so explicitly search from index 0...
                scannedBodyInfo.setSystemReserves(bi);
                break; // Expect only one hit
            }
        }

        findAndRemove("BACK", bodyInfoMatches, sortedLabelIndexes); // Back button...
        findAndRemove("EXIT", bodyInfoMatches, sortedLabelIndexes); // Exit button...

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
        }

        if (nRings > 0) {
            List<ScannedRingInfo> rings = new ArrayList<>(nRings);
            for (int n = 1; n <= nRings; n++) {
                ScannedRingInfo ring = new ScannedRingInfo();
                ring.setRingName(fixAndRemoveRingName("RING" + n + "_RINGTYPE:", bodyInfoMatches, sortedLabelIndexes, systemName, bodyName));
                ring.setRingType(fixAndRemoveRingType("RING" + n + "_RINGTYPE:", bodyInfoMatches, sortedLabelIndexes));
                ring.setMassMt(fixAndRemoveRingMass("RING" + n + "_MASS:", bodyInfoMatches, sortedLabelIndexes));
                ring.setSemiMajorAxisAU(fixAndRemoveSemiMajorAxis("RING" + n + "_SEMIMAJORAXIS:", bodyInfoMatches, sortedLabelIndexes));
                ring.setInnerRadiusKm(fixAndRemoveRingRadius("RING" + n + "_INNERRADIUS:", bodyInfoMatches, sortedLabelIndexes));
                ring.setOuterRadiusKm(fixAndRemoveRingRadius("RING" + n + "_OUTERRADIUS:", bodyInfoMatches, sortedLabelIndexes));
                rings.add(ring);
            }
            scannedBodyInfo.setRings(rings);
        }

        try {
            // FIXME Remove debug output!!!
            if (scannedBodyInfo.getAtmosphereType() == BodyInfo.ATMOSPHERE_TYPE_MATEALLIC_VAPOUR) {
                logger.debug("ATMOSPHERE_TYPE_MATEALLIC_VAPOUR: " + currentScreenshotFilename);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        // TODO Remove this debug output
        //        System.out.print("         ");
        //        for (int i = 0; i < bodyNameMatches.size(); i++) {
        //            System.out.print(String.format("%-3d", i));
        //        }
        //        System.out.println();
        //        System.out.print("Scanned: ");
        //        for (Match m : bodyNameMatches) {
        //            System.out.print("<" + m.getTemplate().getText() + ">");
        //        }
        //        System.out.println();
        //        System.out.print("Parsed:  ");
        //        for (Match m : bodyNameMatches) {
        //            System.out.print("<" + (m.getShouldHaveBeen() == null ? "?" : m.getShouldHaveBeen()) + ">");
        //        }
        //        System.out.println();
        //        System.out.print("         ");
        //        for (int i = 0; i < bodyInfoMatches.size(); i++) {
        //            System.out.print(String.format("%-3d", i));
        //        }
        //        System.out.println();
        //        System.out.print("Scanned: ");
        //        for (Match m : bodyInfoMatches) {
        //            System.out.print("<" + m.getTemplate().getText() + ">");
        //        }
        //        System.out.println();
        //        System.out.print("Parsed:  ");
        //        for (Match m : bodyInfoMatches) {
        //            System.out.print("<" + (m.getShouldHaveBeen() == null ? "?" : m.getShouldHaveBeen()) + ">");
        //        }
        //        System.out.println();

        learnWronglyDetectedChars(bodyNameMatches);
        learnWronglyDetectedChars(bodyInfoMatches);

        return scannedBodyInfo;
    }

    private static String fixAndRemoveRingName(String beforeLabel, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes, String systemName, String bodyName) {
        Integer labelIndex = indexOf(beforeLabel, sortedLabelIndexes);
        if (labelIndex != null) {
            List<Match> remainingMatches = new ArrayList<>();
            for (int i = labelIndex - 1; i >= 0; i--) {
                if (matches.get(i).getShouldHaveBeen() != null) {
                    break;
                } else if ("▪".equals(matches.get(i).getTemplate().getText())) {
                    // Skip
                } else {
                    remainingMatches.add(0, matches.get(i));
                }
            }
            String ringName = bodyName;
            List<TextLine> remainingTextLines = new ArrayList<>();
            try {
                remainingTextLines = TextBuilder.matchesToText(remainingMatches);
            } catch (Exception e) {
                logger.error(currentScreenshotFilename + ": Failed to parse ring name matches", e);
            }
            if (remainingTextLines.size() > 0) {
                ringName = remainingTextLines.get(0).toText();
            }
            ringName = removePixelErrorsFromBodyName(ringName);
            ringName = fixGeneratedBodyName(systemName, ringName);
            String fixedText = ringName.replace(bodyName, "").replaceAll("\\s", "");
            // Set shouldHaveBeen, effectively removing the matches
            if (remainingMatches.size() == fixedText.length()) {
                for (int i = 0; i < remainingMatches.size(); i++) {
                    char shouldHaveBeen = fixedText.charAt(i);
                    remainingMatches.get(i).setShouldHaveBeen(Character.toString(shouldHaveBeen));
                }
            }
            // Return the result
            return ringName;
        }
        return null;
    }

    private static void learnWronglyDetectedChars(List<Match> fixedMatches) {
        for (Match m : fixedMatches) {
            if (m != null && m.getShouldHaveBeen() != null) {
                // We know what it should have been
                if (m.getxInRegion() == 0 && m.getyInRegion() == 0) {
                    // It is a full region match, not a partial region match
                    if (!m.getShouldHaveBeen().equals(m.getTemplate().getText())) {
                        // It is NOT what it should have been
                        if (!isSameUppercaseAndLowercase(m.getShouldHaveBeen(), m.getTemplate().getText()) || Constants.LEARN_Z_VS_z) {
                            if (!is0vsO(m.getShouldHaveBeen(), m.getTemplate().getText()) || Constants.LEARN_0_VS_O) {
                                // It is totally wrong, or we are allowed to learn difficult chars like 0<->O
                                try {
                                    Template.createNewFromRegion(m.getRegion(), "LEARNED_FIXED", m.getShouldHaveBeen());
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        }
                    } else if (m.getErrorPerPixel() > BodyScanner.ERROR_PER_PIXEL_KNOWN && m.getErrorPerPixel() <= BodyScanner.ERROR_PER_PIXEL_GUESSED) {
                        // It has been detected/guessed correctly and is quite, but not totally off.
                        try {
                            Template.createNewFromRegion(m.getRegion(), "LEARNED_VARIANT", m.getShouldHaveBeen());
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
        }
    }

    private static String removePixelErrorsFromBodyName(final String scannedBodyName) {
        if (StringUtils.isNotBlank(scannedBodyName)) {
            String fixedName = scannedBodyName;
            while (fixedName.startsWith(".") || fixedName.startsWith(",") || fixedName.startsWith(":") || fixedName.startsWith("-") || fixedName.startsWith("'") || fixedName.startsWith("°")) {
                fixedName = fixedName.substring(1, fixedName.length()).trim();
            }
            while (fixedName.endsWith(".") || fixedName.endsWith(",") || fixedName.endsWith(":") || fixedName.endsWith("-") || fixedName.endsWith("'") || fixedName.endsWith("°")) {
                fixedName = fixedName.substring(0, fixedName.length() - 1).trim();
            }
            return fixedName;
        }
        return scannedBodyName;
    }

    private static String fixGeneratedBodyName(final String systemName, final String scannedBodyName) {
        if (StringUtils.isBlank(systemName) || StringUtils.isBlank(scannedBodyName)) {
            return scannedBodyName;
        } else {
            String fixedBodyName = scannedBodyName;

            // Try to fix the first part which often is the system name
            // Example: ETA COR0NAE BOREALIS A 1 -> Eta Coronae Borealis A 1
            String partOfScannedBodyNameToReplaceWithSystemName = null;
            float bestMatchSoFar = 1.0f;
            int index = 0;
            while (scannedBodyName.indexOf(" ", index + 1) >= 0) {
                index = scannedBodyName.indexOf(" ", index + 1);
                String partOfScannedBodyName = scannedBodyName.substring(0, index);
                float err = MiscUtil.levenshteinError(systemName.replace("0", "O").replace("l", "I"), partOfScannedBodyName.replace("0", "O").replace("l", "I"));
                if (err <= 0.25f && err < bestMatchSoFar) {
                    partOfScannedBodyNameToReplaceWithSystemName = partOfScannedBodyName;
                    bestMatchSoFar = err;
                }
            }
            float fullNameErr = MiscUtil.levenshteinError(systemName.replace("0", "O").replace("l", "I"), scannedBodyName.replace("0", "O").replace("l", "I"));
            if (fullNameErr <= 0.25f && fullNameErr < bestMatchSoFar) {
                partOfScannedBodyNameToReplaceWithSystemName = scannedBodyName;
            }
            if (partOfScannedBodyNameToReplaceWithSystemName != null) {
                fixedBodyName = scannedBodyName.replace(partOfScannedBodyNameToReplaceWithSystemName, systemName);
                if (!fixedBodyName.equalsIgnoreCase(scannedBodyName)) {
                    logger.trace("Fixed '" + scannedBodyName + "' to '" + fixedBodyName.toUpperCase() + "'");
                }
            }

            // If the name does not start with the system convert to camel case, replacing 0 with O
            // Example: L0WING'S ROCK -> Lowing's Rock
            if (partOfScannedBodyNameToReplaceWithSystemName == null) {
                fixedBodyName = WordUtils.capitalizeFully(scannedBodyName.replace("0", "O"));
                if (!fixedBodyName.equalsIgnoreCase(scannedBodyName)) {
                    logger.trace("Fixed '" + scannedBodyName + "' to '" + fixedBodyName.toUpperCase() + "'");
                }
            } else {
                // Because the body name is generated we might have a designation. Try to improve it.
                // Example: Eta Coronae Borealis A1 A Rlng -> Eta Coronae Borealis A 1 A Ring
                String scannedDesignation = scannedBodyName.replace(partOfScannedBodyNameToReplaceWithSystemName, "").trim();
                if (StringUtils.isNotEmpty(scannedDesignation)) {
                    String fixedDesignation = "";
                    for (String part : scannedDesignation.toUpperCase().split("\\s")) {
                        String fixedPart = " ";
                        Matcher mA1A = Pattern.compile("([A-Z]+)([0-9]+)([A-Z]+)").matcher(part);
                        Matcher m1A1 = Pattern.compile("([0-9]+)([A-Z]+)([0-9]+)").matcher(part);
                        Matcher mA1 = Pattern.compile("([A-Z]+)([0-9]+)").matcher(part);
                        Matcher m1A = Pattern.compile("([0-9]+)([A-Z]+)").matcher(part);
                        if (MiscUtil.levenshteinError("RING", part) <= 0.25f) {
                            fixedPart += "Ring";
                        } else if (MiscUtil.levenshteinError("BELT", part) <= 0.25f) {
                            fixedPart += "Belt";
                        } else if (mA1A.matches()) {
                            fixedPart += (mA1A.group(1) + " " + mA1A.group(2) + " " + mA1A.group(3));
                        } else if (m1A1.matches()) {
                            fixedPart += (m1A1.group(1) + " " + m1A1.group(2) + " " + m1A1.group(3));
                        } else if (mA1.matches()) {
                            fixedPart += (mA1.group(1) + " " + mA1.group(2));
                        } else if (m1A.matches()) {
                            fixedPart += (m1A.group(1) + " " + m1A.group(2));
                        } else {
                            fixedPart += part;
                        }
                        fixedDesignation += fixedPart;
                    }
                    fixedBodyName = systemName + fixedDesignation;
                    if (!fixedBodyName.equalsIgnoreCase(scannedBodyName)) {
                        logger.trace("Fixed '" + scannedBodyName + "' to '" + fixedBodyName.toUpperCase() + "'");
                    }
                }
            }

            return fixedBodyName;
        }
    }

    private static boolean isSameUppercaseAndLowercase(String shouldHaveBeen, String actuallyIs) {
        boolean isCc = "C".equalsIgnoreCase(shouldHaveBeen) && "c".equalsIgnoreCase(actuallyIs);
        boolean isOo = "O".equalsIgnoreCase(shouldHaveBeen) && "o".equalsIgnoreCase(actuallyIs);
        boolean isSs = "S".equalsIgnoreCase(shouldHaveBeen) && "s".equalsIgnoreCase(actuallyIs);
        boolean isVv = "V".equalsIgnoreCase(shouldHaveBeen) && "v".equalsIgnoreCase(actuallyIs);
        boolean isWw = "W".equalsIgnoreCase(shouldHaveBeen) && "w".equalsIgnoreCase(actuallyIs);
        boolean isXx = "X".equalsIgnoreCase(shouldHaveBeen) && "x".equalsIgnoreCase(actuallyIs);
        boolean isZz = "Z".equalsIgnoreCase(shouldHaveBeen) && "z".equalsIgnoreCase(actuallyIs);

        return isCc || isOo || isSs || isVv || isWw || isXx || isZz;
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

    private static Integer findAndRemove(CharSequence chars, List<Match> templateMatches, SortedMap<Integer, String> sortedPreviousIndexes) {
        String storeWithPrefix = "";

        return findAndRemove(chars, templateMatches, storeWithPrefix, sortedPreviousIndexes);
    }

    private static Integer findAndRemove(CharSequence chars, List<Match> templateMatches, String storeWithPrefix, SortedMap<Integer, String> sortedPreviousIndexes) {
        int lastKnownIndex = sortedPreviousIndexes.isEmpty() ? 0 : sortedPreviousIndexes.lastKey();
        int veryLastIndex = templateMatches.size();

        return findAndRemove(chars, templateMatches, lastKnownIndex, veryLastIndex, storeWithPrefix, sortedPreviousIndexes);
    }

    private static Integer findAndRemove(CharSequence chars, List<Match> templateMatches, final int fromIndex, final int toIndex, String storeWithPrefix, SortedMap<Integer, String> sortedPreviousIndexes) {
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
                            Match match = templateMatches.get(k);
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

    private static CharSequence getScannedText(List<Match> templateMatches, int startIndex, int length) {
        StringBuilder scannedText = new StringBuilder();
        for (int j = 0; j < length; j++) {
            int k = startIndex + j;
            if (k >= 0 && k < templateMatches.size()) {
                Match match = templateMatches.get(k);
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
            if ("D".equals(unit) && (fixedText.endsWith("O") || fixedText.endsWith("0"))) {
                fixedText = fixedText.substring(0, fixedText.length() - 1) + "D";
            }
            Pattern p = Pattern.compile("(▪*[\\d\\.\\-,]+)([^\\d\\.\\-,]{" + unit.length() + "})(▪*)"); // 0-n crap at start, 1-n digits, exact number of unit chars, 0-n crap at end
            Matcher m = p.matcher(text);
            if (m.matches()) {
                fixedText = m.group(1) + unit + m.group(3);
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

    private static BigDecimal fixAndRemoveDistance(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: #,##0.00LS
                BigDecimal min = new BigDecimal("2.00"); // Screenshot: 2016-10-10 04-33-04 Ross 780.png
                BigDecimal max = new BigDecimal("526274.25"); // Screenshot: 2016-10-02 20-02-39 Jaradharre.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                fixedText = lastCharsToUnit(fixedText, "LS");
                Pattern pDistance = Pattern.compile(".*?((\\d{1,3})(,\\d{3})*\\.\\d{2}LS).*?");
                Matcher mDistance = pDistance.matcher(fixedText);
                if (!mDistance.matches()) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String distanceText = mDistance.group(1);
                    String parseableText = distanceText.replace(",", "").replace("LS", ""); // Remove all thousands separators and units
                    BigDecimal value = new BigDecimal(parseableText);
                    if (value.compareTo(min) < 0) {
                        throw new NumberFormatException("Parsed value " + value + " is below the allowed minimum " + label.toLowerCase().replace(":", "") + " of " + min);
                    } else if (value.compareTo(max) > 0) {
                        throw new NumberFormatException("Parsed value " + value + " is above the allowed maximum " + label.toLowerCase().replace(":", "") + " of " + max);
                    } else {
                        // Set shouldHaveBeen, effectively removing the matches
                        int offset = fixedText.indexOf(distanceText);
                        for (int i = 0; i < distanceText.length(); i++) {
                            char shouldHaveBeen = distanceText.charAt(i);
                            matches.get(i + startIndex + offset).setShouldHaveBeen(Character.toString(shouldHaveBeen));
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

    private static BigDecimal fixAndRemoveAge(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal min = new BigDecimal("14"); // Screenshot: 2016-10-10 05-39-29 Gooheimar.png
                BigDecimal max = new BigDecimal("13042"); // Screenshot: 2016-10-10 04-19-56 Kareco.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveSolarMasses(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal min = new BigDecimal("0.0117"); // Screenshot: 2016-10-02 07-08-51 Xi Ursae Majoris.png
                BigDecimal max = new BigDecimal("2.5312"); // Screenshot: 2016-10-02 06-57-39 Sirius.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveSolarRadius(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal min = new BigDecimal("0.0111"); // Screenshot: 2016-10-02 06-57-48 Sirius.png
                BigDecimal max = new BigDecimal("2.0400"); // Screenshot: 2016-10-03 07-08-50 Liaedin.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveEarthMasses(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal min = new BigDecimal("0.0000"); // Screenshot: 2016-10-02 07-29-13 Dahan.png
                BigDecimal max = new BigDecimal("3274.6501"); // Screenshot: 2016-09-28 08-00-48 Arque.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveRadius(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal min = new BigDecimal("167"); // Screenshot: 2016-10-02 07-29-13 Dahan.png
                BigDecimal max = new BigDecimal("78930"); // Screenshot: 2016-09-30 17-22-30 Deciat.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveGravity(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal min = new BigDecimal("0.01"); // Screenshot: 2016-09-28 07-43-22 Sol.png
                BigDecimal max = new BigDecimal("29.55"); // Screenshot: 2016-09-28 08-00-48 Arque.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveSurfaceTemp(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal max = new BigDecimal("23048"); // Screenshot: 2016-10-10 04-46-02 Slink's Eye.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousands(fixedText);
                fixedText = lastCharsToUnit(fixedText, "K");
                if (fixedText.matches(".+,\\d{2}K")) {
                    fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                }
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

    private static BigDecimal fixAndRemoveSurfacePressure(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal max = new BigDecimal("3166833.00"); // Screenshot: 2016-10-21 08-26-51 Furuhjelm III-674.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BodyInfo fixAndRemoveVolcanism(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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

    private static BodyInfo fixAndRemoveAtmosphereType(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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

    private static BigDecimal fixAndRemoveOrbitalPeriod(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                if (fixedText.matches(".+[\\.,].[DO0o]")) {
                    fixedText = fixedText.substring(0, fixedText.length() - 1).replace("D", "0") + "D";
                }
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveSemiMajorAxis(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.replaceFirst("RING\\d_", "").length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: 0.00AU
                BigDecimal min = new BigDecimal("0.00"); // Screenshot: 2016-10-02 07-32-11 Wyrd.png
                BigDecimal max = new BigDecimal("564.58"); // Screenshot: 2016-09-30 16-57-43 HIP 2453.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveOrbitalEccentricity(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal max = new BigDecimal("0.5923"); // Screenshot: 2016-10-02 06-57-48 Sirius.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveOrbitalInclination(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: 0.00° (-180°..+360°)
                BigDecimal min = new BigDecimal("-180.00"); // Screenshot: ?
                BigDecimal max = new BigDecimal("360.00"); // Screenshot: ?
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveArgOfPeriapsis(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveRotationalPeriod(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal max = new BigDecimal("536.4"); // Screenshot: 2016-10-10 05-41-47 LHS 1050.png
                String fixedText = scannedText.toString();
                if (fixedText.matches(".+[\\.,].[DO0o]")) {
                    fixedText = fixedText.substring(0, fixedText.length() - 1).replace("D", "0") + "D";
                }
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveAxialTilt(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BodyInfo fixAndRemoveRingType(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.replaceFirst("RING\\d_", "").length();
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

    private static BigDecimal fixAndRemoveMoonMasses(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                BigDecimal max = new BigDecimal("411.1901"); // Screenshot: 2016-10-06 22-55-55 Delta Equulei.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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

    private static BigDecimal fixAndRemoveRingMass(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.replaceFirst("RING\\d_", "").length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: #,##0.0MT
                BigDecimal min = new BigDecimal("501.8"); // Screenshot: 2016-10-01 22-39-34 Moirai.png
                BigDecimal max = new BigDecimal("15310708015104.0"); // Screenshot: 2016-10-01 20-50-38 HIP 30953.png
                String fixedText = scannedText.toString();
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousandsExceptLast(fixedText);
                fixedText = lastCharsToUnit(fixedText, "MT");
                if (!fixedText.matches("(\\d{1,3})(,\\d{3})*\\.\\d{1}MT")) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String parseableText = fixedText.replace(",", "").replace("MT", ""); // Remove all thousands separators and units
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

    private static BigDecimal fixAndRemoveRingRadius(String label, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
        Integer labelIndex = indexOf(label, sortedLabelIndexes);
        if (labelIndex != null) {
            int endIndex = indexAfter(labelIndex, sortedLabelIndexes, matches.size());
            int startIndex = labelIndex + label.replaceFirst("RING\\d_", "").length();
            StringBuilder scannedText = new StringBuilder();
            for (int i = startIndex; i < endIndex; i++) {
                scannedText.append(matches.get(i).getTemplate().getText());
            }
            try {
                // Pattern: #,##0KM
                BigDecimal min = new BigDecimal("7549"); // Screenshot: 2016-10-01 22-39-34 Moirai.png
                BigDecimal max = new BigDecimal("516969"); // Screenshot: 2016-09-28 08-00-48 Arque.png
                String fixedText = scannedText.toString();
                if (fixedText.contains("KM")) {
                    fixedText = fixedText.substring(0, fixedText.indexOf("KM") + 2);
                    while (fixedText.length() < scannedText.length()) {
                        fixedText += "▪";
                    }
                }
                fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
                fixedText = allSeparatorsToThousands(fixedText);
                fixedText = lastCharsToUnit(fixedText, "KM");
                if (!fixedText.matches("▪*(\\d{1,3})(,\\d{3})*KM▪*")) {
                    throw new NumberFormatException("Fixed text '" + fixedText + "' does not match the expected pattern");
                } else {
                    String parseableText = fixedText.replace("▪", "").replace(",", "").replace("KM", ""); // Remove all thousands separators and units
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

    private static LinkedHashMap<BodyInfo, BigDecimal> fixAndRemoveComposition(String label, String prefix, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
                        fixedText = fixedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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
            LinkedHashMap<String, BigDecimal> wrongPercentages = new LinkedHashMap<>();
            for (BodyInfo bi : result.keySet()) {
                wrongPercentages.put(bi.getName(), result.get(bi));
            }
            LinkedHashMap<String, BigDecimal> fixedPercentages = fixPercentagesTo100(wrongPercentages);
            if (fixedPercentages == null) {
                //logger.warn(currentScreenshotFilename + ": Sum of " + label.toLowerCase().replace(":", "") + " is " + totalSumPercent + ": " + result);
                return result;
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
            //logger.debug(currentScreenshotFilename + ": Sum of " + label.toLowerCase().replace(":", "") + " is " + totalSumPercent + ": " + result);
            return result;
        } else {
            return result;
        }
    }

    private static LinkedHashMap<Item, BigDecimal> fixAndRemovePlanetMaterials(String label, String prefix, List<Match> matches, SortedMap<Integer, String> sortedLabelIndexes) {
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
            LinkedHashMap<String, BigDecimal> wrongPercentages = new LinkedHashMap<>();
            for (Item el : result.keySet()) {
                wrongPercentages.put(el.getName(), result.get(el));
            }
            LinkedHashMap<String, BigDecimal> fixedPercentages = fixPercentagesTo100(wrongPercentages);
            if (fixedPercentages == null) {
                //logger.warn(currentScreenshotFilename + ": Sum of " + label.toLowerCase().replace(":", "") + " is " + totalSumPercent + ": " + result);
                return result;
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
            //logger.debug(currentScreenshotFilename + ": Sum of " + label.toLowerCase().replace(":", "") + " is " + totalSumPercent + ": " + result);
            return result;
        } else {
            return result;
        }
    }

    private static String fixPlanetMaterialPercentage(String scannedText) {
        String fixedText = scannedText.replace("o", "0").replace("O", "0").replace("D", "0").replace("S", "5").replace("B", "8"); // Replace all chars which cannot occur
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
        // TODO 5 and 9 also?
        // Try to fix the sum by changing ONE (only ONE!) of them.
        for (String name : wrongPercentages.keySet()) {
            BigDecimal scannedPercentage = wrongPercentages.get(name);

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
