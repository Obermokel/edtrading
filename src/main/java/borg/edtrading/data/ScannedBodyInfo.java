package borg.edtrading.data;

import borg.edtrading.Constants;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.ocr.fixer.ArgOfPeriapsisFixer;
import borg.edtrading.ocr.fixer.AtmosphereTypeFixer;
import borg.edtrading.ocr.fixer.AxialTiltFixer;
import borg.edtrading.ocr.fixer.EarthMassesFixer;
import borg.edtrading.ocr.fixer.GravityFixer;
import borg.edtrading.ocr.fixer.OrbitalEccentricityFixer;
import borg.edtrading.ocr.fixer.OrbitalInclinationFixer;
import borg.edtrading.ocr.fixer.OrbitalPeriodFixer;
import borg.edtrading.ocr.fixer.RadiusFixer;
import borg.edtrading.ocr.fixer.RotationalPeriodFixer;
import borg.edtrading.ocr.fixer.SemiMajorAxisFixer;
import borg.edtrading.ocr.fixer.SurfaceTempFixer;
import borg.edtrading.ocr.fixer.TidallyLockedFixer;
import borg.edtrading.ocr.fixer.ValueFixer;
import borg.edtrading.ocr.fixer.VolcanismFixer;
import borg.edtrading.util.MatchSorter.MatchGroup;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;

/**
 * ScannedBodyInfo
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScannedBodyInfo {

    static final Logger logger = LogManager.getLogger(ScannedBodyInfo.class);

    private static final Pattern BODY_DESIGNATION_PATTERN = Pattern.compile("^(.+?) ([A-Fa-f]{1,5} [0-9]{1,2} ?[A-Fa-f]?)$");

    private String screenshotFilename = null;
    private String systemName = null;
    private String bodyName = null;
    private BodyInfo bodyType = null;
    private BodyInfo terraforming = null;
    private BigDecimal distanceLs = null;
    private BigDecimal earthMasses = null;
    private BigDecimal radiusKm = null;
    private BigDecimal gravityG = null;
    private BigDecimal surfaceTempK = null;
    private BodyInfo volcanism = null;
    private BodyInfo atmosphereType = null;
    private LinkedHashMap<BodyInfo, BigDecimal> composition = null;
    private BigDecimal orbitalPeriodD = null;
    private BigDecimal semiMajorAxisAU = null;
    private BigDecimal orbitalEccentricity = null;
    private BigDecimal orbitalInclinationDeg = null;
    private BigDecimal argOfPeriapsisDeg = null;
    private BigDecimal rotationalPeriodD = null;
    private Boolean tidallyLocked = null;
    private BigDecimal axialTiltDeg = null;
    private LinkedHashMap<Item, BigDecimal> planetMaterials = null;

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("==== ").append(this.getScreenshotFilename()).append(" ====").append("\n");
        sb.append(this.getSystemName()).append(" // ").append(this.getBodyName()).append("\n");
        if (this.getBodyType() != null) {
            sb.append(this.getBodyType().getName());
        } else {
            sb.append("null");
        }
        if (this.getTerraforming() != null) {
            sb.append(" // ").append(this.getTerraforming().getName());
        }
        sb.append(" // ").append(String.format("%.2fLs", this.getDistanceLs())).append("\n");
        sb.append(String.format("%-21s\t%.4f", "EARTH MASSES:", this.getEarthMasses())).append("\n");
        sb.append(String.format("%-21s\t%.0fKM", "RADIUS:", this.getRadiusKm())).append("\n");
        sb.append(String.format("%-21s\t%.2fG", "GRAVITY:", this.getGravityG())).append("\n");
        sb.append(String.format("%-21s\t%.0fK", "SURFACE TEMP:", this.getSurfaceTempK())).append("\n");
        sb.append(String.format("%-21s\t%s", "VOLCANISM:", this.getVolcanism() == null ? null : this.getVolcanism().getName())).append("\n");
        sb.append(String.format("%-21s\t%s", "ATMOSPHERE TYPE:", this.getAtmosphereType() == null ? null : this.getAtmosphereType().getName())).append("\n");
        sb.append(String.format("%-21s\t", "COMPOSITION:"));
        if (this.getComposition() == null) {
            sb.append("null").append("\n");
        } else {
            Iterator<BodyInfo> it = this.getComposition().keySet().iterator();
            while (it.hasNext()) {
                BodyInfo material = it.next();
                BigDecimal percent = this.getComposition().get(material);
                sb.append(String.format("%.1f%% %s", percent, material.getName())).append(it.hasNext() ? ", " : "\n");
            }
        }
        sb.append(String.format("%-21s\t%.1fD", "ORBITAL PERIOD:", this.getOrbitalPeriodD())).append("\n");
        sb.append(String.format("%-21s\t%.2fAU", "SEMI MAJOR AXIS:", this.getSemiMajorAxisAU())).append("\n");
        sb.append(String.format("%-21s\t%.4f", "ORBITAL ECCENTRICITY:", this.getOrbitalEccentricity())).append("\n");
        sb.append(String.format("%-21s\t%.2f°", "ORBITAL INCLINATION:", this.getOrbitalInclinationDeg())).append("\n");
        sb.append(String.format("%-21s\t%.2f°", "ARG OF PERIAPSIS:", this.getArgOfPeriapsisDeg())).append("\n");
        sb.append(String.format("%-21s\t%.1fD", "ROTATIONAL PERIOD:", this.getRotationalPeriodD()));
        if (Boolean.TRUE.equals(this.getTidallyLocked())) {
            sb.append(" (TIDALLY LOCKED)").append("\n");
        } else if (Boolean.FALSE.equals(this.getTidallyLocked())) {
            sb.append("\n");
        } else {
            sb.append(" (null)").append("\n");
        }
        sb.append(String.format("%-21s\t%.2f°", "AXIAL TILT:", this.getAxialTiltDeg() == null ? null : this.getAxialTiltDeg().doubleValue())).append("\n");
        sb.append(String.format("%-21s\t", "PLANET MATERIALS:"));
        if (this.getPlanetMaterials() == null) {
            sb.append("null").append("\n");
        } else {
            Iterator<Item> it = this.getPlanetMaterials().keySet().iterator();
            while (it.hasNext()) {
                Item material = it.next();
                BigDecimal percent = this.getPlanetMaterials().get(material);
                sb.append(String.format("%s(%.1f%%)", material.getName(), percent)).append(it.hasNext() ? ", " : "\n");
            }
        }
        return sb.toString().trim();
    }

    private ScannedBodyInfo(String screenshotFilename, String systemName, String bodyName, BodyInfo bodyType, BigDecimal distanceLs) {
        this.setScreenshotFilename(screenshotFilename);
        this.setSystemName(systemName);
        this.setBodyName(bodyName);
        this.setBodyType(bodyType);
        this.setDistanceLs(distanceLs);
    }

    public static ScannedBodyInfo fromScannedAndSortedWords(String screenshotFilename, String systemName, List<MatchGroup> bodyNameWords, List<MatchGroup> bodyInfoWords, List<Body> eddbBodies) {
        String bodyName = null;
        BodyInfo bodyType = null;
        BigDecimal distanceLs = null;
        LinkedList<String> lowercasedScannedNameWords = new LinkedList<>();
        for (MatchGroup w : bodyNameWords) {
            lowercasedScannedNameWords.add(w.getText().toLowerCase());
        }
        int indexArrivalPoint = indexOfWords(lowercasedScannedNameWords, "arrival", "point:");
        if (indexArrivalPoint >= lowercasedScannedNameWords.size()) {
            logger.warn("indexArrivalPoint not found in " + screenshotFilename);
        } else {
            // Everything before arrival point is planet name
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < indexArrivalPoint; i++) {
                lowercasedScannedNameWords.set(i, null);
                sb.append(bodyNameWords.get(i).getText()).append(" ");
            }
            bodyName = removePixelErrorsFromBodyName(sb.toString().trim());
            bodyName = fixGeneratedBodyName(systemName, bodyName);

            // The first one after arrival point is the actual distance
            for (int i = indexArrivalPoint; i < lowercasedScannedNameWords.size(); i++) {
                if (lowercasedScannedNameWords.get(i) != null) {
                    String distanceText = lowercasedScannedNameWords.set(i, null);
                    try {
                        String parseableText = distanceText.replace("ls", ""); // remove trailing LS
                        parseableText = parseableText.replace("o", "0").replace("d", "0").replace("s", "5").replace("b", "8"); // fix digits
                        if (!parseableText.matches("\\d{1,3}\\.\\d{2}") && !parseableText.matches("\\d{1,3},\\d{3}\\.\\d{2}")) {
                            logger.warn(screenshotFilename + ": '" + distanceText + "' does not look like distance from arrival point");
                        } else {
                            distanceLs = new BigDecimal(parseableText.replace(",", ""));
                        }
                    } catch (NumberFormatException e) {
                        logger.warn(screenshotFilename + ": Cannot parse '" + distanceText + "' to distance from arrival point");
                    }
                    break;
                }
            }

            // Everything else is body type
            sb = new StringBuilder();
            for (int i = indexArrivalPoint; i < lowercasedScannedNameWords.size(); i++) {
                if (lowercasedScannedNameWords.get(i) != null) {
                    sb.append(lowercasedScannedNameWords.set(i, null)).append(" ");
                }
            }
            String bodyTypeText = sb.toString().trim();
            bodyTypeText = bodyTypeText.replace("ocly", "ody"); // d broken into c and l
            bodyType = BodyInfo.findBestMatching(bodyTypeText);
            if (bodyType == null) {
                logger.warn(screenshotFilename + ": Cannot parse '" + bodyTypeText + "' to body type");
            }
        }

        // See if it is a known body on EDDB, in which case we can greatly improve auto-learning from the correct values (assuming EDDB has correct info^^)
        Body eddbBody = lookupEddbBody(eddbBodies, bodyName, distanceLs, bodyType);
        if (eddbBody == null) {
            logger.debug(screenshotFilename + ": Did not find body on EDDB: " + bodyName);
        } else {
            logger.debug(screenshotFilename + ": Found body on EDDB: " + eddbBody);
            autoLearnBody(eddbBody, bodyName, distanceLs, bodyType, bodyNameWords, indexArrivalPoint, screenshotFilename);
        }

        ScannedBodyInfo scannedBodyInfo = new ScannedBodyInfo(screenshotFilename, systemName, bodyName, bodyType, distanceLs);

        // Create a LinkedList which makes replacing found words easy, and also lowercase all scanned words in this step
        LinkedList<String> lowercasedScannedWords = new LinkedList<>();
        for (MatchGroup w : bodyInfoWords) {
            lowercasedScannedWords.add(w.getText().toLowerCase());
        }

        // Search the start indexes of labels, also replacing the found words with NULL entries
        int indexEarthMasses = indexOfWords(lowercasedScannedWords, "earth", "masses:");
        if (indexEarthMasses < lowercasedScannedWords.size()) {
            // Clear everything before earth masses because it is only bla bla about the body
            for (int i = 0; i < indexEarthMasses; i++) {
                lowercasedScannedWords.set(i, null);
            }
        }
        int indexRadius = indexOfWords(lowercasedScannedWords, "radius:");
        int indexGravity = indexOfWords(lowercasedScannedWords, "gravity:");
        int indexSurfaceTemp = indexOfWords(lowercasedScannedWords, "surface", "temp:");
        int indexVolcanism = indexOfWords(lowercasedScannedWords, "volcanism:");
        int indexAtmosphereType = indexOfWords(lowercasedScannedWords, "atmosphere", "type:");
        int indexComposition = indexOfWords(lowercasedScannedWords, "composition:");
        int indexOrbitalPeriod = indexOfWords(lowercasedScannedWords, "orbital", "period:");
        int indexSemiMajorAxis = indexOfWords(lowercasedScannedWords, "semi", "major", "axis:");
        int indexOrbitalEccentricity = indexOfWords(lowercasedScannedWords, "orbital", "eccentricity:");
        int indexOrbitalInclination = indexOfWords(lowercasedScannedWords, "orbital", "inclination:");
        int indexArgOfPeriapsis = indexOfWords(lowercasedScannedWords, "arg", "of", "periapsis:");
        int indexRotationalPeriod = indexOfWords(lowercasedScannedWords, "rotational", "period:");
        int indexTidallyLocked = indexOfWords(lowercasedScannedWords, "(tidally", "locked)");
        int indexAxialTilt = indexOfWords(lowercasedScannedWords, "axial", "tilt:");
        int indexPlanetMaterials = indexOfWords(lowercasedScannedWords, "planet", "materials:");

        LinkedHashMap<String, Integer> indexByLabel = new LinkedHashMap<>();
        if (indexEarthMasses < lowercasedScannedWords.size()) {
            indexByLabel.put("earth masses:", indexEarthMasses);
        } else {
            logger.warn("indexEarthMasses not found in " + screenshotFilename);
        }
        if (indexRadius < lowercasedScannedWords.size()) {
            indexByLabel.put("radius:", indexRadius);
        } else {
            logger.warn("indexRadius not found in " + screenshotFilename);
        }
        if (indexGravity < lowercasedScannedWords.size()) {
            indexByLabel.put("gravity:", indexGravity);
        } else {
            logger.warn("indexGravity not found in " + screenshotFilename);
        }
        if (indexSurfaceTemp < lowercasedScannedWords.size()) {
            indexByLabel.put("surface temp:", indexSurfaceTemp);
        } else {
            logger.warn("indexSurfaceTemp not found in " + screenshotFilename);
        }
        if (indexVolcanism < lowercasedScannedWords.size()) {
            indexByLabel.put("volcanism:", indexVolcanism);
        } else {
            logger.warn("indexVolcanism not found in " + screenshotFilename);
        }
        if (indexAtmosphereType < lowercasedScannedWords.size()) {
            indexByLabel.put("atmosphere type:", indexAtmosphereType);
        } else {
            logger.warn("indexAtmosphereType not found in " + screenshotFilename);
        }
        if (indexComposition < lowercasedScannedWords.size()) {
            indexByLabel.put("composition:", indexComposition);
        } else {
            logger.warn("indexComposition not found in " + screenshotFilename);
        }
        if (indexOrbitalPeriod < lowercasedScannedWords.size()) {
            indexByLabel.put("orbital period:", indexOrbitalPeriod);
        } else {
            logger.warn("indexOrbitalPeriod not found in " + screenshotFilename);
        }
        if (indexSemiMajorAxis < lowercasedScannedWords.size()) {
            indexByLabel.put("semi major axis:", indexSemiMajorAxis);
        } else {
            logger.warn("indexSemiMajorAxis not found in " + screenshotFilename);
        }
        if (indexOrbitalEccentricity < lowercasedScannedWords.size()) {
            indexByLabel.put("orbital eccentricity:", indexOrbitalEccentricity);
        } else {
            logger.warn("indexOrbitalEccentricity not found in " + screenshotFilename);
        }
        if (indexOrbitalInclination < lowercasedScannedWords.size()) {
            indexByLabel.put("orbital inclination:", indexOrbitalInclination);
        } else {
            logger.warn("indexOrbitalInclination not found in " + screenshotFilename);
        }
        if (indexArgOfPeriapsis < lowercasedScannedWords.size()) {
            indexByLabel.put("arg of periapsis:", indexArgOfPeriapsis);
        } else {
            logger.warn("indexArgOfPeriapsis not found in " + screenshotFilename);
        }
        if (indexRotationalPeriod < lowercasedScannedWords.size()) {
            indexByLabel.put("rotational period:", indexRotationalPeriod);
        } else {
            logger.warn("indexRotationalPeriod not found in " + screenshotFilename);
        }
        if (indexTidallyLocked < lowercasedScannedWords.size()) {
            indexByLabel.put("(tidally locked)", indexTidallyLocked);
        } else {
            //logger.warn("indexTidallyLocked not found in "+screenshotFilename);
        }
        if (indexAxialTilt < lowercasedScannedWords.size()) {
            indexByLabel.put("axial tilt:", indexAxialTilt);
        } else {
            logger.warn("indexAxialTilt not found in " + screenshotFilename);
        }
        if (indexPlanetMaterials < lowercasedScannedWords.size()) {
            indexByLabel.put("planet materials:", indexPlanetMaterials);
        } else {
            logger.warn("indexPlanetMaterials not found in " + screenshotFilename);
        }

        List<Integer> sortedIndexes = new ArrayList<>(indexByLabel.values());
        Collections.sort(sortedIndexes);

        if (indexEarthMasses < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexEarthMasses, "EARTHMASSES:", new EarthMassesFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setEarthMasses(new BigDecimal(value));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexRadius < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexRadius, "RADIUS:", new RadiusFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setRadiusKm(new BigDecimal(value.replace(",", "").replace("KM", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexGravity < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexGravity, "GRAVITY:", new GravityFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setGravityG(new BigDecimal(value.replace("G", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexSurfaceTemp < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexSurfaceTemp, "SURFACETEMP:", new SurfaceTempFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setSurfaceTempK(new BigDecimal(value.replace("K", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexVolcanism < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexVolcanism, "VOLCANISM:", new VolcanismFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setVolcanism(BodyInfo.findBestMatching(value));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexAtmosphereType < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexAtmosphereType, "ATMOSPHERETYPE:", new AtmosphereTypeFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setAtmosphereType(BodyInfo.findBestMatching(value));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexComposition < lowercasedScannedWords.size()) {
            // Concatenate the whole remaining text
            // Remember the real start and end index for later auto-learning.
            String wholeRemainingText = "";
            int z = sortedIndexes.indexOf(indexComposition) + 1;
            int nextIndex = z < sortedIndexes.size() ? sortedIndexes.get(z) : lowercasedScannedWords.size();
            Integer realStartIndexIncl = null;
            Integer realEndEndexExcl = null;
            for (int i = indexComposition; i < nextIndex; i++) {
                if (lowercasedScannedWords.get(i) != null) {
                    if (wholeRemainingText.length() > 0) {
                        wholeRemainingText += " ";
                    }
                    wholeRemainingText += lowercasedScannedWords.set(i, null);
                    if (realStartIndexIncl == null) {
                        realStartIndexIncl = i;
                    }
                    realEndEndexExcl = i + 1;
                }
            }
            // Now we should have s.th. like this:
            // 70.0% ice 20.o% rock 10.0% meta1
            // Split this at every whitespace
            String[] percentagesAndNames = wholeRemainingText.split("\\s");
            // Now we should have a list like this:
            // [70.0%, ice, ,20.o%, ..., meta1]
            // What we can do now is to fix wrongly detected chars as in
            // 20.o%
            // or
            // meta1
            boolean unfixableError = false;
            List<String> fixedPercentagesAndNames = new ArrayList<>(percentagesAndNames.length);
            for (int i = 0; i < percentagesAndNames.length; i++) {
                if (i % 2 == 1) {
                    // Should be a name
                    String fixedName = percentagesAndNames[i].replace("0", "o").replace("5", "s").replace("8", "b");
                    BodyInfo mat = BodyInfo.findBestMatching(fixedName);
                    if (mat != null) {
                        fixedPercentagesAndNames.add(mat.getName());
                    } else {
                        logger.warn(screenshotFilename + ": Unfixable solid: " + percentagesAndNames[i]);
                        unfixableError = true;
                    }
                } else {
                    // Should be a percentage
                    String fixedPercentage = percentagesAndNames[i].replace("o", "0").replace("d", "0").replace("s", "5").replace("b", "8").replace(",", ".");
                    if (fixedPercentage.indexOf(".") == fixedPercentage.length() - 3) {
                        fixedPercentage = fixedPercentage.substring(0, fixedPercentage.length() - 1) + "%";
                    }
                    if (fixedPercentage.matches("\\d{1,3}\\.\\d%")) {
                        fixedPercentagesAndNames.add(fixedPercentage);
                    } else {
                        logger.warn(screenshotFilename + ": Unfixable percentage: " + percentagesAndNames[i]);
                        unfixableError = true;
                    }
                }
            }
            if (!unfixableError) {
                // Now we now what the whole text should have been. We can build it together from the fixed parts.
                String fixedWholeRemainingText = "";
                for (int i = 0; i < fixedPercentagesAndNames.size(); i++) {
                    if (fixedWholeRemainingText.length() > 0) {
                        fixedWholeRemainingText += " ";
                    }
                    fixedWholeRemainingText += fixedPercentagesAndNames.get(i);
                }
                // The fixed text can later be used for auto-learning.
                // First though parse all mats and do a plausi check. The sum of percentages should be 100% +/- 0.1%.
                // If not we might haved missed something completely. This can happen if the text is too close to the border.
                LinkedHashMap<BodyInfo, BigDecimal> composition = new LinkedHashMap<>();
                BigDecimal totalPercentage = BigDecimal.ZERO;
                for (int i = 0; i < fixedPercentagesAndNames.size(); i += 2) {
                    BigDecimal percentage = new BigDecimal(fixedPercentagesAndNames.get(i).replace("%", ""));
                    BodyInfo mat = BodyInfo.findBestMatching(fixedPercentagesAndNames.get(i + 1));
                    composition.put(mat, percentage);
                    totalPercentage = totalPercentage.add(percentage);
                }
                if (Math.abs(100.0 - totalPercentage.doubleValue()) > 0.1) {
                    logger.warn(screenshotFilename + ": Sum of body composition is " + totalPercentage + ": " + fixedWholeRemainingText);
                } else {
                    scannedBodyInfo.setComposition(composition);
                    // >>>> BEGIN AUTO LEARNING >>>>
                    if (fixedWholeRemainingText.length() == wholeRemainingText.length()) {
                        String autoLearnText = fixedWholeRemainingText.replaceAll("\\s", "");
                        for (int i = realStartIndexIncl; i < realEndEndexExcl; i++) {
                            MatchGroup mg = bodyInfoWords.get(i);
                            for (TemplateMatch m : mg.getGroupMatches()) {
                                String shouldHaveBeen = autoLearnText.substring(0, m.getTemplate().getText().length());
                                autoLearnText = autoLearnText.substring(m.getTemplate().getText().length());
                                if (!m.getTemplate().getText().equals(shouldHaveBeen)) {
                                    doAutoLearn(m, shouldHaveBeen, screenshotFilename);
                                }
                            }
                        }
                    }
                    // <<<< END AUTO LEARNING <<<<
                }
            }
        }
        if (indexOrbitalPeriod < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexOrbitalPeriod, "ORBITALPERIOD:", new OrbitalPeriodFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setOrbitalPeriodD(new BigDecimal(value.replace(",", "").replace("D", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexSemiMajorAxis < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexSemiMajorAxis, "SEMIMAJORAXIS:", new SemiMajorAxisFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setSemiMajorAxisAU(new BigDecimal(value.replace("AU", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexOrbitalEccentricity < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexOrbitalEccentricity, "ORBITALECCENTRICITY:", new OrbitalEccentricityFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setOrbitalEccentricity(new BigDecimal(value));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexOrbitalInclination < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexOrbitalInclination, "ORBITALINCLINATION:", new OrbitalInclinationFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setOrbitalInclinationDeg(new BigDecimal(value.replace("°", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexArgOfPeriapsis < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexArgOfPeriapsis, "ARGOFPERIAPSIS:", new ArgOfPeriapsisFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setArgOfPeriapsisDeg(new BigDecimal(value.replace("°", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexRotationalPeriod < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexRotationalPeriod, "ROTATIONALPERIOD:", new RotationalPeriodFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setRotationalPeriodD(new BigDecimal(value.replace("D", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexTidallyLocked < lowercasedScannedWords.size()) {
            try {
                scannedBodyInfo.setTidallyLocked(true);
                valueForLabel(indexTidallyLocked, "(TIDALLYLOCKED)", new TidallyLockedFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        } else if (indexRotationalPeriod < lowercasedScannedWords.size()) {
            scannedBodyInfo.setTidallyLocked(false); // If we have info about the rotational period, but the text "(tidally locked)" is missing, then it is not locked
        }
        if (indexAxialTilt < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexAxialTilt, "AXIALTILT:", new AxialTiltFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setAxialTiltDeg(new BigDecimal(value.replace("°", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexPlanetMaterials < lowercasedScannedWords.size()) {
            // Concatenate the whole remaining text
            // Remember the real start and end index for later auto-learning.
            String wholeRemainingText = "";
            int z = sortedIndexes.indexOf(indexPlanetMaterials) + 1;
            int nextIndex = z < sortedIndexes.size() ? sortedIndexes.get(z) : lowercasedScannedWords.size();
            Integer realStartIndexIncl = null;
            Integer realEndEndexExcl = null;
            for (int i = indexPlanetMaterials; i < nextIndex; i++) {
                if (lowercasedScannedWords.get(i) != null) {
                    wholeRemainingText += lowercasedScannedWords.set(i, null);
                    if (realStartIndexIncl == null) {
                        realStartIndexIncl = i;
                    }
                    realEndEndexExcl = i + 1;
                }
            }
            // Sometimes the back button has been successfully scanned. Remove everything starting at the back button.
            if (wholeRemainingText.endsWith("%)back") || wholeRemainingText.endsWith("%)8ack")) {
                wholeRemainingText = wholeRemainingText.substring(0, wholeRemainingText.length() - 4);
                realEndEndexExcl--;
            }
            // Now we should have s.th. like this:
            // iron(19.1%),sulphur(18.5%),carbon(15.6%),nickel(14.5%),ph0sph0rus(10.o%),chromium(8.6%).oermanium(5.5%),zinc(5.2%)niobium(1.3%),tungsten(1.1%),technetium(0.7%)
            // Split this at every closing bracket.
            // !!! IT IS VERY IMPORTANT THAT BRACKETS ARE DETECTED CORRECTLY, OTHERWISE THIS WILL NOT WORK !!!
            String[] matsAndPercentages = wholeRemainingText.split("\\)");
            // Now we should have a lot of these:
            // iron(19.1%
            // ,sulphur(18.5%
            // ,carbon(15.6%
            // ...
            // ,technetium(0.7%
            // Split each of them at the opening bracket in order to separate the material from the percentage.
            // !!! IT IS VERY IMPORTANT THAT BRACKETS ARE DETECTED CORRECTLY, OTHERWISE THIS WILL NOT WORK !!!
            boolean bracketError = false;
            List<String> matsAndPercentagesSeparated = new ArrayList<>(2 * matsAndPercentages.length);
            for (String s : matsAndPercentages) {
                if (",".equals(s)) {
                    logger.warn(screenshotFilename + ": Incomplete planet materials screenshot: " + wholeRemainingText);
                } else {
                    String[] matAndPercentage = s.split("\\(");
                    if (matAndPercentage.length != 2) {
                        bracketError = true;
                        break;
                    } else {
                        matsAndPercentagesSeparated.add(matAndPercentage[0].replace(".", ",")); // fix . to ,
                        matsAndPercentagesSeparated.add(matAndPercentage[1].replace(",", ".")); // fix , to .
                    }
                }
            }
            if (bracketError) {
                throw new RuntimeException(screenshotFilename + ": Bracket error for planet materials: " + wholeRemainingText);
            } else {
                // Now we should have a list like this:
                // [iron, 19.1%, ,sulphur, 18.5%, ,carbon, 15.6%, ... ,technetium, 0.7%]
                // What we can do now is to fix wrongly detected chars as in
                // ph0sph0rus(10.o%)
                boolean unfixableError = false;
                List<String> fixedMatsAndPercentages = new ArrayList<>(matsAndPercentagesSeparated.size());
                for (int i = 0; i < matsAndPercentagesSeparated.size(); i++) {
                    if (i % 2 == 0) {
                        // Should be a mat
                        String fixedName = matsAndPercentagesSeparated.get(i).replace("0", "o").replace("5", "s").replace("8", "b");
                        Item mat = Item.findBestMatching(fixedName.replace(",", ""), ItemType.ELEMENT);
                        if (mat != null) {
                            fixedMatsAndPercentages.add((fixedName.startsWith(",") ? "," : "") + mat.getName());
                        } else {
                            logger.warn(screenshotFilename + ": Unfixable material: " + matsAndPercentagesSeparated.get(i));
                            unfixableError = true;
                        }
                    } else {
                        // Should be a percentage
                        String fixedPercentage = matsAndPercentagesSeparated.get(i).replace("o", "0").replace("d", "0").replace("s", "5").replace("b", "8").replace(",", ".");
                        if (fixedPercentage.indexOf(".") == fixedPercentage.length() - 3) {
                            fixedPercentage = fixedPercentage.substring(0, fixedPercentage.length() - 1) + "%";
                        }
                        if (fixedPercentage.matches("\\d{1,2}\\.\\d%")) {
                            fixedMatsAndPercentages.add(fixedPercentage);
                        } else {
                            logger.warn(screenshotFilename + ": Unfixable percentage: " + matsAndPercentagesSeparated.get(i));
                            unfixableError = true;
                        }
                    }
                }
                if (!unfixableError) {
                    // Now we now what the whole text should have been. We can build it together from the fixed parts.
                    // The brackets must be inserted again as they have been lost when using the split() function.
                    String fixedWholeRemainingText = "";
                    for (int i = 0; i < fixedMatsAndPercentages.size(); i++) {
                        if (i % 2 == 0) {
                            fixedWholeRemainingText += fixedMatsAndPercentages.get(i);
                        } else {
                            fixedWholeRemainingText += ("(" + fixedMatsAndPercentages.get(i) + ")");
                        }
                    }
                    // The fixed text can later be used for auto-learning.
                    // First though parse all mats and do a plausi check. The sum of percentages should be 100% +/- 0.1%.
                    // If not we might haved missed something completely. This can happen if the text is too close to the border.
                    LinkedHashMap<Item, BigDecimal> planetMaterials = new LinkedHashMap<>();
                    BigDecimal totalPercentage = BigDecimal.ZERO;
                    for (int i = 0; i < fixedMatsAndPercentages.size(); i += 2) {
                        Item mat = Item.findBestMatching(fixedMatsAndPercentages.get(i).replace(",", ""), ItemType.ELEMENT);
                        BigDecimal percentage = new BigDecimal(fixedMatsAndPercentages.get(i + 1).replace("%", ""));
                        planetMaterials.put(mat, percentage);
                        totalPercentage = totalPercentage.add(percentage);
                    }
                    if (Math.abs(100.0 - totalPercentage.doubleValue()) > 0.1) {
                        logger.warn(screenshotFilename + ": Sum of planet materials is " + totalPercentage + ": " + fixedWholeRemainingText);
                    } else {
                        scannedBodyInfo.setPlanetMaterials(planetMaterials);
                        // >>>> BEGIN AUTO LEARNING >>>>
                        if (fixedWholeRemainingText.length() == wholeRemainingText.length()) {
                            String autoLearnText = fixedWholeRemainingText;
                            for (int i = realStartIndexIncl; i < realEndEndexExcl; i++) {
                                MatchGroup mg = bodyInfoWords.get(i);
                                for (TemplateMatch m : mg.getGroupMatches()) {
                                    String shouldHaveBeen = autoLearnText.substring(0, m.getTemplate().getText().length());
                                    autoLearnText = autoLearnText.substring(m.getTemplate().getText().length());
                                    if (!m.getTemplate().getText().equals(shouldHaveBeen)) {
                                        doAutoLearn(m, shouldHaveBeen, screenshotFilename);
                                    }
                                }
                            }
                        }
                        // <<<< END AUTO LEARNING <<<<
                    }
                }
            }
        }

        return scannedBodyInfo;
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
        if (StringUtils.isNotBlank(systemName) && StringUtils.isNotBlank(scannedBodyName)) {
            String bodyNameWithoutDesignation = scannedBodyName.trim();
            while (bodyNameWithoutDesignation.matches("^.+ [0-9OS]{1,2}$") || bodyNameWithoutDesignation.matches("^.+ [A-J]{1,4}$")) {
                bodyNameWithoutDesignation = bodyNameWithoutDesignation.substring(0, bodyNameWithoutDesignation.lastIndexOf(" ")).trim();
            }
            float dist = StringUtils.getLevenshteinDistance(systemName.toLowerCase(), bodyNameWithoutDesignation.toLowerCase());
            float err = dist / systemName.length();
            if (err <= 0.25f) {
                String designationWithoutBodyName = scannedBodyName.replace(bodyNameWithoutDesignation, "").trim();
                String fixedDesignation = fixDesignation(designationWithoutBodyName);
                String fixedName = (systemName + " " + fixedDesignation).trim().toUpperCase();
                if (!fixedName.equals(scannedBodyName)) {
                    logger.info("Fixed '" + scannedBodyName + "' to '" + fixedName + "'");
                }
                return fixedName;
            }
        }
        return scannedBodyName;
    }

    private static String fixDesignation(String scannedDesignation) {
        if (StringUtils.isNotBlank(scannedDesignation)) {
            String[] parts = scannedDesignation.split("\\s");
            int votesForEvenIndexMustBeDigits = 0;
            int votesForEvenIndexMustBeLetters = 0;
            for (int i = 0; i < parts.length; i++) {
                if (parts[i].replace("O", "0").replace("S", "5").matches("\\d+")) {
                    // Digits only (assuming O and S cannot occur)
                    if (i % 2 == 0) {
                        votesForEvenIndexMustBeDigits++;
                    } else {
                        votesForEvenIndexMustBeLetters++;
                    }
                } else if (parts[i].matches("[A-J]+")) {
                    // Letters only
                    if (i % 2 == 0) {
                        votesForEvenIndexMustBeLetters++;
                    } else {
                        votesForEvenIndexMustBeDigits++;
                    }
                }
            }
            boolean evenIndexMustBeDigits = votesForEvenIndexMustBeDigits > votesForEvenIndexMustBeLetters;
            String fixedDesignation = "";
            for (int i = 0; i < parts.length; i++) {
                if (evenIndexMustBeDigits) {
                    if (i % 2 == 0) {
                        // Even indexes must be digits, and we have an even index => convert to digits
                        // O,D,C to 0, S to 5, B to 8
                        fixedDesignation += (" " + parts[i].replace("O", "0").replace("D", "0").replace("C", "0").replace("S", "5").replace("B", "8"));
                    } else {
                        // Even indexes must be digits, but we have an odd index => convert to letters
                        // 0 to D because it is likelier than C and O is too far in the alphabet. 8 to B obviously.
                        fixedDesignation += (" " + parts[i].replace("0", "D").replace("8", "B"));
                    }
                } else {
                    if (i % 2 == 0) {
                        // Even indexes must not be digits, but we have an even index => convert to letters
                        // 0 to D because it is likelier than C and O is too far in the alphabet. 8 to B obviously.
                        fixedDesignation += (" " + parts[i].replace("0", "D").replace("8", "B"));
                    } else {
                        // Even indexes must not be digits, but we have an odd index => convert to digits
                        // O,D,C to 0, S to 5, B to 8
                        fixedDesignation += (" " + parts[i].replace("O", "0").replace("D", "0").replace("C", "0").replace("S", "5").replace("B", "8"));
                    }
                }
            }
            fixedDesignation = fixedDesignation.trim();
            // Now we should have blocks of plain digits and plain letters, no mixed ones
            boolean hasMixed = false;
            for (String s : fixedDesignation.split("\\s")) {
                if (!s.matches("[0-9]+") && !s.matches("[A-J]+")) {
                    logger.warn("Failed to fix '" + scannedDesignation + "' to '" + fixedDesignation + "' - trying the other way round");
                    evenIndexMustBeDigits = !evenIndexMustBeDigits; // Try the other way round
                    hasMixed = true;
                    break;
                }
            }
            if (hasMixed) {
                fixedDesignation = "";
                for (int i = 0; i < parts.length; i++) {
                    if (evenIndexMustBeDigits) {
                        if (i % 2 == 0) {
                            // Even indexes must be digits, and we have an even index => convert to digits
                            // O,D,C to 0, S to 5, B to 8
                            fixedDesignation += (" " + parts[i].replace("O", "0").replace("D", "0").replace("C", "0").replace("S", "5").replace("B", "8"));
                        } else {
                            // Even indexes must be digits, but we have an odd index => convert to letters
                            // 0 to D because it is likelier than C and O is too far in the alphabet. 8 to B obviously.
                            fixedDesignation += (" " + parts[i].replace("0", "D").replace("8", "B"));
                        }
                    } else {
                        if (i % 2 == 0) {
                            // Even indexes must not be digits, but we have an even index => convert to letters
                            // 0 to D because it is likelier than C and O is too far in the alphabet. 8 to B obviously.
                            fixedDesignation += (" " + parts[i].replace("0", "D").replace("8", "B"));
                        } else {
                            // Even indexes must not be digits, but we have an odd index => convert to digits
                            // O,D,C to 0, S to 5, B to 8
                            fixedDesignation += (" " + parts[i].replace("O", "0").replace("D", "0").replace("C", "0").replace("S", "5").replace("B", "8"));
                        }
                    }
                }
                fixedDesignation = fixedDesignation.trim();
                // Now we REALLY should have blocks of plain digits and plain letters, no mixed ones
                for (String s : fixedDesignation.split("\\s")) {
                    if (!s.matches("[0-9]+") && !s.matches("[A-J]+")) {
                        logger.warn("Failed to fix '" + scannedDesignation + "' to '" + fixedDesignation + "' - using the scanned one");
                        return scannedDesignation; // Use the scanned one instead of messing it up even more
                    }
                }
            }
            if (!fixedDesignation.equals(scannedDesignation)) {
                logger.info("Fixed '" + scannedDesignation + "' to '" + fixedDesignation + "' (" + votesForEvenIndexMustBeDigits + ":" + votesForEvenIndexMustBeLetters + " votes for start with digits)");
            }
            return fixedDesignation;
        }
        return scannedDesignation;
    }

    private static Body lookupEddbBody(List<Body> eddbBodies, String bodyName, BigDecimal distanceLs, BodyInfo bodyType) {
        Body bestMatch = null;
        Body exactMatch = null;

        if (eddbBodies != null && bodyName != null && distanceLs != null) {
            for (Body eddbBody : eddbBodies) {
                if ("Planet".equalsIgnoreCase(eddbBody.getGroupName()) && eddbBody.getName() != null && eddbBody.getDistance_to_arrival() != null) {
                    // The name is allowed to have 2 errors if the distance has 0 errors.
                    // If the name has more than 2 errors, or if the distance also has errors, we cannot be certain!
                    //
                    // The distance is allowed to have 1 error if the name has 0 errors.
                    // If the distance has more than 1 error, or if the name also has errors, we cannot be certain!
                    //
                    // Tricky: Our scanned distance always has two decimal places, whereas EDDB has none.
                    // Therefore we test both rounded up and down and use the better one.
                    String scannedName = bodyName.toLowerCase();
                    String eddbName = eddbBody.getName().toLowerCase();
                    String scannedDistanceDown = String.valueOf(distanceLs.longValue()); // w/o fraction = rounded down
                    String scannedDistanceUp = String.valueOf(distanceLs.longValue() + 1); // w/o fraction+1 = rounded up
                    String eddbDistance = String.valueOf(eddbBody.getDistance_to_arrival());

                    int nameError = StringUtils.getLevenshteinDistance(scannedName, eddbName);
                    int distanceErrorDown = StringUtils.getLevenshteinDistance(scannedDistanceDown, eddbDistance);
                    int distanceErrorUp = StringUtils.getLevenshteinDistance(scannedDistanceUp, eddbDistance);
                    int distanceError = Math.min(distanceErrorDown, distanceErrorUp);

                    if (nameError == 0 && distanceError == 0) {
                        exactMatch = eddbBody;
                    } else {
                        if (nameError > 2) {
                            continue; // Name too bad, no matter what
                        } else {
                            // Name has 2 or less errors. If the distance has no error we can be quite certain.
                            if (distanceError > 0) {
                                continue; // We cannot be certain
                            }
                        }
                        if (distanceError > 1) {
                            continue; // Distance too bad, no matter what
                        } else {
                            // Distance has 1 or less errors. If the name has no error we can be quite certain.
                            if (nameError > 0) {
                                continue; // We cannot be certain
                            }
                        }
                        // Still there? Then use as best match!
                        bestMatch = eddbBody;
                    }
                }
            }
        }

        if (exactMatch != null) {
            return exactMatch;
        } else if (bestMatch != null) {
            return bestMatch;
        } else {
            return null;
        }
    }

    private static String valueForLabel(int labelStartIndex, String correctLabel, ValueFixer valueFixer, List<MatchGroup> bodyInfoWords, LinkedList<String> lowercasedScannedWords, List<Integer> sortedIndexes, String screenshotFilename) {
        String value = "";
        int z = sortedIndexes.indexOf(labelStartIndex) + 1;
        int nextIndex = z < sortedIndexes.size() ? sortedIndexes.get(z) : -1;
        List<Integer> labelIndexes = new ArrayList<>();
        List<Integer> valueIndexes = new ArrayList<>();
        for (int i = labelStartIndex; i < nextIndex; i++) {
            if (lowercasedScannedWords.get(i) != null) {
                value += lowercasedScannedWords.set(i, null);
                valueIndexes.add(i);
            } else {
                labelIndexes.add(i);
            }
        }
        // START auto-learn
        String actualLabel = "";
        for (int idx : labelIndexes) {
            MatchGroup mg = bodyInfoWords.get(idx);
            for (TemplateMatch m : mg.getGroupMatches()) {
                actualLabel += m.getTemplate().getText();
            }
        }
        if (actualLabel.length() == correctLabel.length()) {
            String correctLabelRemaining = correctLabel;
            for (int idx : labelIndexes) {
                MatchGroup mg = bodyInfoWords.get(idx);
                for (TemplateMatch m : mg.getGroupMatches()) {
                    if (!correctLabelRemaining.startsWith(m.getTemplate().getText())) {
                        String shouldHaveBeen = correctLabelRemaining.substring(0, m.getTemplate().getText().length());
                        correctLabelRemaining = correctLabelRemaining.substring(m.getTemplate().getText().length());
                        doAutoLearn(m, shouldHaveBeen, screenshotFilename);
                    } else {
                        correctLabelRemaining = correctLabelRemaining.substring(m.getTemplate().getText().length());
                    }
                }
            }
        }
        String correctValue = valueFixer.fixValue(value);
        if (!valueFixer.seemsPlausible(correctValue)) {
            throw new NumberFormatException(value + " does not seem plausible for " + correctLabel.replace(":", ""));
        } else {
            for (int idx : valueIndexes) {
                MatchGroup mg = bodyInfoWords.get(idx);
                for (TemplateMatch m : mg.getGroupMatches()) {
                    if (!correctValue.startsWith(m.getTemplate().getText())) {
                        try {
                            String shouldHaveBeen = correctValue.substring(0, m.getTemplate().getText().length());
                            correctValue = correctValue.substring(m.getTemplate().getText().length());
                            doAutoLearn(m, shouldHaveBeen, screenshotFilename);
                        } catch (StringIndexOutOfBoundsException e) {
                            // Ignore
                        }
                    } else {
                        correctValue = correctValue.substring(m.getTemplate().getText().length());
                    }
                }
            }
        }
        return valueFixer.fixValue(value);
    }

    private static boolean is0vsO(String shouldHaveBeen, String actuallyIs) {
        return ("0".equals(shouldHaveBeen) || "O".equals(shouldHaveBeen)) && ("0".equals(actuallyIs) || "O".equals(actuallyIs));
    }

    private static void autoLearnBody(Body eddbBody, String scannedBodyName, BigDecimal scannedDistanceLs, BodyInfo scannedBodyType, List<MatchGroup> bodyNameWords, int indexArrivalPoint, String screenshotFilename) {
        try {
            if (ValueFixer.TRUST_EDDB && eddbBody.getName() != null && eddbBody.getName().length() > 0) {
                // Ignore case and whitespaces for body name
                if (!scannedBodyName.toLowerCase().replaceAll("\\s", "").equals(eddbBody.getName().toLowerCase().replaceAll("\\s", ""))) {
                    learnText(eddbBody.getName().toUpperCase().replaceAll("\\s", ""), bodyNameWords.subList(0, indexArrivalPoint), screenshotFilename);
                }
            } else if (StringUtils.isNotBlank(scannedBodyName)) {
                learnText(scannedBodyName.replaceAll("\\s", ""), bodyNameWords.subList(0, indexArrivalPoint), screenshotFilename);
            }
            learnText("ARRIVAL", Arrays.asList(bodyNameWords.get(indexArrivalPoint)), screenshotFilename);
            learnText("POINT:", Arrays.asList(bodyNameWords.get(indexArrivalPoint + 1)), screenshotFilename);
            if (ValueFixer.TRUST_EDDB && eddbBody.getDistance_to_arrival() != null && eddbBody.getDistance_to_arrival() > 0.0 && scannedDistanceLs != null) {
                double scannedArrivalFraction = scannedDistanceLs.doubleValue() - scannedDistanceLs.longValue();
                learnText(new DecimalFormat("#,##0.00LS", new DecimalFormatSymbols(Locale.US)).format(eddbBody.getDistance_to_arrival().doubleValue() + scannedArrivalFraction), Arrays.asList(bodyNameWords.get(indexArrivalPoint + 2)), screenshotFilename);
            } else if (scannedDistanceLs != null) {
                learnText(new DecimalFormat("#,##0.00LS", new DecimalFormatSymbols(Locale.US)).format(scannedDistanceLs), Arrays.asList(bodyNameWords.get(indexArrivalPoint + 2)), screenshotFilename);
            }
            if (ValueFixer.TRUST_EDDB && eddbBody.getTypeName() != null && eddbBody.getTypeName().length() > 0) {
                learnText(eddbBody.getTypeName().replaceAll("\\s", ""), bodyNameWords.subList(indexArrivalPoint + 3, bodyNameWords.size()), screenshotFilename);
            } else if (scannedBodyType != null) {
                learnText(scannedBodyType.getName().replaceAll("\\s", ""), bodyNameWords.subList(indexArrivalPoint + 3, bodyNameWords.size()), screenshotFilename);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void learnText(final String correctText, List<MatchGroup> matchGroups, String screenshotFilename) {
        String correctValue = correctText;
        for (MatchGroup mg : matchGroups) {
            for (TemplateMatch m : mg.getGroupMatches()) {
                if (!correctValue.startsWith(m.getTemplate().getText())) {
                    try {
                        if (m.getTemplate().getText().matches("[\\.,:'°\\-]") && correctValue.substring(0, 1).matches("\\w")) {
                            continue; // We have scanned a pixel error to a punctuation char
                        }
                        String shouldHaveBeen = correctValue.substring(0, m.getTemplate().getText().length());
                        correctValue = correctValue.substring(m.getTemplate().getText().length());
                        doAutoLearn(m, shouldHaveBeen, screenshotFilename);
                    } catch (StringIndexOutOfBoundsException e) {
                        // Ignore
                    }
                } else {
                    correctValue = correctValue.substring(m.getTemplate().getText().length());
                }
            }
        }
    }

    private static void doAutoLearn(TemplateMatch m, String shouldHaveBeen, String screenshotFilename) {
        if (Constants.LEARN_0_VS_O || !is0vsO(shouldHaveBeen, m.getTemplate().getText())) {
            File autoLearnFolder = new File(Constants.AUTO_LEARNED_DIR, TemplateMatcher.textToFolder(shouldHaveBeen));
            autoLearnFolder.mkdirs();
            try {
                ImageIO.write(m.getSubimage(), "PNG", new File(autoLearnFolder, autoLearnFolder.getName() + "#" + m.getMatch().x + "#" + m.getMatch().y + "#" + screenshotFilename));
                logger.info("Learned new '" + shouldHaveBeen + "'");
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * @param lowercasedScannedWords
     * @param wordsToSearch
     *      Must be lowercased!
     *      <br>
     *      The first occurence of each found word will be removed from lowercasedScannedWords.
     *      <br>
     *      Example: [&quot;planet&quot;, &quot;materials:&quot;, &quot;planet materials:&quot;]
     * @return
     *      Integer.MAX_VALUE if not found, index of the first found word otherwise
     */
    private static int indexOfWords(LinkedList<String> lowercasedScannedWords, String... wordsToSearch) {
        int startIndex = Integer.MAX_VALUE;

        String combinedWordsToSearch = "";
        for (String w : wordsToSearch) {
            combinedWordsToSearch += w.toLowerCase().replaceAll("\\s", "");
        }
        int numWords = wordsToSearch.length;

        int maxLevenshteinDistance = combinedWordsToSearch.length() / 4;
        for (int index = 0; index <= (lowercasedScannedWords.size() - numWords); index++) {
            String combinedScannedWords = "";
            for (int i = 0; i < numWords; i++) {
                if (lowercasedScannedWords.get(index + i) != null) {
                    combinedScannedWords += lowercasedScannedWords.get(index + i);
                }
            }
            String digitsReplaced = combinedScannedWords.replace("0", "o").replace("5", "s").replace("8", "b"); // We do not expect digits, so replace look-alike chars (0 and O, 8 and B)
            if (StringUtils.getLevenshteinDistance(combinedWordsToSearch, digitsReplaced) <= maxLevenshteinDistance) {
                startIndex = Math.min(index, startIndex);
                for (int i = 0; i < numWords; i++) {
                    lowercasedScannedWords.set(index + i, null); // Do not interpret these as another word
                }
                break; // Stop checking other scanned words
            }
        }

        return startIndex;
    }

    public PlausiCheckResult checkPlausi() {
        PlausiCheckResult result = new PlausiCheckResult(this.getScreenshotFilename());
        if (StringUtils.isBlank(this.getSystemName())) {
            result.addErrorField("systemName", "missing"); // TODO We could try to extract it from the body name. System name = body name - body designation.
        }
        if (StringUtils.isBlank(this.getBodyName())) {
            result.addErrorField("bodyName", "missing");
        }
        if (StringUtils.isNotEmpty(this.getBodyName()) && StringUtils.isNotEmpty(this.getSystemName())) {
            // See if the body name is a generated one.
            // For example, if the system is named 'Eoch Prou RS-T d3-94' and the body is simply the system name plus a designation, like 'Eoch Prou RS-T d3-94 A 4'.
            // However it is not generated if it has its own name, for example the body name 'Colonia Foobar' in the system name 'Eoch Prou RS-T d3-94'.
            //
            // First, try to split the body name into a system name and a designation.
            String systemPart = null;
            String designationPart = null;
            Matcher matcher = BODY_DESIGNATION_PATTERN.matcher(this.getBodyName().trim());
            if (matcher.matches()) {
                systemPart = matcher.group(1);
                designationPart = matcher.group(2);
                String withUppercasedDesignation = systemPart + " " + designationPart.toUpperCase();
                if (!withUppercasedDesignation.equals(this.getBodyName())) {
                    result.addFixedField("bodyName", "Uppercased designation: " + this.getBodyName() + " -> " + withUppercasedDesignation);
                    this.setBodyName(withUppercasedDesignation);
                }
                // Now see if the extracted system name and the real system name are very similar. If so, replace the extracted with the real one.
                float error = (float) StringUtils.getLevenshteinDistance(this.getSystemName().toLowerCase(), systemPart.toLowerCase()) / (float) this.getSystemName().length();
                if (error <= 0.25f) {
                    // We most likely have a system name + designation
                    String withRealSystemName = this.getSystemName() + " " + designationPart.toUpperCase();
                    if (!withRealSystemName.equals(this.getBodyName())) {
                        result.addFixedField("bodyName", "Used real system name: " + this.getBodyName() + " -> " + withRealSystemName);
                        this.setBodyName(withRealSystemName);
                    }
                }
            }

            // Finally, see if the first parts of the real system name and the extracted body name are very similar.
            // This of course only works if the body name is longer than the system name.
            if (this.getBodyName().length() > this.getSystemName().length()) {
                String systemOfBody = this.getBodyName().substring(0, this.getSystemName().length());
                float error = (float) StringUtils.getLevenshteinDistance(this.getSystemName().toLowerCase(), systemOfBody.toLowerCase()) / (float) this.getSystemName().length();
                if (error <= 0.25f) {
                    // We most likely have a system name + designation
                    if (designationPart == null) {
                        result.addErrorField("bodyName", "Looks like system+designation, but designation pattern not found in " + this.getBodyName());
                    } else {
                        String withRealSystemName = this.getSystemName() + " " + designationPart.toUpperCase();
                        if (!withRealSystemName.equals(this.getBodyName())) {
                            result.addFixedField("bodyName", "Used real system name: " + this.getBodyName() + " -> " + withRealSystemName);
                            this.setBodyName(withRealSystemName);
                        }
                    }
                }
            }
        }
        // TODO Check all single percentages for being over 100%, for example 32.6% scanned as 32,6% parsed as 326%
        return result;
    }

    public String getScreenshotFilename() {
        return this.screenshotFilename;
    }

    public void setScreenshotFilename(String screenshotFilename) {
        this.screenshotFilename = screenshotFilename;
    }

    public String getSystemName() {
        return this.systemName;
    }

    public void setSystemName(String systemName) {
        this.systemName = systemName;
    }

    public String getBodyName() {
        return this.bodyName;
    }

    public void setBodyName(String bodyName) {
        this.bodyName = bodyName;
    }

    public BodyInfo getBodyType() {
        return this.bodyType;
    }

    public void setBodyType(BodyInfo bodyType) {
        this.bodyType = bodyType;
    }

    public BodyInfo getTerraforming() {
        return this.terraforming;
    }

    public void setTerraforming(BodyInfo terraforming) {
        this.terraforming = terraforming;
    }

    public BigDecimal getDistanceLs() {
        return this.distanceLs;
    }

    public void setDistanceLs(BigDecimal distanceLs) {
        this.distanceLs = distanceLs;
    }

    public BigDecimal getEarthMasses() {
        return this.earthMasses;
    }

    public void setEarthMasses(BigDecimal earthMasses) {
        this.earthMasses = earthMasses;
    }

    public BigDecimal getRadiusKm() {
        return this.radiusKm;
    }

    public void setRadiusKm(BigDecimal radiusKm) {
        this.radiusKm = radiusKm;
    }

    public BigDecimal getGravityG() {
        return this.gravityG;
    }

    public void setGravityG(BigDecimal gravityG) {
        this.gravityG = gravityG;
    }

    public BigDecimal getSurfaceTempK() {
        return this.surfaceTempK;
    }

    public void setSurfaceTempK(BigDecimal surfaceTempK) {
        this.surfaceTempK = surfaceTempK;
    }

    public BodyInfo getVolcanism() {
        return this.volcanism;
    }

    public void setVolcanism(BodyInfo volcanism) {
        this.volcanism = volcanism;
    }

    public BodyInfo getAtmosphereType() {
        return this.atmosphereType;
    }

    public void setAtmosphereType(BodyInfo atmosphereType) {
        this.atmosphereType = atmosphereType;
    }

    public LinkedHashMap<BodyInfo, BigDecimal> getComposition() {
        return this.composition;
    }

    public void setComposition(LinkedHashMap<BodyInfo, BigDecimal> composition) {
        this.composition = composition;
    }

    public BigDecimal getOrbitalPeriodD() {
        return this.orbitalPeriodD;
    }

    public void setOrbitalPeriodD(BigDecimal orbitalPeriodD) {
        this.orbitalPeriodD = orbitalPeriodD;
    }

    public BigDecimal getSemiMajorAxisAU() {
        return this.semiMajorAxisAU;
    }

    public void setSemiMajorAxisAU(BigDecimal semiMajorAxisAU) {
        this.semiMajorAxisAU = semiMajorAxisAU;
    }

    public BigDecimal getOrbitalEccentricity() {
        return this.orbitalEccentricity;
    }

    public void setOrbitalEccentricity(BigDecimal orbitalEccentricity) {
        this.orbitalEccentricity = orbitalEccentricity;
    }

    public BigDecimal getOrbitalInclinationDeg() {
        return this.orbitalInclinationDeg;
    }

    public void setOrbitalInclinationDeg(BigDecimal orbitalInclinationDeg) {
        this.orbitalInclinationDeg = orbitalInclinationDeg;
    }

    public BigDecimal getArgOfPeriapsisDeg() {
        return this.argOfPeriapsisDeg;
    }

    public void setArgOfPeriapsisDeg(BigDecimal argOfPeriapsisDeg) {
        this.argOfPeriapsisDeg = argOfPeriapsisDeg;
    }

    public BigDecimal getRotationalPeriodD() {
        return this.rotationalPeriodD;
    }

    public void setRotationalPeriodD(BigDecimal rotationalPeriodD) {
        this.rotationalPeriodD = rotationalPeriodD;
    }

    public Boolean getTidallyLocked() {
        return this.tidallyLocked;
    }

    public void setTidallyLocked(Boolean tidallyLocked) {
        this.tidallyLocked = tidallyLocked;
    }

    public BigDecimal getAxialTiltDeg() {
        return this.axialTiltDeg;
    }

    public void setAxialTiltDeg(BigDecimal axialTiltDeg) {
        this.axialTiltDeg = axialTiltDeg;
    }

    public LinkedHashMap<Item, BigDecimal> getPlanetMaterials() {
        return this.planetMaterials;
    }

    public void setPlanetMaterials(LinkedHashMap<Item, BigDecimal> planetMaterials) {
        this.planetMaterials = planetMaterials;
    }

}
