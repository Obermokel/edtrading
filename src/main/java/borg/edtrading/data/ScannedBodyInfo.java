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
import borg.edtrading.ocr.fixer.ValueFixer;
import borg.edtrading.ocr.fixer.VolcanismFixer;
import borg.edtrading.util.MatchSorter.MatchGroup;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
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
    private BigDecimal orbitalInclinationDeg = null; // TODO unit?
    private BigDecimal argOfPeriapsisDeg = null; // TODO unit?
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

    public static ScannedBodyInfo fromScannedAndSortedWords(String screenshotFilename, String systemName, List<MatchGroup> bodyNameWords, List<MatchGroup> bodyInfoWords) {
        String bodyName = null;
        BodyInfo bodyType = null;
        BigDecimal distanceLs = null;
        LinkedList<String> lowercasedScannedNameWords = new LinkedList<>();
        for (MatchGroup w : bodyNameWords) {
            lowercasedScannedNameWords.add(w.getText().toLowerCase());
        }
        int indexArrivalPoint = indexOfWords(lowercasedScannedNameWords, "arrival", "point:", "arrival point:");
        if (indexArrivalPoint >= lowercasedScannedNameWords.size()) {
            logger.warn("indexArrivalPoint not found in " + screenshotFilename);
        } else {
            // Everything before arrival point is planet name.
            // However, sometimes the distance is wrongly sorted before the arrival point...
            StringBuilder sb = new StringBuilder();
            boolean distanceBeforeArrival = false;
            for (int i = 0; i < indexArrivalPoint; i++) {
                String word = lowercasedScannedNameWords.set(i, null);
                if (!looksLikeDistanceLs(word)) {
                    sb.append(bodyNameWords.get(i).getText()).append(" ");
                } else {
                    distanceBeforeArrival = true;
                    try {
                        distanceLs = new BigDecimal(word.replace("o", "0").replace("b", "8").replace(",", "").replace("l", "").replace("s", ""));
                    } catch (NumberFormatException e) {
                        logger.warn(screenshotFilename + ": Cannot parse '" + word + "' to distance from arrival point (v1)");
                    }
                    break;
                }
            }
            bodyName = sb.toString().trim();

            // The first one after arrival point is the actual distance
            if (!distanceBeforeArrival) {
                for (int i = indexArrivalPoint; i < lowercasedScannedNameWords.size(); i++) {
                    if (lowercasedScannedNameWords.get(i) != null) {
                        String distanceText = lowercasedScannedNameWords.set(i, null);
                        try {
                            distanceLs = new BigDecimal(distanceText.replace("o", "0").replace("b", "8").replace(",", "").replace("l", "").replace("s", ""));
                        } catch (NumberFormatException e) {
                            logger.warn(screenshotFilename + ": Cannot parse '" + distanceText + "' to distance from arrival point (v2)");
                        }
                        break;
                    }
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
            bodyType = BodyInfo.findBestMatching(bodyTypeText);
            if (bodyType == null) {
                logger.warn(screenshotFilename + ": Cannot parse '" + bodyTypeText + "' to body type");
            }
        }

        ScannedBodyInfo scannedBodyInfo = new ScannedBodyInfo(screenshotFilename, systemName, bodyName, bodyType, distanceLs);

        // Create a LinkedList which makes replacing found words easy, and also lowercase all scanned words in this step
        LinkedList<String> lowercasedScannedWords = new LinkedList<>();
        for (MatchGroup w : bodyInfoWords) {
            lowercasedScannedWords.add(w.getText().toLowerCase());
        }

        // Search the start indexes of labels, also replacing the found words with NULL entries
        int indexEarthMasses = indexOfWords(lowercasedScannedWords, "earth", "masses:", "earth masses:");
        if (indexEarthMasses < lowercasedScannedWords.size()) {
            // Clear everything before earth masses
            for (int i = 0; i < indexEarthMasses; i++) {
                lowercasedScannedWords.set(i, null);
            }
        }
        int indexRadius = indexOfWords(lowercasedScannedWords, "radius:");
        int indexGravity = indexOfWords(lowercasedScannedWords, "gravity:");
        int indexSurfaceTemp = indexOfWords(lowercasedScannedWords, "surface", "temp:", "surface temp:");
        int indexVolcanism = indexOfWords(lowercasedScannedWords, "volcanism:");
        int indexAtmosphereType = indexOfWords(lowercasedScannedWords, "atmosphere", "type:", "atmosphere type:");
        int indexComposition = indexOfWords(lowercasedScannedWords, "composition:");
        int indexOrbitalPeriod = indexOfWords(lowercasedScannedWords, "orbital", "period:", "orbital period:");
        int indexSemiMajorAxis = indexOfWords(lowercasedScannedWords, "semi", "major", "axis:", "semi major", "major axis:", "semi major axis:");
        int indexOrbitalEccentricity = indexOfWords(lowercasedScannedWords, "orbital", "eccentricity:", "orbital eccentricity:");
        int indexOrbitalInclination = indexOfWords(lowercasedScannedWords, "orbital", "inclination:", "orbital inclination:");
        int indexArgOfPeriapsis = indexOfWords(lowercasedScannedWords, "arg", "of", "periapsis:", "arg of", "of periapsis:", "arg of periapsis:");
        int indexRotationalPeriod = indexOfWords(lowercasedScannedWords, "rotational", "period:", "rotational period:");
        int indexTidallyLocked = indexOfWords(lowercasedScannedWords, "(tidally", "locked)", "(tidally locked)");
        int indexAxialTilt = indexOfWords(lowercasedScannedWords, "axial", "tilt:", "axial tilt:");
        int indexPlanetMaterials = indexOfWords(lowercasedScannedWords, "planet", "materials:", "planet materials:");

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
                String value = valueForLabel(indexEarthMasses, "EARTHMASSES:", new EarthMassesFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setEarthMasses(new BigDecimal(value));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexRadius < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexRadius, "RADIUS:", new RadiusFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setRadiusKm(new BigDecimal(value.replace(",", "").replace("KM", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexGravity < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexGravity, "GRAVITY:", new GravityFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setGravityG(new BigDecimal(value.replace("G", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexSurfaceTemp < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexSurfaceTemp, "SURFACETEMP:", new SurfaceTempFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setSurfaceTempK(new BigDecimal(value.replace("K", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexVolcanism < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexVolcanism, "VOLCANISM:", new VolcanismFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setVolcanism(BodyInfo.findBestMatching(value));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexAtmosphereType < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexAtmosphereType, "ATMOSPHERETYPE:", new AtmosphereTypeFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setAtmosphereType(BodyInfo.findBestMatching(value));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexComposition < lowercasedScannedWords.size()) {
            scannedBodyInfo.setComposition(new LinkedHashMap<>());
            BigDecimal sumPercent = BigDecimal.ZERO;
            int z = sortedIndexes.indexOf(indexComposition) + 1;
            int nextIndex = z < sortedIndexes.size() ? sortedIndexes.get(z) : lowercasedScannedWords.size();
            for (int i = indexComposition; i < nextIndex; i++) {
                if (lowercasedScannedWords.get(i) != null) {
                    String percentText = lowercasedScannedWords.set(i, null);
                    String materialText = null;
                    if (percentText.matches(".{3,4}%.{3,99}")) {
                        materialText = percentText.substring(percentText.indexOf("%") + 1);
                        percentText = percentText.replace(materialText, "");
                    }
                    if (materialText == null) {
                        materialText = lowercasedScannedWords.set(++i, null);
                    }
                    if (materialText != null) {
                        BodyInfo material = BodyInfo.findBestMatching(materialText.replace("0", "o").replace("8", "b"));
                        if (material == null) {
                            logger.warn(screenshotFilename + ": Unknown body composition '" + materialText + "'");
                        } else {
                            try {
                                BigDecimal percent = new BigDecimal(percentText.replace("o", "0").replace("b", "8").replace("%", "").replace(",", ""));
                                scannedBodyInfo.getComposition().put(material, percent);
                                sumPercent = sumPercent.add(percent);
                            } catch (NumberFormatException e) {
                                logger.warn(screenshotFilename + ": Cannot parse '" + percentText + "' to percentage for " + material);
                                scannedBodyInfo.getComposition().put(material, null);
                            }
                        }
                    }
                }
            }
            if (sumPercent.doubleValue() < 99.9) {
                logger.warn(screenshotFilename + ": Only " + sumPercent + "% body composition total sum");
            }
        }
        if (indexOrbitalPeriod < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexOrbitalPeriod, "ORBITALPERIOD:", new OrbitalPeriodFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setOrbitalPeriodD(new BigDecimal(value.replace("D", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexSemiMajorAxis < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexSemiMajorAxis, "SEMIMAJORAXIS:", new SemiMajorAxisFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setSemiMajorAxisAU(new BigDecimal(value.replace("AU", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexOrbitalEccentricity < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexOrbitalEccentricity, "ORBITALECCENTRICITY:", new OrbitalEccentricityFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setOrbitalEccentricity(new BigDecimal(value));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexOrbitalInclination < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexOrbitalInclination, "ORBITALINCLINATION:", new OrbitalInclinationFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setOrbitalInclinationDeg(new BigDecimal(value.replace("°", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexArgOfPeriapsis < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexArgOfPeriapsis, "ARGOFPERIAPSIS:", new ArgOfPeriapsisFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setArgOfPeriapsisDeg(new BigDecimal(value.replace("°", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexRotationalPeriod < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexRotationalPeriod, "ROTATIONALPERIOD:", new RotationalPeriodFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setRotationalPeriodD(new BigDecimal(value.replace("D", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexTidallyLocked < lowercasedScannedWords.size()) {
            scannedBodyInfo.setTidallyLocked(true);
        } else if (indexRotationalPeriod < lowercasedScannedWords.size()) {
            scannedBodyInfo.setTidallyLocked(false); // If we have info about the rotational period, but the text "(tidally locked)" is missing, then it is not locked
        }
        if (indexAxialTilt < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexAxialTilt, "AXIALTILT:", new AxialTiltFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes);
                scannedBodyInfo.setAxialTiltDeg(new BigDecimal(value.replace("°", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexPlanetMaterials < lowercasedScannedWords.size()) {
            scannedBodyInfo.setPlanetMaterials(new LinkedHashMap<>());
            BigDecimal sumPercent = BigDecimal.ZERO;
            int z = sortedIndexes.indexOf(indexPlanetMaterials) + 1;
            int nextIndex = z < sortedIndexes.size() ? sortedIndexes.get(z) : lowercasedScannedWords.size();
            for (int i = indexPlanetMaterials; i < nextIndex; i++) {
                if (lowercasedScannedWords.get(i) != null) {
                    String materialText = lowercasedScannedWords.set(i, null);
                    String percentText = null;
                    if (materialText.matches(".+\\(.+%\\).*")) {
                        percentText = materialText.substring(materialText.indexOf("("));
                        materialText = materialText.replace(percentText, "");
                    }
                    Item material = Item.findBestMatching(materialText.replace("0", "o").replace("8", "b"), ItemType.ELEMENT);
                    if (material == null) {
                        logger.warn(screenshotFilename + ": Unknown material '" + materialText + "'");
                    } else {
                        if (percentText == null) {
                            percentText = lowercasedScannedWords.set(++i, null);
                        }
                        try {
                            BigDecimal percent = new BigDecimal(percentText.replace("o", "0").replace("b", "8").replace("(", "").replace(")", "").replace("%", "").replace(",", ""));
                            scannedBodyInfo.getPlanetMaterials().put(material, percent);
                            sumPercent = sumPercent.add(percent);
                        } catch (NumberFormatException e) {
                            logger.warn(screenshotFilename + ": Cannot parse '" + percentText + "' to percentage for " + material);
                            scannedBodyInfo.getPlanetMaterials().put(material, null);
                        }
                    }
                }
            }
            if (sumPercent.doubleValue() < 99.9) {
                logger.warn(screenshotFilename + ": Only " + sumPercent + "% materials total sum");
            }
        }

        return scannedBodyInfo;
    }

    private static String valueForLabel(int labelStartIndex, String correctLabel, ValueFixer valueFixer, List<MatchGroup> bodyInfoWords, LinkedList<String> lowercasedScannedWords, List<Integer> sortedIndexes) {
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
        String correctLabelRemaining = correctLabel;
        for (int idx : labelIndexes) {
            MatchGroup mg = bodyInfoWords.get(idx);
            for (TemplateMatch m : mg.getGroupMatches()) {
                if (!correctLabelRemaining.startsWith(m.getTemplate().getText())) {
                    String shouldHaveBeen = correctLabelRemaining.substring(0, m.getTemplate().getText().length());
                    correctLabelRemaining = correctLabelRemaining.substring(m.getTemplate().getText().length());
                    File autoLearnFolder = new File(Constants.AUTO_LEARNED_DIR, TemplateMatcher.textToFolder(shouldHaveBeen));
                    autoLearnFolder.mkdirs();
                    try {
                        ImageIO.write(m.getSubimage(), "PNG", new File(autoLearnFolder, "Auto-Learned " + System.currentTimeMillis() + ".png"));
                        logger.info("Learned new '" + shouldHaveBeen + "'");
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                } else {
                    correctLabelRemaining = correctLabelRemaining.substring(m.getTemplate().getText().length());
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
                        String shouldHaveBeen = correctValue.substring(0, m.getTemplate().getText().length());
                        correctValue = correctValue.substring(m.getTemplate().getText().length());
                        File autoLearnFolder = new File(Constants.AUTO_LEARNED_DIR, TemplateMatcher.textToFolder(shouldHaveBeen));
                        autoLearnFolder.mkdirs();
                        try {
                            ImageIO.write(m.getSubimage(), "PNG", new File(autoLearnFolder, "Auto-Learned " + System.currentTimeMillis() + ".png"));
                            logger.info("Learned new '" + shouldHaveBeen + "'");
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    } else {
                        correctValue = correctValue.substring(m.getTemplate().getText().length());
                    }
                }
            }
        }
        return valueFixer.fixValue(value);
    }

    private static boolean looksLikeDistanceLs(String word) {
        return word.replace("o", "0").replace("b", "8").matches(".*[\\d\\,]+\\.\\d{2}ls.*");
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

        for (String wordToSearch : wordsToSearch) {
            int maxLevenshteinDistance = wordToSearch.length() / 4;
            for (int index = 0; index < lowercasedScannedWords.size(); index++) {
                String scannedWord = lowercasedScannedWords.get(index);
                if (scannedWord != null) {
                    String digitsReplaced = scannedWord.replace("0", "o").replace("8", "b"); // We do not expect digits, so replace look-alike chars (0 and O, 8 and B)
                    if (StringUtils.getLevenshteinDistance(wordToSearch, digitsReplaced) <= maxLevenshteinDistance) {
                        startIndex = Math.min(index, startIndex);
                        lowercasedScannedWords.set(index, null); // Do not interpret this as another word
                        break; // Stop checking other scanned words, but continue with the next word to search
                    }
                }
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
