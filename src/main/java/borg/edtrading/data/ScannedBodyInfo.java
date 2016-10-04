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
import borg.edtrading.ocr.fixer.MoonMassesFixer;
import borg.edtrading.ocr.fixer.OrbitalEccentricityFixer;
import borg.edtrading.ocr.fixer.OrbitalInclinationFixer;
import borg.edtrading.ocr.fixer.OrbitalPeriodFixer;
import borg.edtrading.ocr.fixer.RadiusFixer;
import borg.edtrading.ocr.fixer.RingMassFixer;
import borg.edtrading.ocr.fixer.RingRadiusFixer;
import borg.edtrading.ocr.fixer.RingTypeFixer;
import borg.edtrading.ocr.fixer.RotationalPeriodFixer;
import borg.edtrading.ocr.fixer.SemiMajorAxisFixer;
import borg.edtrading.ocr.fixer.SolarMassesFixer;
import borg.edtrading.ocr.fixer.SolarRadiusFixer;
import borg.edtrading.ocr.fixer.SurfacePressureFixer;
import borg.edtrading.ocr.fixer.SurfaceTempFixer;
import borg.edtrading.ocr.fixer.TidallyLockedFixer;
import borg.edtrading.ocr.fixer.ValueFixer;
import borg.edtrading.ocr.fixer.VolcanismFixer;
import borg.edtrading.util.MatchSorter.MatchGroup;
import borg.edtrading.util.MiscUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;
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

    private String screenshotFilename = null;
    private String systemName = null;
    private String bodyName = null;
    private BodyInfo bodyGroup = null; // Star, Planet, Belt
    // * Star
    // !no distance if arrival!
    // Class G stars are...
    // age (1.656 million years)
    private BigDecimal solarMasses = null;
    private BigDecimal solarRadius = null;
    private BodyInfo bodyType = null; // Rocky, Icy, HMC, ...
    private BodyInfo terraforming = null;
    private BigDecimal distanceLs = null;
    private BodyInfo systemReserves = null;
    private BigDecimal earthMasses = null;
    private BigDecimal radiusKm = null;
    private BigDecimal gravityG = null;
    private BigDecimal surfaceTempK = null;
    private BigDecimal surfacePressureAtmospheres = null;
    private BodyInfo volcanism = null;
    private BodyInfo atmosphereType = null;
    private LinkedHashMap<BodyInfo, BigDecimal> atmosphere = null;
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
    private List<ScannedRingInfo> rings = null;
    // * Belt
    // !no distance if arrival!
    // ringType (Metallic)
    private BigDecimal moonMasses = null;

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("==== ").append(this.getScreenshotFilename()).append(" ====").append("\n");
        sb.append(this.getSystemName()).append(" // ").append(this.getBodyName()).append(" (").append(this.getBodyGroup() == null ? null : this.getBodyGroup().getName()).append(")").append("\n");
        if (this.getBodyType() != null) {
            sb.append(this.getBodyType().getName());
        } else {
            sb.append("null");
        }
        if (this.getTerraforming() != null) {
            sb.append(" // ").append(this.getTerraforming().getName());
        }
        sb.append(" // ").append(String.format(Locale.US, "%.2fLs", this.getDistanceLs())).append("\n");
        if (this.getSystemReserves() != null) {
            sb.append(this.getSystemReserves().getName()).append("\n");
        }
        if (this.getSolarMasses() != null) {
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "SOLAR MASSES:", this.getSolarMasses())).append("\n");
        } else if (this.getMoonMasses() != null) {
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "MOON MASSES:", this.getMoonMasses())).append("\n");
        } else {
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "EARTH MASSES:", this.getEarthMasses())).append("\n");
        }
        if (this.getSolarRadius() != null) {
            sb.append(String.format(Locale.US, "%-21s\t%.4f", "SOLAR RADIUS:", this.getSolarRadius())).append("\n");
        } else {
            sb.append(String.format(Locale.US, "%-21s\t%.0fKM", "RADIUS:", this.getRadiusKm())).append("\n");
        }
        sb.append(String.format(Locale.US, "%-21s\t%.2fG", "GRAVITY:", this.getGravityG())).append("\n");
        sb.append(String.format(Locale.US, "%-21s\t%.0fK", "SURFACE TEMP:", this.getSurfaceTempK())).append("\n");
        sb.append(String.format(Locale.US, "%-21s\t%.2f", "SURFACE PRESSURE:", this.getSurfacePressureAtmospheres())).append("\n");
        sb.append(String.format(Locale.US, "%-21s\t%s", "VOLCANISM:", this.getVolcanism() == null ? null : this.getVolcanism().getName())).append("\n");
        sb.append(String.format(Locale.US, "%-21s\t%s", "ATMOSPHERE TYPE:", this.getAtmosphereType() == null ? null : this.getAtmosphereType().getName())).append("\n");
        if (this.getAtmosphere() != null || this.getAtmosphereType() != BodyInfo.ATMOSPHERE_TYPE_NO_ATMOSPHERE) {
            sb.append(String.format(Locale.US, "%-21s\t", "ATMOSPHERE:"));
            if (this.getAtmosphere() == null) {
                sb.append("null").append("\n");
            } else {
                Iterator<BodyInfo> it = this.getAtmosphere().keySet().iterator();
                while (it.hasNext()) {
                    BodyInfo material = it.next();
                    BigDecimal percent = this.getAtmosphere().get(material);
                    sb.append(String.format(Locale.US, "%.1f%% %s", percent, material.getName())).append(it.hasNext() ? ", " : "\n");
                }
            }
        }
        sb.append(String.format(Locale.US, "%-21s\t", "COMPOSITION:"));
        if (this.getComposition() == null) {
            sb.append("null").append("\n");
        } else {
            Iterator<BodyInfo> it = this.getComposition().keySet().iterator();
            while (it.hasNext()) {
                BodyInfo material = it.next();
                BigDecimal percent = this.getComposition().get(material);
                sb.append(String.format(Locale.US, "%.1f%% %s", percent, material.getName())).append(it.hasNext() ? ", " : "\n");
            }
        }
        sb.append(String.format(Locale.US, "%-21s\t%.1fD", "ORBITAL PERIOD:", this.getOrbitalPeriodD())).append("\n");
        sb.append(String.format(Locale.US, "%-21s\t%.2fAU", "SEMI MAJOR AXIS:", this.getSemiMajorAxisAU())).append("\n");
        sb.append(String.format(Locale.US, "%-21s\t%.4f", "ORBITAL ECCENTRICITY:", this.getOrbitalEccentricity())).append("\n");
        sb.append(String.format(Locale.US, "%-21s\t%.2f°", "ORBITAL INCLINATION:", this.getOrbitalInclinationDeg())).append("\n");
        sb.append(String.format(Locale.US, "%-21s\t%.2f°", "ARG OF PERIAPSIS:", this.getArgOfPeriapsisDeg())).append("\n");
        sb.append(String.format(Locale.US, "%-21s\t%.1fD", "ROTATIONAL PERIOD:", this.getRotationalPeriodD()));
        if (Boolean.TRUE.equals(this.getTidallyLocked())) {
            sb.append(" (TIDALLY LOCKED)").append("\n");
        } else if (Boolean.FALSE.equals(this.getTidallyLocked())) {
            sb.append("\n");
        } else {
            sb.append(" (null)").append("\n");
        }
        sb.append(String.format(Locale.US, "%-21s\t%.2f°", "AXIAL TILT:", this.getAxialTiltDeg() == null ? null : this.getAxialTiltDeg().doubleValue())).append("\n");
        sb.append(String.format(Locale.US, "%-21s\t", "PLANET MATERIALS:"));
        if (this.getPlanetMaterials() == null) {
            sb.append("null").append("\n");
        } else {
            Iterator<Item> it = this.getPlanetMaterials().keySet().iterator();
            while (it.hasNext()) {
                Item material = it.next();
                BigDecimal percent = this.getPlanetMaterials().get(material);
                sb.append(String.format(Locale.US, "%s(%.1f%%)", material.getName(), percent)).append(it.hasNext() ? ", " : "\n");
            }
        }
        if (this.getRings() != null) {
            for (ScannedRingInfo sri : this.getRings()) {
                sb.append(sri.toString()).append("\n");
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
            // Arrival star. Everything is planet name.
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < lowercasedScannedNameWords.size(); i++) {
                lowercasedScannedNameWords.set(i, null);
                sb.append(bodyNameWords.get(i).getText()).append(" ");
            }
            bodyName = removePixelErrorsFromBodyName(sb.toString().trim());
            bodyName = fixGeneratedBodyName(systemName, bodyName);
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
                        String parseableText = distanceText.replace("l5", "").replace("ls", ""); // remove trailing LS (with S possibly detected as 5)
                        parseableText = parseableText.replace("o", "0").replace("d", "0").replace("s", "5").replace("b", "8"); // fix digits
                        parseableText = parseableText.replace(".", ","); // make all to comma
                        if (parseableText.length() >= 4 && parseableText.lastIndexOf(",") == parseableText.length() - 3) {
                            parseableText = parseableText.substring(0, parseableText.lastIndexOf(",")) + "." + parseableText.substring(parseableText.lastIndexOf(",") + 1); // Replace last comma with dot
                        }
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
            bodyType = BodyInfo.findBestMatching(bodyTypeText, "TYPE_");
            if (bodyType == null) {
                logger.warn(screenshotFilename + ": Cannot parse '" + bodyTypeText + "' to body type");
            }
        }

        // See if it is a known body on EDDB, in which case we can greatly improve auto-learning from the correct values (assuming EDDB has correct info^^)
        Body eddbBody = lookupEddbBody(eddbBodies, bodyName, distanceLs, bodyType);
        autoLearnBody(eddbBody, bodyName, distanceLs, bodyType, bodyNameWords, indexArrivalPoint, screenshotFilename);

        ScannedBodyInfo scannedBodyInfo = new ScannedBodyInfo(screenshotFilename, systemName, bodyName, bodyType, distanceLs);

        // Create a LinkedList which makes replacing found words easy, and also lowercase all scanned words in this step
        LinkedList<String> lowercasedScannedWords = new LinkedList<>();
        for (MatchGroup w : bodyInfoWords) {
            lowercasedScannedWords.add(w.getText().toLowerCase());
        }

        LinkedHashMap<String, Integer> indexByLabel = new LinkedHashMap<>();

        // Parse rings at first
        int indexRingType = indexOfWords(lowercasedScannedWords, "ring", "type:");
        while (indexRingType < lowercasedScannedWords.size()) {
            indexByLabel.put("ring type " + indexRingType + ":", indexRingType);
            int indexName = indexOfWords(lowercasedScannedWords, ArrayUtils.add(bodyName.split("\\s"), "ring"));
            if (indexName < lowercasedScannedWords.size()) {
                indexByLabel.put("ring name " + indexRingType + ":", indexName);
            }
            int indexMass = indexOfWords(lowercasedScannedWords, "mass:");
            if (indexMass < lowercasedScannedWords.size()) {
                indexByLabel.put("ring mass " + indexRingType + ":", indexMass);
            }
            int indexSemiMajorAxis = indexOfWords(lowercasedScannedWords, "semi", "major", "axis:");
            if (indexSemiMajorAxis < lowercasedScannedWords.size()) {
                indexByLabel.put("ring semi major axis " + indexRingType + ":", indexSemiMajorAxis);
            }
            int indexInnerRadius = indexOfWords(lowercasedScannedWords, "inner", "radius:");
            if (indexInnerRadius < lowercasedScannedWords.size()) {
                indexByLabel.put("ring inner radius " + indexRingType + ":", indexInnerRadius);
            }
            int indexOuterRadius = indexOfWords(lowercasedScannedWords, "outer", "radius:");
            if (indexOuterRadius < lowercasedScannedWords.size()) {
                indexByLabel.put("ring outer radius " + indexRingType + ":", indexOuterRadius);
            }

            indexRingType = indexOfWords(lowercasedScannedWords, "ring", "type:"); // More rings?
        }

        // Search the start indexes of labels, also replacing the found words with NULL entries
        int indexSolarMasses = indexOfWords(lowercasedScannedWords, "solar", "masses:");
        if (indexSolarMasses < lowercasedScannedWords.size()) {
            // The two word before solar masses can be system reserves - otherwise they are at the very end
            if (indexSolarMasses >= 2) {
                String w1 = lowercasedScannedWords.get(indexSolarMasses - 2);
                String w2 = lowercasedScannedWords.get(indexSolarMasses - 1);
                if (w1 != null && w2 != null) {
                    scannedBodyInfo.setSystemReserves(BodyInfo.findBestMatching(w1 + w2, "RESERVES_"));
                }
            }
            // Clear everything before solar masses because it is only bla bla about the body
            for (int i = 0; i < indexSolarMasses; i++) {
                lowercasedScannedWords.set(i, null);
            }
        }
        int indexEarthMasses = indexOfWords(lowercasedScannedWords, "earth", "masses:");
        if (indexEarthMasses < lowercasedScannedWords.size()) {
            // The two word before earth masses can be system reserves - otherwise they are at the very end
            if (indexEarthMasses >= 2) {
                String w1 = lowercasedScannedWords.get(indexEarthMasses - 2);
                String w2 = lowercasedScannedWords.get(indexEarthMasses - 1);
                if (w1 != null && w2 != null) {
                    scannedBodyInfo.setSystemReserves(BodyInfo.findBestMatching(w1 + w2, "RESERVES_"));
                }
            }
            // Clear everything before earth masses because it is only bla bla about the body
            for (int i = 0; i < indexEarthMasses; i++) {
                lowercasedScannedWords.set(i, null);
            }
        }
        int indexMoonMasses = indexOfWords(lowercasedScannedWords, "moon", "masses:");
        if (indexMoonMasses < lowercasedScannedWords.size()) {
            // The two word before moon masses can be system reserves - otherwise they are at the very end
            if (indexMoonMasses >= 2) {
                String w1 = lowercasedScannedWords.get(indexMoonMasses - 2);
                String w2 = lowercasedScannedWords.get(indexMoonMasses - 1);
                if (w1 != null && w2 != null) {
                    scannedBodyInfo.setSystemReserves(BodyInfo.findBestMatching(w1 + w2, "RESERVES_"));
                }
            }
            // Clear everything before moon masses because it is only bla bla about the body
            for (int i = 0; i < indexMoonMasses; i++) {
                lowercasedScannedWords.set(i, null);
            }
        }
        int indexSolarRadius = indexOfWords(lowercasedScannedWords, "solar", "radius:");
        int indexRadius = indexOfWords(lowercasedScannedWords, "radius:");
        int indexGravity = indexOfWords(lowercasedScannedWords, "gravity:");
        int indexSurfaceTemp = indexOfWords(lowercasedScannedWords, "surface", "temp:");
        int indexSurfacePressure = indexOfWords(lowercasedScannedWords, "surface", "pressure:");
        int indexVolcanism = indexOfWords(lowercasedScannedWords, "volcanism:");
        int indexAtmosphereType = indexOfWords(lowercasedScannedWords, "atmosphere", "type:");
        int indexAtmosphere = indexOfWords(lowercasedScannedWords, "atmosphere:");
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
        int indexStarCatalogueId = indexOfWords(lowercasedScannedWords, "star", "catalogue", "id:");

        List<Integer> indexesReserves = new ArrayList<>(0);
        for (BodyInfo reserves : BodyInfo.byPrefix("RESERVES_")) {
            int indexReserves = lowercasedScannedWords.size();
            while ((indexReserves = indexOfWords(lowercasedScannedWords, reserves.getName().split("\\s"))) < lowercasedScannedWords.size()) {
                // These have no value (label only) and can occur multiple times
                // Remember index
                indexesReserves.add(indexReserves);
                // Parse label and set in scannedBodyInfo
                BodyInfo findBestMatching = BodyInfo.findBestMatching(bodyInfoWords.get(indexReserves).getText() + bodyInfoWords.get(indexReserves + 1).getText(), "RESERVES_");
                scannedBodyInfo.setSystemReserves(findBestMatching);
            }
        }

        if (indexSolarMasses < lowercasedScannedWords.size()) {
            indexByLabel.put("solar masses:", indexSolarMasses);
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_STAR);
        }
        if (indexEarthMasses < lowercasedScannedWords.size()) {
            indexByLabel.put("earth masses:", indexEarthMasses);
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_PLANET);
        }
        if (indexMoonMasses < lowercasedScannedWords.size()) {
            indexByLabel.put("moon masses:", indexMoonMasses);
            scannedBodyInfo.setBodyGroup(BodyInfo.GROUP_BELT);
        }
        if (indexSolarRadius < lowercasedScannedWords.size()) {
            indexByLabel.put("solar radius:", indexSolarRadius);
        }
        if (indexRadius < lowercasedScannedWords.size()) {
            indexByLabel.put("radius:", indexRadius);
        }
        if (indexGravity < lowercasedScannedWords.size()) {
            indexByLabel.put("gravity:", indexGravity);
        }
        if (indexSurfaceTemp < lowercasedScannedWords.size()) {
            indexByLabel.put("surface temp:", indexSurfaceTemp);
        }
        if (indexSurfacePressure < lowercasedScannedWords.size()) {
            indexByLabel.put("surface pressure:", indexSurfacePressure);
        }
        if (indexVolcanism < lowercasedScannedWords.size()) {
            indexByLabel.put("volcanism:", indexVolcanism);
        }
        if (indexAtmosphereType < lowercasedScannedWords.size()) {
            indexByLabel.put("atmosphere type:", indexAtmosphereType);
        }
        if (indexAtmosphere < lowercasedScannedWords.size()) {
            indexByLabel.put("atmosphere:", indexAtmosphere);
        }
        if (indexComposition < lowercasedScannedWords.size()) {
            indexByLabel.put("composition:", indexComposition);
        }
        if (indexOrbitalPeriod < lowercasedScannedWords.size()) {
            indexByLabel.put("orbital period:", indexOrbitalPeriod);
        }
        if (indexSemiMajorAxis < lowercasedScannedWords.size()) {
            indexByLabel.put("semi major axis:", indexSemiMajorAxis);
        }
        if (indexOrbitalEccentricity < lowercasedScannedWords.size()) {
            indexByLabel.put("orbital eccentricity:", indexOrbitalEccentricity);
        }
        if (indexOrbitalInclination < lowercasedScannedWords.size()) {
            indexByLabel.put("orbital inclination:", indexOrbitalInclination);
        }
        if (indexArgOfPeriapsis < lowercasedScannedWords.size()) {
            indexByLabel.put("arg of periapsis:", indexArgOfPeriapsis);
        }
        if (indexRotationalPeriod < lowercasedScannedWords.size()) {
            indexByLabel.put("rotational period:", indexRotationalPeriod);
        }
        if (indexTidallyLocked < lowercasedScannedWords.size()) {
            indexByLabel.put("(tidally locked)", indexTidallyLocked);
        }
        if (indexAxialTilt < lowercasedScannedWords.size()) {
            indexByLabel.put("axial tilt:", indexAxialTilt);
        }
        if (indexPlanetMaterials < lowercasedScannedWords.size()) {
            indexByLabel.put("planet materials:", indexPlanetMaterials);
        }
        if (indexStarCatalogueId < lowercasedScannedWords.size()) {
            indexByLabel.put("star catalogue id:", indexStarCatalogueId);
        }
        for (Integer index : indexesReserves) {
            indexByLabel.put("reserves" + index, index);
        }

        List<Integer> sortedIndexes = new ArrayList<>(indexByLabel.values());
        Collections.sort(sortedIndexes);

        if (indexSolarMasses < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexSolarMasses, "SOLARMASSES:", new SolarMassesFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setSolarMasses(new BigDecimal(value));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexEarthMasses < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexEarthMasses, "EARTHMASSES:", new EarthMassesFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setEarthMasses(new BigDecimal(value.replace(",", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexMoonMasses < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexMoonMasses, "MOONMASSES:", new MoonMassesFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setMoonMasses(new BigDecimal(value));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexSolarRadius < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexSolarRadius, "SOLARRADIUS:", new SolarRadiusFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setSolarRadius(new BigDecimal(value));
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
                scannedBodyInfo.setSurfaceTempK(new BigDecimal(value.replace(",", "").replace("K", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexSurfacePressure < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexSurfacePressure, "SURFACEPRESSURE:", new SurfacePressureFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setSurfacePressureAtmospheres(new BigDecimal(value.replace(",", "").replace("ATMOSPHERES", "")));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexVolcanism < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexVolcanism, "VOLCANISM:", new VolcanismFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setVolcanism(BodyInfo.findBestMatching(value, "VOLCANISM_"));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexAtmosphereType < lowercasedScannedWords.size()) {
            try {
                String value = valueForLabel(indexAtmosphereType, "ATMOSPHERETYPE:", new AtmosphereTypeFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                scannedBodyInfo.setAtmosphereType(BodyInfo.findBestMatching(value, "ATMOSPHERE_TYPE_"));
            } catch (NumberFormatException e) {
                logger.warn(screenshotFilename + ": " + e.getMessage());
            }
        }
        if (indexAtmosphere < lowercasedScannedWords.size()) {
            // Concatenate the whole remaining text
            // Remember the real start and end index for later auto-learning.
            String wholeRemainingText = "";
            int z = sortedIndexes.indexOf(indexAtmosphere) + 1;
            int nextIndex = z < sortedIndexes.size() ? sortedIndexes.get(z) : lowercasedScannedWords.size();
            Integer realStartIndexIncl = null;
            Integer realEndEndexExcl = null;
            for (int i = indexAtmosphere; i < nextIndex; i++) {
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
                    BodyInfo mat = BodyInfo.findBestMatching(fixedName, "ATMOSPHERE_COMPONENT_");
                    if (mat != null) {
                        fixedPercentagesAndNames.add(mat.getName());
                    } else {
                        logger.warn(screenshotFilename + ": Unfixable atmosphere component: " + percentagesAndNames[i]);
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
                // First though parse all mats and do a plausi check. The sum of percentages should be 100% +/- 0.4% (ROSS 847 A 5 has 99.6%).
                // If not we might haved missed something completely. This can happen if the text is too close to the border.
                LinkedHashMap<BodyInfo, BigDecimal> atmosphere = new LinkedHashMap<>();
                BigDecimal totalPercentage = BigDecimal.ZERO;
                for (int i = 0; i < fixedPercentagesAndNames.size(); i += 2) {
                    BigDecimal percentage = new BigDecimal(fixedPercentagesAndNames.get(i).replace("%", ""));
                    BodyInfo mat = BodyInfo.findBestMatching(fixedPercentagesAndNames.get(i + 1), "ATMOSPHERE_COMPONENT_");
                    atmosphere.put(mat, percentage);
                    totalPercentage = totalPercentage.add(percentage);
                }
                if (Math.abs(100.0 - totalPercentage.doubleValue()) > 0.4001) {
                    logger.warn(screenshotFilename + ": Sum of atmosphere components is " + totalPercentage + ": " + fixedWholeRemainingText);
                } else {
                    scannedBodyInfo.setAtmosphere(atmosphere);
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
                    BodyInfo mat = BodyInfo.findBestMatching(fixedName, "COMPOSITION_");
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
                    BodyInfo mat = BodyInfo.findBestMatching(fixedPercentagesAndNames.get(i + 1), "COMPOSITION_");
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
                    // First though parse all mats and do a plausi check. The sum of percentages should be 100% +/- 0.3%. (Cavins B 2 has 99.7%)
                    // If not we might haved missed something completely. This can happen if the text is too close to the border.
                    LinkedHashMap<Item, BigDecimal> planetMaterials = new LinkedHashMap<>();
                    BigDecimal totalPercentage = BigDecimal.ZERO;
                    for (int i = 0; i < fixedMatsAndPercentages.size(); i += 2) {
                        Item mat = Item.findBestMatching(fixedMatsAndPercentages.get(i).replace(",", ""), ItemType.ELEMENT);
                        BigDecimal percentage = new BigDecimal(fixedMatsAndPercentages.get(i + 1).replace("%", ""));
                        planetMaterials.put(mat, percentage);
                        totalPercentage = totalPercentage.add(percentage);
                    }
                    if (Math.abs(100.0 - totalPercentage.doubleValue()) > 0.3) {
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

        for (String indexLabel : indexByLabel.keySet()) {
            if (indexLabel.startsWith("ring type ")) {
                ScannedRingInfo scannedRingInfo = new ScannedRingInfo();

                indexRingType = indexByLabel.get(indexLabel);
                if (indexRingType < lowercasedScannedWords.size()) {
                    try {
                        String value = valueForLabel(indexRingType, "RINGTYPE:", new RingTypeFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                        scannedRingInfo.setRingType(BodyInfo.findBestMatching(value, "RING_TYPE_"));
                    } catch (NumberFormatException e) {
                        logger.warn(screenshotFilename + ": " + e.getMessage());
                    }

                    Integer indexRingName = indexByLabel.get(indexLabel.replace("ring type ", "ring name "));
                    if (indexRingName != null && indexRingName < lowercasedScannedWords.size()) {
                        StringBuilder name = new StringBuilder();
                        for (int i = indexRingName; i < indexRingType; i++) {
                            name.append(bodyInfoWords.get(i).getText()).append(" ");
                        }
                        String scannedRingName = name.toString().trim();
                        String fixedRingName = fixGeneratedBodyName(systemName, scannedRingName);
                        scannedRingInfo.setRingName(fixedRingName);
                    }
                }

                Integer indexRingMass = indexByLabel.get(indexLabel.replace("ring type ", "ring mass "));
                if (indexRingMass != null && indexRingMass < lowercasedScannedWords.size()) {
                    try {
                        String value = valueForLabel(indexRingMass, "MASS:", new RingMassFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                        scannedRingInfo.setMassMt(new BigDecimal(value.replace(",", "").replace("MT", "")));
                    } catch (NumberFormatException e) {
                        logger.warn(screenshotFilename + ": " + e.getMessage());
                    }
                }

                Integer indexRingSemiMajorAxis = indexByLabel.get(indexLabel.replace("ring type ", "ring semi major axis "));
                if (indexRingSemiMajorAxis != null && indexRingSemiMajorAxis < lowercasedScannedWords.size()) {
                    try {
                        String value = valueForLabel(indexRingSemiMajorAxis, "SEMIMAJORAXIS:", new SemiMajorAxisFixer(eddbBody), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                        scannedRingInfo.setSemiMajorAxisAU(new BigDecimal(value.replace(",", "").replace("AU", "")));
                    } catch (NumberFormatException e) {
                        logger.warn(screenshotFilename + ": " + e.getMessage());
                    }
                }

                Integer indexRingInnerRadius = indexByLabel.get(indexLabel.replace("ring type ", "ring inner radius "));
                if (indexRingInnerRadius != null && indexRingInnerRadius < lowercasedScannedWords.size()) {
                    try {
                        String value = valueForLabel(indexRingInnerRadius, "INNERRADIUS:", new RingRadiusFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                        scannedRingInfo.setInnerRadiusKm(new BigDecimal(value.replace(",", "").replace("KM", "")));
                    } catch (NumberFormatException e) {
                        logger.warn(screenshotFilename + ": " + e.getMessage());
                    }
                }

                Integer indexRingOuterRadius = indexByLabel.get(indexLabel.replace("ring type ", "ring outer radius "));
                if (indexRingOuterRadius != null && indexRingOuterRadius < lowercasedScannedWords.size()) {
                    try {
                        String value = valueForLabel(indexRingOuterRadius, "OUTERRADIUS:", new RingRadiusFixer(), bodyInfoWords, lowercasedScannedWords, sortedIndexes, screenshotFilename);
                        scannedRingInfo.setOuterRadiusKm(new BigDecimal(value.replace(",", "").replace("KM", "")));
                    } catch (NumberFormatException e) {
                        logger.warn(screenshotFilename + ": " + e.getMessage());
                    }
                }

                if (scannedBodyInfo.getRings() == null) {
                    scannedBodyInfo.setRings(new ArrayList<>());
                }
                scannedBodyInfo.getRings().add(scannedRingInfo);
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
                float err = MiscUtil.levenshteinError(systemName, partOfScannedBodyName);
                if (err <= 0.25f && err < bestMatchSoFar) {
                    partOfScannedBodyNameToReplaceWithSystemName = partOfScannedBodyName;
                    bestMatchSoFar = err;
                }
            }
            if (partOfScannedBodyNameToReplaceWithSystemName != null) {
                fixedBodyName = scannedBodyName.replace(partOfScannedBodyNameToReplaceWithSystemName, systemName);
                logger.debug("Fixed '" + scannedBodyName + "' to '" + fixedBodyName + "'");
            }

            // If the name does not start with the system convert to camel case, replacing 0 with O
            // Example: L0WING'S ROCK -> Lowing's Rock
            if (partOfScannedBodyNameToReplaceWithSystemName == null) {
                fixedBodyName = WordUtils.capitalizeFully(scannedBodyName.replace("0", "O"));
                logger.debug("Fixed '" + scannedBodyName + "' to '" + fixedBodyName + "'");
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
                    logger.debug("Fixed '" + scannedBodyName + "' to '" + fixedBodyName + "'");
                }
            }

            return fixedBodyName;
        }
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
        int nextIndex = z < sortedIndexes.size() ? sortedIndexes.get(z) : lowercasedScannedWords.size();
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
        boolean is0vsO = ("0".equals(shouldHaveBeen) && "O".equals(actuallyIs)) || ("O".equals(shouldHaveBeen) && "0".equals(actuallyIs));
        boolean isIvsl = ("I".equals(shouldHaveBeen) && "l".equals(actuallyIs)) || ("l".equals(shouldHaveBeen) && "I".equals(actuallyIs));

        return is0vsO || isIvsl;
    }

    private static void autoLearnBody(Body eddbBody, String scannedBodyName, BigDecimal scannedDistanceLs, BodyInfo scannedBodyType, List<MatchGroup> bodyNameWords, int indexArrivalPoint, String screenshotFilename) {
        try {
            if (ValueFixer.TRUST_EDDB && eddbBody != null && eddbBody.getName() != null && eddbBody.getName().length() > 0) {
                // Ignore case and whitespaces for body name
                if (!scannedBodyName.toLowerCase().replaceAll("\\s", "").equals(eddbBody.getName().toLowerCase().replaceAll("\\s", ""))) {
                    learnText(eddbBody.getName().toUpperCase().replaceAll("\\s", ""), bodyNameWords.subList(0, indexArrivalPoint), screenshotFilename);
                }
            } else if (StringUtils.isNotBlank(scannedBodyName)) {
                learnText(scannedBodyName.replaceAll("\\s", ""), bodyNameWords.subList(0, Math.min(indexArrivalPoint, bodyNameWords.size())), screenshotFilename);
            }
            if (indexArrivalPoint < bodyNameWords.size()) {
                learnText("ARRIVAL", Arrays.asList(bodyNameWords.get(indexArrivalPoint)), screenshotFilename);
                learnText("POINT:", Arrays.asList(bodyNameWords.get(indexArrivalPoint + 1)), screenshotFilename);
            }
            if (ValueFixer.TRUST_EDDB && eddbBody != null && eddbBody.getDistance_to_arrival() != null && eddbBody.getDistance_to_arrival() > 0.0 && scannedDistanceLs != null) {
                double scannedArrivalFraction = scannedDistanceLs.doubleValue() - scannedDistanceLs.longValue();
                learnText(new DecimalFormat("#,##0.00LS", new DecimalFormatSymbols(Locale.US)).format(eddbBody.getDistance_to_arrival().doubleValue() + scannedArrivalFraction), Arrays.asList(bodyNameWords.get(indexArrivalPoint + 2)), screenshotFilename);
            } else if (scannedDistanceLs != null) {
                learnText(new DecimalFormat("#,##0.00LS", new DecimalFormatSymbols(Locale.US)).format(scannedDistanceLs), Arrays.asList(bodyNameWords.get(indexArrivalPoint + 2)), screenshotFilename);
            }
            if (ValueFixer.TRUST_EDDB && eddbBody != null && eddbBody.getTypeName() != null && eddbBody.getTypeName().length() > 0) {
                learnText(eddbBody.getTypeName().replaceAll("\\s", ""), bodyNameWords.subList(indexArrivalPoint + 3, bodyNameWords.size()), screenshotFilename);
            } else if (scannedBodyType != null) {
                learnText(scannedBodyType.getName().replaceAll("\\s", ""), bodyNameWords.subList(indexArrivalPoint + 3, bodyNameWords.size()), screenshotFilename);
            }
        } catch (Exception e) {
            logger.error("Auto-learning failed for " + screenshotFilename, e);
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
        if (shouldHaveBeen.equalsIgnoreCase(m.getTemplate().getText())) {
            return; // Just wrong case, probably shouldHaveBeen has been lowercased to look nicer
        }
        if (Constants.LEARN_0_VS_O || !is0vsO(shouldHaveBeen, m.getTemplate().getText())) {
            String folderName = TemplateMatcher.textToFolder(shouldHaveBeen);
            File autoLearnFolder = new File(Constants.AUTO_LEARNED_DIR, folderName);
            autoLearnFolder.mkdirs();
            try {
                ImageIO.write(m.getSubimage(), "PNG", new File(autoLearnFolder, "LEARNED#" + folderName + "#" + m.getMatch().x + "#" + m.getMatch().y + "#" + screenshotFilename));
                logger.trace("Learned new '" + shouldHaveBeen + "' from " + screenshotFilename);
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
            if (combinedWordsToSearch.contains(":") && !combinedScannedWords.contains(":")) {
                continue; // Do not steal words! For example, searching for the LABEL "atmosphere:" would very likely match the VALUE "no atmosphere".
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

    public BodyInfo getBodyGroup() {
        return this.bodyGroup;
    }

    public void setBodyGroup(BodyInfo bodyGroup) {
        this.bodyGroup = bodyGroup;
    }

    public BigDecimal getSolarMasses() {
        return this.solarMasses;
    }

    public void setSolarMasses(BigDecimal solarMasses) {
        this.solarMasses = solarMasses;
    }

    public BigDecimal getSolarRadius() {
        return this.solarRadius;
    }

    public void setSolarRadius(BigDecimal solarRadius) {
        this.solarRadius = solarRadius;
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

    public BodyInfo getSystemReserves() {
        return this.systemReserves;
    }

    public void setSystemReserves(BodyInfo systemReserves) {
        this.systemReserves = systemReserves;
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

    public BigDecimal getSurfacePressureAtmospheres() {
        return this.surfacePressureAtmospheres;
    }

    public void setSurfacePressureAtmospheres(BigDecimal surfacePressureAtmospheres) {
        this.surfacePressureAtmospheres = surfacePressureAtmospheres;
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

    public LinkedHashMap<BodyInfo, BigDecimal> getAtmosphere() {
        return this.atmosphere;
    }

    public void setAtmosphere(LinkedHashMap<BodyInfo, BigDecimal> atmosphere) {
        this.atmosphere = atmosphere;
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

    public List<ScannedRingInfo> getRings() {
        return this.rings;
    }

    public void setRings(List<ScannedRingInfo> rings) {
        this.rings = rings;
    }

    public BigDecimal getMoonMasses() {
        return this.moonMasses;
    }

    public void setMoonMasses(BigDecimal moonMasses) {
        this.moonMasses = moonMasses;
    }

}
