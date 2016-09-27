package borg.edtrading;

import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import borg.edtrading.data.Body;
import borg.edtrading.data.Galaxy;
import borg.edtrading.data.Item;
import borg.edtrading.data.ScannedBodyInfo;
import borg.edtrading.data.StarSystem;
import borg.edtrading.eddb.BodyUpdater;
import borg.edtrading.eddb.SystemNotFoundException;
import borg.edtrading.ocr.CharacterFinder;
import borg.edtrading.ocr.ScreenshotCropper;
import borg.edtrading.ocr.ScreenshotPreprocessor;
import borg.edtrading.util.ImageUtil;
import borg.edtrading.util.MatchSorter;
import borg.edtrading.util.MatchSorter.MatchGroup;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.RasterFormatException;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

import javax.imageio.ImageIO;

/**
 * SurfaceMats2
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyInfoApp {

    static final Logger logger = LogManager.getLogger(BodyInfoApp.class);

    public static void main(String[] args) throws IOException, InterruptedException {
        final boolean doEddbUpdate = true;

        // TODO Permanently remember successfully created/updated bodies and do not update them again! Otherwise we might end in a battle with other OCR users...

        FileUtils.cleanDirectory(Constants.TEMP_DIR);
        Galaxy galaxy = Galaxy.readDataFromFiles();
        BodyUpdater bodyUpdater = doEddbUpdate ? new BodyUpdater(args[0], args[1]) : null;

        try {
            List<Template> templates = TemplateMatcher.loadTemplates("Body Info");

            List<ScannedBodyInfo> scannedBodyInfos = new ArrayList<>();

            List<File> screenshotFiles = getScreenshotsFromAllDir();
            for (File screenshotFile : screenshotFiles) {
                try {
                    //logger.debug(screenshotFile.getName());

                    // Fast skip?
                    if (bodyUpdater != null && bodyUpdater.isScreenshotFinished(screenshotFile.getName())) {
                        continue;
                    }

                    // Extract system name from filename
                    String systemName = systemNameFromFilename(screenshotFile);
                    StarSystem eddbStarSystem = galaxy.searchStarSystemByExactName(systemName);
                    List<Body> eddbBodies = eddbStarSystem == null ? Collections.emptyList() : galaxy.searchBodiesOfStarSystem(eddbStarSystem.getId());

                    // Darken and scale to 4k
                    BufferedImage originalImage = ImageIO.read(screenshotFile);
                    BufferedImage fourKImage = ImageUtil.toFourK(originalImage);

                    // Extract planet name, type and distance from arrival
                    BufferedImage bodyNameImage = ScreenshotCropper.cropSystemMapToBodyName(fourKImage);
                    bodyNameImage = ScreenshotPreprocessor.highlightWhiteText(bodyNameImage);
                    List<MatchGroup> bodyNameWords = scanWords(bodyNameImage, templates, screenshotFile.getName());

                    // Extract body info
                    BufferedImage bodyInfoImage = ScreenshotCropper.cropSystemMapToBodyInfo(fourKImage);
                    bodyInfoImage = ScreenshotPreprocessor.highlightWhiteText(bodyInfoImage);
                    List<MatchGroup> bodyInfoWords = scanWords(bodyInfoImage, templates, screenshotFile.getName());

                    // Parse!
                    ScannedBodyInfo scannedBodyInfo = ScannedBodyInfo.fromScannedAndSortedWords(screenshotFile.getName(), systemName, bodyNameWords, bodyInfoWords, eddbBodies);

                    // sysout
                    System.out.println(scannedBodyInfo);
                    printStats(scannedBodyInfos);

                    // Update!
                    if (doEddbUpdate) {
                        bodyUpdater.updateBody(scannedBodyInfo);
                    }
                } catch (SystemNotFoundException e) {
                    logger.error("Failed to update body info for " + screenshotFile.getName(), e);
                }
            }
        } finally {
            if (bodyUpdater != null) {
                bodyUpdater.close();
            }
        }
    }

    static void printStats(List<ScannedBodyInfo> scannedBodyInfos) {
        Map<Item, ScannedBodyInfo> highestOccurences = new TreeMap<>();
        Map<Item, List<BigDecimal>> allOccurences = new TreeMap<>();
        int nPlanets = 0; // Each screenshot represents a planet. However we only count successfully scanned screenshots.
        for (ScannedBodyInfo bi : scannedBodyInfos) {
            if (bi.getPlanetMaterials() != null && bi.getPlanetMaterials().size() > 0) {
                nPlanets++;
                for (Item element : bi.getPlanetMaterials().keySet()) {
                    BigDecimal percentage = bi.getPlanetMaterials().get(element);

                    ScannedBodyInfo bestSoFar = highestOccurences.get(element);
                    if (bestSoFar == null || percentage.compareTo(bestSoFar.getPlanetMaterials().get(element)) > 0) {
                        highestOccurences.put(element, bi);
                    }

                    List<BigDecimal> all = allOccurences.get(element);
                    if (all == null) {
                        all = new ArrayList<>();
                        allOccurences.put(element, all);
                    }
                    all.add(percentage);
                }
            }
        }
        System.out.println("\n>>>> >>>> >>>> >>>> RESULTS FROM " + nPlanets + " PLANETS <<<< <<<< <<<< <<<<\n");
        System.out.println(String.format(Locale.US, "%-15s %10s %10s %10s", "ELEMENT", "HIGHEST", "MEDIAN", "OCCURENCE"));
        System.out.println(String.format(Locale.US, "%-15s %10s %10s %10s", "---------------", "----------", "----------", "----------"));
        for (Item element : highestOccurences.keySet()) {
            String name = highestOccurences.get(element).getBodyName();
            BigDecimal highest = highestOccurences.get(element).getPlanetMaterials().get(element);
            List<BigDecimal> all = allOccurences.get(element);
            Collections.sort(all);
            BigDecimal median = all.get(all.size() / 2);
            BigDecimal frequency = new BigDecimal(all.size()).multiply(new BigDecimal(100)).divide(new BigDecimal(nPlanets), 1, BigDecimal.ROUND_HALF_UP);

            System.out.println(String.format(Locale.US, "%-15s %9.1f%% %9.1f%% %9.1f%%    %s", element.getName(), highest, median, frequency, name));
        }
    }

    static String systemNameFromFilename(File screenshotFile) {
        String systemName = null;
        if (screenshotFile.getName().matches("\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}\\-\\d{2}\\-\\d{2} .+\\.png")) {
            systemName = screenshotFile.getName().substring("0000-00-00 00-00-00 ".length()).replace(".png", "");
        }
        return systemName;
    }

    static List<MatchGroup> scanWords(BufferedImage croppedfourK, List<Template> templates, String screenshotFilename) throws IOException {
        List<Rectangle> characterLocations = CharacterFinder.findCharacterLocations(croppedfourK, false);
        BufferedImage blurredImage = ScreenshotPreprocessor.gaussian(croppedfourK, 2);

        List<TemplateMatch> matches = new ArrayList<>(characterLocations.size());
        for (Rectangle r : characterLocations) {
            try {
                BufferedImage charImage = blurredImage.getSubimage(r.x, r.y, r.width, r.height);
                TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(charImage, templates, r.x, r.y, screenshotFilename);
                if (bestMatch != null) {
                    matches.add(bestMatch);
                }
            } catch (RasterFormatException e) {
                // Ignore
            }
        }

        List<MatchGroup> matchGroups = MatchSorter.sortMatches(matches);
        //return matchGroups.stream().map(mg -> mg.getText()).collect(Collectors.toList());
        return matchGroups;
    }

    private static List<File> getScreenshotsFromAllDir() {
        File allDir = new File(Constants.SURFACE_MATS_DIR, "_4k_");
        File[] screenshotFiles = allDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().endsWith(".png");
            }
        });
        List<File> sortableList = new ArrayList<>(Arrays.asList(screenshotFiles));
        Collections.sort(sortableList, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                return f1.getName().toLowerCase().compareTo(f2.getName().toLowerCase());
            }
        });
        return sortableList;
    }

}
