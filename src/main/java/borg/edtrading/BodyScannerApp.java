package borg.edtrading;

import borg.edtrading.bodyscanner.BodyScanner;
import borg.edtrading.bodyscanner.BodyScannerResult;
import borg.edtrading.bodyscanner.ScannedBodyInfo;
import borg.edtrading.data.Item;
import borg.edtrading.eddb.updater.BodyUpdater;
import borg.edtrading.eddb.updater.SystemNotFoundException;
import borg.edtrading.util.ImageUtil;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

import javax.imageio.ImageIO;

/**
 * BodyScannerApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyScannerApp {

    static final Logger logger = LogManager.getLogger(BodyScannerApp.class);

    private static final DateFormat DF_ETA = new SimpleDateFormat("MMM dd @ HH:mm");

    public static void main(String[] args) throws IOException {
        final boolean doEddbUpdate = false;

        BodyUpdater bodyUpdater = doEddbUpdate ? new BodyUpdater(args[0], args[1]) : null;

        try {
            logger.trace("Cleaning dirs...");
            FileUtils.cleanDirectory(Constants.TEMP_DIR);
            FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "KNOWN"));
            FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "UNKNOWN"));
            FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "GUESSED"));
            FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "CRAP"));
            FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "LEARNED_FIXED"));
            FileUtils.cleanDirectory(new File(Constants.TEMPLATES_DIR, "LEARNED_VARIANT"));

            File debugFile = new File(Constants.TEMP_DIR, "debug.txt");

            logger.trace("Creating the scanner...");
            BodyScanner scanner = new BodyScanner();
            scanner.setDebugFinal(true);

            Map<String, ScannedBodyInfo> resultsByBodyName = new TreeMap<>();
            List<File> screenshotFiles = BodyScannerApp.selectAllScreenshots();
            int n = 0;
            int total = screenshotFiles.size();
            int batchSize = 10;
            long startBatch = System.currentTimeMillis();
            for (File screenshotFile : screenshotFiles) {
                try {
                    // Print ETA
                    if (++n % batchSize == 0) {
                        long millis = System.currentTimeMillis() - startBatch;
                        double screenshotsPerSec = ((double) batchSize / Math.max(1, millis)) * 1000d;
                        int screenshotsRemaining = total - n;
                        double secondsRemaining = screenshotsRemaining / screenshotsPerSec;
                        Date eta = new Date(System.currentTimeMillis() + (long) (secondsRemaining * 1000));
                        logger.trace(String.format("Processed %,d of %,d screenshots (%.1f/min) -- ETA %s", n, total, screenshotsPerSec * 60, DF_ETA.format(eta)));
                        startBatch = System.currentTimeMillis();
                    }

                    // Fast skip?
                    if (bodyUpdater != null && (bodyUpdater.isScreenshotFinished(screenshotFile.getName()) || bodyUpdater.isScreenshotFailed(screenshotFile.getName()))) {
                        continue;
                    }

                    // Parse!
                    logger.trace("Processing screenshot " + n + " of " + screenshotFiles.size() + ": " + screenshotFile.getName());
                    BodyScannerResult result = scanner.scanScreenshotFile(screenshotFile);
                    ScannedBodyInfo sbi = result.getScannedBodyInfo();
                    FileUtils.write(debugFile, sbi.toString() + "\n\n", "UTF-8", true);
                    if (result.getFinalDebugImage() != null) {
                        ImageIO.write(ImageUtil.toFullHd(result.getFinalDebugImage()), "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName()));
                    }
                    if (resultsByBodyName.containsKey(sbi.getBodyName())) {
                        ScannedBodyInfo prevSBI = resultsByBodyName.get(sbi.getBodyName());
                        sbi.addMissing(prevSBI);
                    }
                    resultsByBodyName.put(sbi.getBodyName(), sbi);

                    // Update!
                    if (doEddbUpdate) {
                        bodyUpdater.updateBody(sbi);
                    }
                } catch (SystemNotFoundException | IllegalArgumentException e) {
                    logger.error("Failed to update body info for " + screenshotFile.getName(), e);
                }
            }

            List<ScannedBodyInfo> results = new ArrayList<>(resultsByBodyName.size());
            for (String bodyName : resultsByBodyName.keySet()) {
                logger.debug(bodyName);
                results.add(resultsByBodyName.get(bodyName));
            }

            printStats(results);
            System.out.println();
            System.out.println("Highest gravity:          " + highestGravity(results, true).getGravityG() + "G (" + highestGravity(results, true).getBodyName() + ")");
            System.out.println("Shortest orbital period:  " + shortestOrbitalPeriod(results, true).getOrbitalPeriodD() + "D (" + shortestOrbitalPeriod(results, true).getBodyName() + ")");
            System.out.println("Hottest surface temp:     " + hottestSurfaceTemp(results, true).getSurfaceTempK() + "K (" + hottestSurfaceTemp(results, true).getBodyName() + ")");
            System.out.println("Coolest surface temp:     " + coolestSurfaceTemp(results, true).getSurfaceTempK() + "K (" + coolestSurfaceTemp(results, true).getBodyName() + ")");
        } finally {
            if (bodyUpdater != null) {
                bodyUpdater.close();
            }
        }
    }

    static ScannedBodyInfo highestGravity(List<ScannedBodyInfo> scannedBodyInfos, boolean landableOnly) {
        ScannedBodyInfo result = null;
        BigDecimal max = BigDecimal.ZERO;
        for (ScannedBodyInfo sbi : scannedBodyInfos) {
            if (sbi.getGravityG() != null && sbi.getGravityG().compareTo(max) > 0) {
                if (!landableOnly || (sbi.getPlanetMaterials() != null && sbi.getPlanetMaterials().size() > 0)) {
                    result = sbi;
                    max = sbi.getGravityG();
                }
            }
        }
        return result;
    }

    static ScannedBodyInfo shortestOrbitalPeriod(List<ScannedBodyInfo> scannedBodyInfos, boolean landableOnly) {
        ScannedBodyInfo result = null;
        BigDecimal min = new BigDecimal("999999999");
        for (ScannedBodyInfo sbi : scannedBodyInfos) {
            if (sbi.getOrbitalPeriodD() != null && sbi.getOrbitalPeriodD().compareTo(min) < 0) {
                if (!landableOnly || (sbi.getPlanetMaterials() != null && sbi.getPlanetMaterials().size() > 0)) {
                    result = sbi;
                    min = sbi.getOrbitalPeriodD();
                }
            }
        }
        return result;
    }

    static ScannedBodyInfo hottestSurfaceTemp(List<ScannedBodyInfo> scannedBodyInfos, boolean landableOnly) {
        ScannedBodyInfo result = null;
        BigDecimal max = BigDecimal.ZERO;
        for (ScannedBodyInfo sbi : scannedBodyInfos) {
            if (sbi.getSurfaceTempK() != null && sbi.getSurfaceTempK().compareTo(max) > 0) {
                if (!landableOnly || (sbi.getPlanetMaterials() != null && sbi.getPlanetMaterials().size() > 0)) {
                    result = sbi;
                    max = sbi.getSurfaceTempK();
                }
            }
        }
        return result;
    }

    static ScannedBodyInfo coolestSurfaceTemp(List<ScannedBodyInfo> scannedBodyInfos, boolean landableOnly) {
        ScannedBodyInfo result = null;
        BigDecimal min = new BigDecimal("999999999");
        for (ScannedBodyInfo sbi : scannedBodyInfos) {
            if (sbi.getSurfaceTempK() != null && sbi.getSurfaceTempK().compareTo(min) < 0) {
                if (!landableOnly || (sbi.getPlanetMaterials() != null && sbi.getPlanetMaterials().size() > 0)) {
                    result = sbi;
                    min = sbi.getSurfaceTempK();
                }
            }
        }
        return result;
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
        System.out.println("\n>>>> >>>> >>>> >>>> RESULTS FROM " + nPlanets + " LANDABLE PLANETS <<<< <<<< <<<< <<<<\n");
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

    static List<File> selectSpecificScreenshot(String filename) {
        return Arrays.asList(new File(new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR), filename));
    }

    static List<File> selectRandomScreenshot() {
        return selectAllScreenshots().subList(0, 1);
    }

    static List<File> selectAllScreenshots() {
        File dir = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR);
        File[] fileArray = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().endsWith(".png");
            }
        });
        List<File> fileList = new ArrayList<>(fileArray.length);
        for (File f : fileArray) {
            fileList.add(f);
        }
        Collections.shuffle(fileList);
        return fileList;
    }

}
