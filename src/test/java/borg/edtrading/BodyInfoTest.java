package borg.edtrading;

import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import borg.edtrading.data.Body;
import borg.edtrading.data.Galaxy;
import borg.edtrading.data.ScannedBodyInfo;
import borg.edtrading.data.StarSystem;
import borg.edtrading.ocr.CharacterFinder;
import borg.edtrading.ocr.ScannedBodyInfoParser;
import borg.edtrading.ocr.ScreenshotCropper;
import borg.edtrading.ocr.ScreenshotPreprocessor;
import borg.edtrading.util.ImageUtil;
import borg.edtrading.util.MatchSorter.MatchGroup;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
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
import java.util.Random;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

/**
 * BodyInfoTest
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyInfoTest {

    static final Logger logger = LogManager.getLogger(BodyInfoTest.class);

    public static void main(String[] args) throws IOException {
        FileUtils.cleanDirectory(Constants.TEMP_DIR);
        Galaxy galaxy = Galaxy.readDataFromFiles();
        //testAllImages();

        List<Template> templates = TemplateMatcher.loadTemplates("Body Info");
        List<ScannedBodyInfo> scannedBodyInfos = new ArrayList<>();

        //File sourceFile = selectRandomScreenshot();
        File sourceFile = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR + "\\2016-09-28 07-56-53 Har Itari.png");
        //for (File sourceFile : selectAllScreenshots()) {
        String systemName = BodyInfoApp.systemNameFromFilename(sourceFile);
        StarSystem eddbStarSystem = galaxy.searchStarSystemByExactName(systemName);
        List<Body> eddbBodies = eddbStarSystem == null ? Collections.emptyList() : galaxy.searchBodiesOfStarSystem(eddbStarSystem.getId());
        BufferedImage originalImage = ImageIO.read(sourceFile);
        logger.trace("Testing " + sourceFile.getName() + " (" + originalImage.getWidth() + "x" + originalImage.getHeight() + ", already finished: " + scannedBodyInfos.size() + ")");
        BufferedImage fourKImage = ImageUtil.toFourK(originalImage);
        BufferedImage bodyNameImage = ScreenshotCropper.cropSystemMapToBodyName(fourKImage);
        bodyNameImage = ScreenshotPreprocessor.highlightWhiteText(bodyNameImage);
        BufferedImage bodyInfoImage = ScreenshotCropper.cropSystemMapToBodyInfo(fourKImage);
        bodyInfoImage = ScreenshotPreprocessor.highlightWhiteText(bodyInfoImage);

        //            groupSimilarChars(bodyNameImage, blurredBodyNameImage);
        //            groupSimilarChars(bodyInfoImage, blurredBodyInfoImage);

        List<MatchGroup> bodyNameWords = BodyInfoApp.scanWords(bodyNameImage, templates, sourceFile.getName());
        List<MatchGroup> bodyInfoWords = BodyInfoApp.scanWords(bodyInfoImage, templates, sourceFile.getName());
        //        ScannedBodyInfo scannedBodyInfo = ScannedBodyInfo.fromScannedAndSortedWords(sourceFile.getName(), systemName, bodyNameWords, bodyInfoWords, eddbBodies);
        ScannedBodyInfo scannedBodyInfo = ScannedBodyInfoParser.fromScannedAndSortedWords(sourceFile.getName(), systemName, bodyNameWords, bodyInfoWords, eddbBodies);
        scannedBodyInfos.add(scannedBodyInfo);
        //            List<String> plausiMessages = BodyPlausiChecker.checkPlanet(scannedBodyInfo.getRadiusKm(), scannedBodyInfo.getEarthMasses(), scannedBodyInfo.getGravityG());
        //            for (String msg : plausiMessages) {
        //                logger.warn("!!! " + sourceFile.getName() + " !!! " + msg + " !!!");
        //            }

        //        BufferedImage blurredBodyNameImage = ScreenshotPreprocessor.gaussian(bodyNameImage, 2);
        //        writeDebugImages("Body Name", false, templates, bodyNameImage, blurredBodyNameImage, sourceFile.getName());
        //        ImageIO.write(bodyNameImage, "PNG", new File(Constants.TEMP_DIR, "bodyNameImage.png"));
        //        ImageIO.write(blurredBodyNameImage, "PNG", new File(Constants.TEMP_DIR, "blurredBodyNameImage.png"));

        BufferedImage blurredBodyInfoImage = ScreenshotPreprocessor.gaussian(bodyInfoImage, 2);
        writeDebugImages("Body Info", false, templates, bodyInfoImage, blurredBodyInfoImage, sourceFile.getName());
        ImageIO.write(bodyInfoImage, "PNG", new File(Constants.TEMP_DIR, "bodyInfoImage.png"));
        ImageIO.write(blurredBodyInfoImage, "PNG", new File(Constants.TEMP_DIR, "blurredBodyInfoImage.png"));

        templates = copyLearnedChars(templates);

        if (scannedBodyInfo.getRings() != null) {
            System.out.println(scannedBodyInfo);
        }
        //            BodyInfoApp.printStats(scannedBodyInfos);
        //}
    }

    private static List<Template> copyLearnedChars(List<Template> currentTemplates) throws IOException {
        final Random random = new Random(System.currentTimeMillis());
        List<String> learned = new ArrayList<>();
        File[] subdirs = Constants.AUTO_LEARNED_DIR.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.isDirectory();
            }
        });
        if (subdirs != null) {
            for (File subdir : subdirs) {
                // Learn max 1 per type
                String text = TemplateMatcher.folderToText(subdir.getName());
                String learnFrom = "LEARNED#";
                //                String learnFrom = "GUESSED#"; // By default learn from guessed
                //                if ("0".equals(text) || "O".equals(text) || "I".equals(text) || "l".equals(text)) {
                //                    learnFrom = "LEARNED#"; // Learn difficult chars from fixed names/values
                //                    //learnFrom = "DONOTLEARN#";
                //                }
                final String filePrefix = learnFrom;
                File[] pngFiles = subdir.listFiles(new FileFilter() {
                    @Override
                    public boolean accept(File file) {
                        return file.getName().startsWith(filePrefix) && file.getName().endsWith(".png");
                    }
                });
                if (pngFiles != null && pngFiles.length >= 1) {
                    File randomPngFile = pngFiles[random.nextInt(pngFiles.length)];
                    List<Template> verifyTemplates = currentTemplates.stream().filter(t -> t.getText().equals(text)).collect(Collectors.toList());
                    TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(ImageIO.read(randomPngFile), verifyTemplates, 0, 0, "verify.png");

                    if (bestMatch != null && bestMatch.getErrorPerPixel() >= 1.0) {
                        BigDecimal errpp = BigDecimal.valueOf(bestMatch.getErrorPerPixel()).setScale(1, BigDecimal.ROUND_HALF_UP);
                        logger.warn(randomPngFile.getName() + " is too different from " + text + " with " + errpp + " err/pixel - putting it aside");
                        File targetDir = new File(Constants.TEMPLATES_DIR, "Suspicious\\" + subdir.getName());
                        FileUtils.copyFileToDirectory(randomPngFile, targetDir);
                    } else {
                        File targetDir = new File(Constants.TEMPLATES_DIR, "Body Info\\" + subdir.getName());
                        FileUtils.copyFileToDirectory(randomPngFile, targetDir);
                        learned.add(text);
                    }
                }
            }
            // Clean auto-learned
            FileUtils.cleanDirectory(Constants.AUTO_LEARNED_DIR);
        }

        if (learned.size() > 0) {
            logger.debug("Learned " + learned.size() + " new char(s): " + learned);
        }

        return TemplateMatcher.loadTemplates("Body Info");
    }

    private static void groupSimilarChars(BufferedImage sharpImage, BufferedImage blurredImage, String screenshotFilename) throws IOException {
        List<Rectangle> bodyInfoCharacterLocations = CharacterFinder.findCharacterLocations(sharpImage, sharpImage, false);
        File templatesDir = new File(Constants.TEMPLATES_DIR, "Similar Unknown");
        templatesDir.mkdirs();
        List<Template> templates = TemplateMatcher.loadTemplates("Similar Unknown");
        //FileUtils.cleanDirectory(templatesDir);
        StringBuilder chars = new StringBuilder();
        for (Rectangle r : bodyInfoCharacterLocations) {
            try {
                BufferedImage charImage = blurredImage.getSubimage(r.x, r.y, r.width, r.height);
                TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(charImage, templates, r.x, r.y, screenshotFilename);
                if (bestMatch != null) {
                    if (!bestMatch.getTemplate().getText().startsWith("_")) {
                        chars.append(bestMatch.getTemplate().getText());
                    }
                } else {
                    // Create a new reference dir+image
                    File referenceDir = new File(templatesDir, "_" + System.currentTimeMillis());
                    referenceDir.mkdirs();
                    ImageIO.write(charImage, "PNG", new File(referenceDir, "REFERENCE_" + System.currentTimeMillis() + ".png"));
                    templates = TemplateMatcher.loadTemplates("Similar Unknown");
                }
            } catch (RasterFormatException e) {
                // Too close to border
            }
        }
        logger.info("Grouped " + bodyInfoCharacterLocations.size() + " chars into " + templatesDir.list().length + " folders");
    }

    private static void writeDebugImages(String debugType, boolean writeCharFinderDebugImages, List<Template> templates, BufferedImage sharpImage, BufferedImage blurredImage, String screenshotFilename) throws IOException {
        List<Rectangle> bodyInfoCharacterLocations = CharacterFinder.findCharacterLocations(sharpImage, sharpImage, writeCharFinderDebugImages);
        File unknownDir = new File(Constants.TEMP_DIR, "Unknown " + debugType);
        unknownDir.mkdirs();
        BufferedImage ocrImage = new BufferedImage(blurredImage.getWidth(), blurredImage.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = ocrImage.createGraphics();
        g.drawImage(blurredImage, 0, 0, null);
        g.setColor(new Color(180, 0, 0));
        g.setFont(new Font("Consolas", Font.PLAIN, 22));
        //g.setFont(new Font("Arial", Font.PLAIN, 22));
        for (Rectangle r : bodyInfoCharacterLocations) {
            try {
                BufferedImage charImage = blurredImage.getSubimage(r.x, r.y, r.width, r.height);
                ImageIO.write(charImage, "PNG", new File(unknownDir, String.format("%05d_%05d_%d.png", (r.y / 10) * 10, r.x, r.hashCode())));
                TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(charImage, templates, r.x, r.y, screenshotFilename);
                if (bestMatch != null) {
                    g.drawString(bestMatch.getTemplate().getText(), r.x, r.y + 18);
                    System.out.print(bestMatch.getTemplate().getText());
                } else {
                    System.out.print("▪");
                }
            } catch (RasterFormatException e) {
                // Too close to border
            }
        }
        System.out.println();
        ImageIO.write(ocrImage, "PNG", new File(Constants.TEMP_DIR, debugType + " OCR.png"));
    }

    private static File selectRandomScreenshot() {
        File dir = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR);
        File[] files = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().endsWith(".png");
            }
        });
        Random rand = new Random();
        return files[rand.nextInt(files.length)];
    }

    private static File[] selectAllScreenshots() {
        File dir = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR);
        File[] files = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().endsWith(".png");
            }
        });
        final Random random = new Random(System.currentTimeMillis());
        Arrays.sort(files, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                return random.nextDouble() < 0.5 ? -1 : 1;
            }
        });
        return files;
    }

    private static void testAllImages() throws IOException {
        List<Template> templates = TemplateMatcher.loadTemplates("Body Name");
        File dir = new File(Constants.SURFACE_MATS_DIR, Constants.SURFACE_MATS_SUBDIR);
        File[] files = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().endsWith(".png");
            }
        });
        int i = 0;
        for (File file : files) {
            logger.trace(++i + " of " + files.length);
            BufferedImage originalImage = ImageIO.read(file);
            BufferedImage whiteText = ScreenshotPreprocessor.highlightWhiteText(originalImage);
            BufferedImage fourK = ImageUtil.toFourK(whiteText);
            //ImageIO.write(fourK, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_fourK.png")));
            BufferedImage bodyName = ScreenshotCropper.cropSystemMapToBodyName(fourK);
            ImageIO.write(bodyName, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_bodyName.png")));
            BufferedImage threshold = ScreenshotPreprocessor.cannyEdge(bodyName);
            ImageIO.write(threshold, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_threshold.png")));
            //writeDebugImages("Body Name", true, templates, bodyName, bodyName);
        }
        throw new RuntimeException();
    }

}
