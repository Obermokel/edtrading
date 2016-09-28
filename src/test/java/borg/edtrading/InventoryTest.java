package borg.edtrading;

import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import borg.edtrading.ocr.CharacterFinder;
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
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Random;

import javax.imageio.ImageIO;

/**
 * InventoryTest
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryTest {

    static final Logger logger = LogManager.getLogger(InventoryTest.class);

    public static void main(String[] args) throws IOException {
        FileUtils.cleanDirectory(Constants.TEMP_DIR);
        testAllImages();

        List<Template> templates = TemplateMatcher.loadTemplates("Inventory");

        //File sourceFile = selectRandomScreenshot();
        //File sourceFile = new File(Constants.INVENTORY_SCREENSHOTS_DIR, "2016-09-28 07-43-01 Sol.png");
        for (File sourceFile : selectAllScreenshots()) {
            BufferedImage originalImage = ImageIO.read(sourceFile);
            logger.trace("Testing " + sourceFile.getName() + " (" + originalImage.getWidth() + "x" + originalImage.getHeight() + ")");
            BufferedImage fourKImage = ImageUtil.toFourK(originalImage);
            BufferedImage inventoryImage = ScreenshotCropper.cropToInventory(fourKImage);
            //ImageIO.write(inventoryImage, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_inventoryImage.png")));
            //BufferedImage cannyImage = ScreenshotPreprocessor.cannyEdge(inventoryImage);
            //ImageIO.write(cannyImage, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_cannyImage.png")));
            //BufferedImage blurredImage = ScreenshotPreprocessor.gaussian(cannyImage, 2);
            //ImageIO.write(blurredImage, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_blurredImage.png")));
            List<MatchGroup> matchGroups = InventoryApp.scanWords(inventoryImage, templates, sourceFile.getName());
            // TODO parse...
            for (MatchGroup mg : matchGroups) {
                for (TemplateMatch m : mg.getGroupMatches()) {
                    System.out.print(m.getTemplate().getText());
                }
                System.out.print(" ");
            }
            System.out.println();

            //groupSimilarChars(inventoryImage, blurredImage, sourceFile.getName());

            //        writeDebugImages("Inventory", false, templates, bodyNameImage, blurredBodyNameImage, sourceFile.getName());

            //            templates = copyLearnedChars();
        }
    }

    private static List<Template> copyLearnedChars() throws IOException {
        final Random random = new Random(System.currentTimeMillis());
        File[] subdirs = Constants.AUTO_LEARNED_DIR.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.isDirectory();
            }
        });
        if (subdirs != null) {
            for (File subdir : subdirs) {
                // Learn max 1 per type
                File[] pngFiles = subdir.listFiles(new FileFilter() {
                    @Override
                    public boolean accept(File file) {
                        return file.getName().endsWith(".png");
                    }
                });
                if (pngFiles != null && pngFiles.length >= 1) {
                    File randomPngFile = pngFiles[random.nextInt(pngFiles.length)];
                    File targetDir = new File(Constants.TEMPLATES_DIR, "Inventory\\" + subdir.getName());
                    FileUtils.copyFileToDirectory(randomPngFile, targetDir);
                }
            }
            // Clean auto-learned
            FileUtils.cleanDirectory(Constants.AUTO_LEARNED_DIR);
        }

        return TemplateMatcher.loadTemplates("Inventory");
    }

    private static void groupSimilarChars(BufferedImage cannySuitableImage, BufferedImage blurredImage, String screenshotFilename) throws IOException {
        BufferedImage thresholdedWhiteCharsOnBlackBackgroundImage = ScreenshotPreprocessor.cannyEdge(cannySuitableImage);
        List<Rectangle> charLocations = CharacterFinder.findCharacterLocations(cannySuitableImage, thresholdedWhiteCharsOnBlackBackgroundImage, false);
        logger.fatal("Found " + charLocations.size());
        File templatesDir = new File(Constants.TEMPLATES_DIR, "Similar Unknown");
        templatesDir.mkdirs();
        List<Template> templates = TemplateMatcher.loadTemplates("Similar Unknown");
        //FileUtils.cleanDirectory(templatesDir);
        StringBuilder chars = new StringBuilder();
        for (Rectangle r : charLocations) {
            try {
                BufferedImage charImage = blurredImage.getSubimage(r.x, r.y, r.width, r.height);
                TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(charImage, templates, r.x, r.y, screenshotFilename);
                if (bestMatch != null) {
                    if (!bestMatch.getTemplate().getText().startsWith("__")) {
                        chars.append(bestMatch.getTemplate().getText());
                    }
                } else {
                    // Create a new reference dir+image
                    File referenceDir = new File(templatesDir, "__" + System.currentTimeMillis());
                    referenceDir.mkdirs();
                    ImageIO.write(charImage, "PNG", new File(referenceDir, "REFERENCE_" + System.currentTimeMillis() + ".png"));
                    templates = TemplateMatcher.loadTemplates("Similar Unknown");
                }
            } catch (RasterFormatException e) {
                // Too close to border
            }
        }
        logger.info("Grouped " + charLocations.size() + " chars into " + templatesDir.list().length + " folders");
    }

    private static void writeDebugImages(String debugType, boolean writeCharFinderDebugImages, List<Template> templates, BufferedImage cannySuitableImage, BufferedImage blurredImage, String screenshotFilename) throws IOException {
        BufferedImage thresholdedWhiteCharsOnBlackBackgroundImage = ScreenshotPreprocessor.cannyEdge(cannySuitableImage);
        List<Rectangle> charLocations = CharacterFinder.findCharacterLocations(cannySuitableImage, thresholdedWhiteCharsOnBlackBackgroundImage, writeCharFinderDebugImages);
        File unknownDir = new File(Constants.TEMP_DIR, "Unknown " + debugType);
        unknownDir.mkdirs();
        BufferedImage ocrImage = new BufferedImage(blurredImage.getWidth(), blurredImage.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = ocrImage.createGraphics();
        g.drawImage(blurredImage, 0, 0, null);
        g.setColor(new Color(180, 0, 0));
        g.setFont(new Font("Consolas", Font.PLAIN, 22));
        //g.setFont(new Font("Arial", Font.PLAIN, 22));
        for (Rectangle r : charLocations) {
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
        File dir = Constants.INVENTORY_SCREENSHOTS_DIR;
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
        File dir = Constants.INVENTORY_SCREENSHOTS_DIR;
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
        List<Template> templates = TemplateMatcher.loadTemplates("Inventory");
        File dir = Constants.INVENTORY_SCREENSHOTS_DIR;
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
            BufferedImage fourKImage = ImageUtil.toFourK(originalImage);
            BufferedImage inventoryImage = ScreenshotCropper.cropToInventory(fourKImage);
            ImageIO.write(inventoryImage, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_inventoryImage.png")));
            //            BufferedImage cannyImage = ScreenshotPreprocessor.cannyEdge(inventoryImage);
            //            ImageIO.write(cannyImage, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_cannyImage.png")));
            //            BufferedImage blurredImage = ScreenshotPreprocessor.gaussian(cannyImage, 2);
            //            ImageIO.write(blurredImage, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_blurredImage.png")));
            BufferedImage textImage = ScreenshotPreprocessor.keepInventoryTextOnly(inventoryImage);
            ImageIO.write(textImage, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_textImage.png")));
            List<MatchGroup> matchGroups = InventoryApp.scanWords(textImage, templates, file.getName());
            for (MatchGroup mg : matchGroups) {
                for (TemplateMatch m : mg.getGroupMatches()) {
                    System.out.print(m.getTemplate().getText());
                }
            }
            System.out.println();
        }
        throw new RuntimeException();
    }

}
