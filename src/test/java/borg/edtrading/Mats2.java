package borg.edtrading;

import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import borg.edtrading.data.ScannedBodyInfo;
import borg.edtrading.ocr.CharacterFinder;
import borg.edtrading.ocr.ScreenshotCropper;
import borg.edtrading.ocr.ScreenshotPreprocessor;
import borg.edtrading.util.ImageUtil;
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
import java.util.List;
import java.util.Random;

import javax.imageio.ImageIO;

/**
 * Mats2
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Mats2 {

    static final Logger logger = LogManager.getLogger(Mats2.class);

    public static void main(String[] args) throws IOException {
        FileUtils.cleanDirectory(Constants.TEMP_DIR);
        //testAllImages();

        List<Template> templates = TemplateMatcher.loadTemplates("Body Info");

        File sourceFile = selectRandomScreenshot();
        //File sourceFile = new File(Constants.SURFACE_MATS_DIR, "_ALL_\\2016-09-11 22-27-48 Ch'i Lingo.png");
        //        for (File sourceFile : selectAllScreenshots()) {

        String systemName = BodyInfoApp.systemNameFromFilename(sourceFile);
        BufferedImage originalImage = ImageIO.read(sourceFile);
        BufferedImage fourKImage = ImageUtil.toFourK(originalImage);
        BufferedImage bodyNameImage = ScreenshotCropper.cropSystemMapToBodyName(fourKImage);
        bodyNameImage = ScreenshotPreprocessor.highlightWhiteText(bodyNameImage);
        BufferedImage blurredBodyNameImage = ScreenshotPreprocessor.gaussian(bodyNameImage, 2);
        //ImageIO.write(blurredBodyNameImage, "PNG", new File(Constants.TEMP_DIR, "blurredBodyNameImage.png"));
        BufferedImage bodyInfoImage = ScreenshotCropper.cropSystemMapToBodyInfo(fourKImage);
        bodyInfoImage = ScreenshotPreprocessor.highlightWhiteText(bodyInfoImage);
        BufferedImage blurredBodyInfoImage = ScreenshotPreprocessor.gaussian(bodyInfoImage, 2);
        //ImageIO.write(blurredBodyInfoImage, "PNG", new File(Constants.TEMP_DIR, "blurredBodyInfoImage.png"));

        //            groupSimilarChars(bodyNameImage, blurredBodyNameImage);
        //            groupSimilarChars(bodyInfoImage, blurredBodyInfoImage);

        List<String> bodyNameWords = BodyInfoApp.scanWords(bodyNameImage, templates);
        List<String> bodyInfoWords = BodyInfoApp.scanWords(bodyInfoImage, templates);
        ScannedBodyInfo scannedBodyInfo = ScannedBodyInfo.fromScannedAndSortedWords(sourceFile.getName(), systemName, bodyNameWords, bodyInfoWords);
        System.out.println(scannedBodyInfo);

        writeDebugImages("Body Name", false, templates, bodyNameImage, blurredBodyNameImage);
        writeDebugImages("Body Info", false, templates, bodyInfoImage, blurredBodyInfoImage);
        //        }
    }

    private static void groupSimilarChars(BufferedImage sharpImage, BufferedImage blurredImage) throws IOException {
        List<Rectangle> bodyInfoCharacterLocations = CharacterFinder.findCharacterLocations(sharpImage, false);
        File templatesDir = new File(Constants.TEMPLATES_DIR, "Similar Unknown");
        templatesDir.mkdirs();
        List<Template> templates = TemplateMatcher.loadTemplates("Similar Unknown");
        //FileUtils.cleanDirectory(templatesDir);
        StringBuilder chars = new StringBuilder();
        for (Rectangle r : bodyInfoCharacterLocations) {
            try {
                BufferedImage charImage = blurredImage.getSubimage(r.x, r.y, r.width, r.height);
                TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(charImage, templates, r.x, r.y, 1000);
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

    private static void writeDebugImages(String debugType, boolean writeCharFinderDebugImages, List<Template> templates, BufferedImage sharpImage, BufferedImage blurredImage) throws IOException {
        List<Rectangle> bodyInfoCharacterLocations = CharacterFinder.findCharacterLocations(sharpImage, writeCharFinderDebugImages);
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
                TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(charImage, templates, r.x, r.y, 1234);
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
        File dir = new File(Constants.SURFACE_MATS_DIR, "_ALL_");
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
        File dir = new File(Constants.SURFACE_MATS_DIR, "_ALL_");
        return dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().endsWith(".png");
            }
        });
    }

    private static void testAllImages() throws IOException {
        List<Template> templates = TemplateMatcher.loadTemplates("Body Name");
        File dir = new File(Constants.SURFACE_MATS_DIR, "_ALL_");
        File[] files = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().endsWith(".png");
            }
        });
        int i = 0;
        for (File file : files) {
            System.out.println(++i + " of " + files.length);
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
