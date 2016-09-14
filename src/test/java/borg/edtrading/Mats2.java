package borg.edtrading;

import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
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

        //File sourceFile = selectRandomScreenshot();
        File sourceFile = new File(Constants.SURFACE_MATS_DIR, "_ALL_\\2016-08-31 21-28-56 LP 298-52.png");

        String systemName = BodyInfoApp.systemNameFromFilename(sourceFile);
        List<Template> infoTemplates = TemplateMatcher.loadTemplates("Body Info");
        List<Template> nameTemplates = TemplateMatcher.loadTemplates("Body Name");
        BufferedImage originalImage = ImageIO.read(sourceFile);
        BufferedImage darkenedImage = ScreenshotPreprocessor.darkenSaturatedAreas(originalImage);
        BufferedImage fourKImage = ImageUtil.toFourK(darkenedImage);
        //ImageIO.write(fourKImage, "PNG", new File(Constants.TEMP_DIR, "fourKImage.png"));
        BufferedImage bodyInfoImage = ScreenshotCropper.cropSystemMapToBodyInfo(fourKImage);
        BufferedImage thresholdedBodyInfoImage = ScreenshotPreprocessor.localSquareThresholdForSystemMap(bodyInfoImage);
        //ImageIO.write(thresholdedBodyInfoImage, "PNG", new File(Constants.TEMP_DIR, "thresholdedBodyInfoImage.png"));
        BufferedImage blurredBodyInfoImage = ScreenshotPreprocessor.gaussian(thresholdedBodyInfoImage, 2);
        ImageIO.write(blurredBodyInfoImage, "PNG", new File(Constants.TEMP_DIR, "blurredBodyInfoImage.png"));
        BufferedImage bodyNameImage = ScreenshotCropper.cropSystemMapToBodyName(fourKImage);
        BufferedImage thresholdedBodyNameImage = ScreenshotPreprocessor.localSquareThresholdForSystemMap(bodyNameImage);
        //ImageIO.write(thresholdedBodyNameImage, "PNG", new File(Constants.TEMP_DIR, "thresholdedBodyNameImage.png"));
        BufferedImage blurredBodyNameImage = ScreenshotPreprocessor.gaussian(thresholdedBodyNameImage, 2);
        ImageIO.write(blurredBodyNameImage, "PNG", new File(Constants.TEMP_DIR, "blurredBodyNameImage.png"));

        List<String> bodyNameWords = BodyInfoApp.scanWords(bodyNameImage, nameTemplates);
        List<String> bodyInfoWords = BodyInfoApp.scanWords(bodyInfoImage, infoTemplates);
        ScannedBodyInfo scannedBodyInfo = ScannedBodyInfo.fromScannedAndSortedWords(sourceFile.getName(), systemName, bodyNameWords, bodyInfoWords);
        System.out.println(scannedBodyInfo);

        writeDebugImages("Body Name", false, nameTemplates, thresholdedBodyNameImage, blurredBodyNameImage);
        writeDebugImages("Body Info", false, infoTemplates, thresholdedBodyInfoImage, blurredBodyInfoImage);
    }

    private static void writeDebugImages(String debugType, boolean debugImages, List<Template> templates, BufferedImage thresholdedImage, BufferedImage blurredImage) throws IOException {
        List<Rectangle> bodyInfoCharacterLocations = CharacterFinder.findCharacterLocations(thresholdedImage, debugImages);
        File unknownDir = new File(Constants.TEMP_DIR, "Unknown " + debugType);
        unknownDir.mkdirs();
        BufferedImage ocrImage = new BufferedImage(blurredImage.getWidth(), blurredImage.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = ocrImage.createGraphics();
        g.drawImage(blurredImage, 0, 0, null);
        g.setColor(Color.GREEN);
        g.setFont(new Font("Consolas", Font.PLAIN, 18));
        for (Rectangle r : bodyInfoCharacterLocations) {
            try {
                BufferedImage charImage = blurredImage.getSubimage(r.x, r.y, r.width, r.height);
                ImageIO.write(charImage, "PNG", new File(unknownDir, String.format("%05d_%05d_%d.png", (r.y / 10) * 10, r.x, r.hashCode())));
                TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(charImage, templates, r.x, r.y);
                if (bestMatch != null) {
                    g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
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

    private static void testAllImages() throws IOException {
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
            BufferedImage darkened = ScreenshotPreprocessor.darkenSaturatedAreas(originalImage);
            BufferedImage fourK = ImageUtil.toFourK(darkened);
            BufferedImage planetMaterialsImage = ScreenshotCropper.cropSystemMapToBodyInfo(fourK);
            GrayF32 grayImage = ConvertBufferedImage.convertFromSingle(planetMaterialsImage, null, GrayF32.class);
            //            for (int radius = 10; radius <= 160; radius += 10) {
            //                for (double scale = 0.5; scale <= 0.5; scale += 0.1) {
            double scale = 0.5;
            //2016-09-01 22-47-22 Hruntia
            GrayU8 t20 = ScreenshotPreprocessor.localSquareThreshold(grayImage, 20, scale, false);
            GrayU8 t80 = ScreenshotPreprocessor.localSquareThreshold(grayImage, 540, scale, false);
            GrayU8 t160 = ScreenshotPreprocessor.localSquareThreshold(grayImage, 160, scale, false);
            GrayU8 tAll = t20.createSameShape();
            for (int y = 0; y < t20.height; y++) {
                for (int x = 0; x < t20.width; x++) {
                    if (t20.unsafe_get(x, y) > 0 && t80.unsafe_get(x, y) > 0 && t160.unsafe_get(x, y) > 0) {
                        tAll.unsafe_set(x, y, t20.unsafe_get(x, y));
                    } else {
                        tAll.unsafe_set(x, y, 0);
                    }
                }
            }
            BufferedImage thresholdedImage = VisualizeBinaryData.renderBinary(tAll, false, null);
            ImageIO.write(thresholdedImage, "PNG", new File(Constants.TEMP_DIR, file.getName().replace(".png", "_rAll_s" + scale + ".png")));
            //                }
            //            }
        }
        throw new RuntimeException();
    }

}
