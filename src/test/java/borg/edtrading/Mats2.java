package borg.edtrading;

import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
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

        File sourceFile = selectRandomScreenshot();
        BufferedImage originalImage = ImageIO.read(sourceFile);
        BufferedImage darkened = ScreenshotPreprocessor.darkenSaturatedAreas(originalImage);
        BufferedImage fourK = ImageUtil.toFourK(darkened);
        ImageIO.write(fourK, "PNG", new File(Constants.TEMP_DIR, "fourK.png"));
        BufferedImage planetMaterialsImage = ScreenshotCropper.cropSystemMapToBodyInfo(fourK);
        BufferedImage thresholdedImage = ScreenshotPreprocessor.localSquareThresholdForSystemMap(planetMaterialsImage);
        ImageIO.write(thresholdedImage, "PNG", new File(Constants.TEMP_DIR, "thresholdedImage.png"));
        List<Rectangle> characterLocations = CharacterFinder.findCharacterLocations(thresholdedImage, true);
        BufferedImage blurredImage = ScreenshotPreprocessor.gaussian(thresholdedImage, 2);
        ImageIO.write(blurredImage, "PNG", new File(Constants.TEMP_DIR, "blurredImage.png"));

        // TODO Extract from here!
        File unknownDir = new File(Constants.TEMP_DIR, "unknown");
        unknownDir.mkdirs();
        BufferedImage ocrImage = new BufferedImage(blurredImage.getWidth(), blurredImage.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = ocrImage.createGraphics();
        g.drawImage(blurredImage, 0, 0, null);
        g.setColor(Color.GREEN);
        g.setFont(new Font("Consolas", Font.BOLD, 20));
        List<Template> templates = TemplateMatcher.loadTemplates("Body Info");
        for (Rectangle r : characterLocations) {
            try {
                //BufferedImage surroundedCharImage = blurredImage.getSubimage(r.x - r.width / 2, r.y - r.height / 2, r.width + r.width, r.height + r.height);
                BufferedImage surroundedCharImage = blurredImage.getSubimage(r.x, r.y, r.width, r.height);
                ImageIO.write(surroundedCharImage, "PNG", new File(unknownDir, String.format("%05d_%05d_%d.png", (r.y / 10) * 10, r.x, r.hashCode())));
                //surroundedCharImage = ImageUtil.scaleTo(surroundedCharImage, surroundedCharImage.getWidth() / 2, surroundedCharImage.getHeight() / 2);
                TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(surroundedCharImage, templates);
                if (bestMatch != null) {
                    g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
                    System.out.println(bestMatch.getTemplate().getText());
                }
            } catch (RasterFormatException e) {
                // Too close to border
            }
        }
        ImageIO.write(ocrImage, "PNG", new File(Constants.TEMP_DIR, "OCR.png"));
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
