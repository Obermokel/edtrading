package borg.edtrading;

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
import java.io.IOException;
import java.util.List;

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

        File sourceFile = new File(Constants.SURFACE_MATS_DIR, "_ALL_\\2016-08-31 21-40-10 Altair.png");
        BufferedImage originalImage = ImageIO.read(sourceFile);
        BufferedImage fourK = ImageUtil.toQuadFourK(originalImage);
        //ImageIO.write(fourK, "PNG", new File(Constants.TEMP_DIR, "fourK.png"));
        BufferedImage planetMaterialsImage = ScreenshotCropper.cropSystemMapToBodyInfo(fourK);
        BufferedImage thresholdedImage = ScreenshotPreprocessor.localSquareThresholdForSystemMap(planetMaterialsImage);
        ImageIO.write(thresholdedImage, "PNG", new File(Constants.TEMP_DIR, "thresholdedImage.png"));
        List<Rectangle> characterLocations = CharacterFinder.findCharacterLocations(thresholdedImage, true);
        BufferedImage blurredImage = ScreenshotPreprocessor.gaussian(thresholdedImage);
        ImageIO.write(blurredImage, "PNG", new File(Constants.TEMP_DIR, "blurredImage.png"));

        // TODO Extract from here!
        File unknownDir = new File(Constants.TEMP_DIR, "unknown");
        unknownDir.mkdirs();
        BufferedImage ocrImage = new BufferedImage(blurredImage.getWidth(), blurredImage.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = ocrImage.createGraphics();
        g.drawImage(blurredImage, 0, 0, null);
        g.setColor(Color.RED);
        g.setFont(new Font("Arial", Font.PLAIN, 14));
        List<Template> templates = TemplateMatcher.loadTemplates("Body Info");
        for (Rectangle r : characterLocations) {
            try {
                BufferedImage surroundedCharImage = blurredImage.getSubimage(r.x - r.width / 2, r.y - r.height / 2, r.width + r.width, r.height + r.height);
                surroundedCharImage = ImageUtil.scaleTo(surroundedCharImage, surroundedCharImage.getWidth() / 2, surroundedCharImage.getHeight() / 2);
                TemplateMatch bestMatch = TemplateMatcher.findBestTemplateMatch(surroundedCharImage, templates);
                if (bestMatch == null) {
                    BufferedImage rawCharImage = blurredImage.getSubimage(r.x, r.y, r.width, r.height);
                    rawCharImage = ImageUtil.scaleTo(rawCharImage, rawCharImage.getWidth() / 2, rawCharImage.getHeight() / 2);
                    ImageIO.write(rawCharImage, "PNG", new File(unknownDir, String.format("%05d_%05d_%d.png", (r.y / 10) * 10, r.x, r.hashCode())));
                } else {
                    g.drawString(bestMatch.getTemplate().getText(), r.x, r.y);
                    System.out.print(bestMatch.getTemplate().getText());
                }
            } catch (RasterFormatException e) {
                // Too close to border
            }
        }
        ImageIO.write(ocrImage, "PNG", new File(Constants.TEMP_DIR, "OCR.png"));
    }

}
