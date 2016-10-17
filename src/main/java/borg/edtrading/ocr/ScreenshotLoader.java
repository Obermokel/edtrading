package borg.edtrading.ocr;

import boofcv.abst.distort.FDistort;
import boofcv.alg.interpolate.TypeInterpolate;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.Planar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

/**
 * ScreenshotLoader
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScreenshotLoader {

    static final Logger logger = LogManager.getLogger(ScreenshotLoader.class);

    private static final float ASPECT_RATIO_16_BY_9 = 16.0f / 9.0f;

    public Screenshot loadSystemMapScreenshot(File screenshotFile, Screenshot previosScreenshot) throws IOException {
        // Load as BufferedImage
        BufferedImage bi = ImageIO.read(screenshotFile);

        // If we have a previous screenshot of the same format we can re-use memory
        boolean sameFormatAsPrevious = false;
        if (previosScreenshot != null) {
            Planar<GrayU8> p = previosScreenshot.getOriginalImage();
            sameFormatAsPrevious = bi.getWidth() == p.getWidth() && bi.getHeight() == p.getHeight() && bi.getRaster().getNumBands() == p.getNumBands();
        }

        // Convert into planar (A)RGB
        Planar<GrayU8> originalImage = sameFormatAsPrevious ? previosScreenshot.getOriginalImage() : new Planar<>(GrayU8.class, bi.getWidth(), bi.getHeight(), bi.getRaster().getNumBands());
        ConvertBufferedImage.convertFromMulti(bi, originalImage, true, GrayU8.class);

        // Resize to 4K and discard alpha channel
        Planar<GrayU8> originalRGB = originalImage.getNumBands() == 4 ? originalImage.partialSpectrum(1, 2, 3) : originalImage;
        Planar<GrayU8> resizedRGB = sameFormatAsPrevious ? previosScreenshot.getResizedImage() : new Planar<>(GrayU8.class, 3840, 2160, 3);
        for (int band = 0; band < 3; band++) {
            GrayU8 input = cropTo16by9(originalRGB.getBand(band));
            GrayU8 output = resizedRGB.getBand(band);
            new FDistort().input(input).output(output).interp(TypeInterpolate.BICUBIC).scale().apply();
        }

        return new Screenshot(screenshotFile, originalImage, resizedRGB);
    }

    private static GrayU8 cropTo16by9(GrayU8 original) {
        float aspectRatio = (float) original.getWidth() / (float) original.getHeight();

        if (aspectRatio > ASPECT_RATIO_16_BY_9) {
            // Crop width
            int targetWidth = Math.round(original.getHeight() * ASPECT_RATIO_16_BY_9);
            return cropTo(original, targetWidth, original.getHeight());
        } else if (aspectRatio < ASPECT_RATIO_16_BY_9) {
            // Crop height
            int targetHeight = Math.round(original.getWidth() / ASPECT_RATIO_16_BY_9);
            return cropTo(original, original.getWidth(), targetHeight);
        } else {
            // Perfect!
            return original;
        }
    }

    private static GrayU8 cropTo(GrayU8 original, int targetWidth, int targetHeight) {
        if (original.getWidth() > targetWidth && original.getHeight() > targetHeight) {
            int x = (original.getWidth() - targetWidth) / 2;
            int y = (original.getHeight() - targetHeight) / 2;
            return original.subimage(x, y, x + targetWidth, y + targetHeight);
        } else if (original.getWidth() > targetWidth) {
            int x = (original.getWidth() - targetWidth) / 2;
            int y = 0;
            return original.subimage(x, y, x + targetWidth, y + targetHeight);
        } else if (original.getHeight() > targetHeight) {
            int x = 0;
            int y = (original.getHeight() - targetHeight) / 2;
            return original.subimage(x, y, x + targetWidth, y + targetHeight);
        } else {
            return original;
        }
    }

}
