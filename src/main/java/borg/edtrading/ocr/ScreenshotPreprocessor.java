package borg.edtrading.ocr;

import boofcv.alg.filter.binary.GThresholdImageOps;
import boofcv.alg.filter.blur.BlurImageOps;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.gui.image.VisualizeImageData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;

/**
 * Preprocesses screenshots to make OCR easier.
 *
 * @see #localSquareThreshold(GrayF32, int, double, boolean)
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public abstract class ScreenshotPreprocessor {

    static final Logger logger = LogManager.getLogger(ScreenshotPreprocessor.class);

    public static BufferedImage gaussian(BufferedImage originalImage) {
        GrayU8 gray = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
        GrayU8 gaussian = BlurImageOps.gaussian(gray, null, /* sigma = */ -1, /* radius = */ 2, null);
        return VisualizeImageData.grayMagnitude(gaussian, null, -1);
    }

    /**
     * Tuned for the white text on mostly black background of system map screenshots.
     */
    public static BufferedImage localSquareThresholdForSystemMap(BufferedImage originalImage) {
        GrayF32 grayImage = ConvertBufferedImage.convertFromSingle(originalImage, null, GrayF32.class);
        GrayU8 thresholdedImage = localSquareThresholdForSystemMap(grayImage);
        return VisualizeBinaryData.renderBinary(thresholdedImage, false, null);
    }

    public static GrayU8 localSquareThresholdForSystemMap(GrayF32 grayImage) {
        return localSquareThreshold(grayImage, 160, 0.7, false);
    }

    /**
     * Useful for keeping text only.
     *
     * @see #localSquareThresholdForSystemMap(BufferedImage)
     */
    public static GrayU8 localSquareThreshold(GrayF32 grayImage, int radius, double scale, boolean down) {
        return GThresholdImageOps.localSquare(grayImage, null, radius, scale, down, null, null);
    }

}
