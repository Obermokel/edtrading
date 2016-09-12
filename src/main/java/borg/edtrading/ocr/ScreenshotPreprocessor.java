package borg.edtrading.ocr;

import boofcv.alg.color.ColorHsv;
import boofcv.alg.filter.binary.GThresholdImageOps;
import boofcv.alg.filter.blur.BlurImageOps;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.gui.image.VisualizeImageData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.Planar;
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

    public static BufferedImage darkenSaturatedAreas(BufferedImage originalImage) {
        Planar<GrayF32> rgb = ConvertBufferedImage.convertFromMulti(originalImage, null, true, GrayF32.class);
        Planar<GrayF32> hsv = rgb.createSameShape();

        ColorHsv.rgbToHsv_F32(rgb, hsv);
        GrayF32 S = hsv.getBand(1);
        GrayF32 V = hsv.getBand(2);
        for (int y = 0; y < hsv.height; y++) {
            for (int x = 0; x < hsv.width; x++) {
                float s = S.unsafe_get(x, y);
                if (s > 0.05f) {
                    S.unsafe_set(x, y, 0.05f);
                    float v = V.unsafe_get(x, y);
                    V.unsafe_set(x, y, v / 5.0f);
                }
            }
        }
        ColorHsv.hsvToRgb_F32(hsv, rgb);

        return ConvertBufferedImage.convertTo_F32(rgb, null, true);
    }

    public static BufferedImage gaussian(BufferedImage originalImage, int radius) {
        GrayU8 gray = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
        GrayU8 gaussian = BlurImageOps.gaussian(gray, null, /* sigma = */ -1, radius, null);
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
        GrayU8 t20 = ScreenshotPreprocessor.localSquareThreshold(grayImage, 20, 0.5, false);
        GrayU8 t80 = ScreenshotPreprocessor.localSquareThreshold(grayImage, 540, 0.5, false);
        GrayU8 t160 = ScreenshotPreprocessor.localSquareThreshold(grayImage, 160, 0.5, false);
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
        return tAll;
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
