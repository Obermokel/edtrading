package borg.edtrading.ocr;

import boofcv.alg.color.ColorHsv;
import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.alg.filter.binary.GThresholdImageOps;
import boofcv.alg.filter.blur.BlurImageOps;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.gui.image.VisualizeImageData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayS16;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.Planar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
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

    public static BufferedImage cannyEdge(BufferedImage originalImage) {
        GrayU8 gray = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
        GrayU8 edge = gray.createSameShape();
        CannyEdge<GrayU8, GrayS16> canny = FactoryEdgeDetectors.canny(3, true, true, GrayU8.class, GrayS16.class);
        canny.process(gray, 0.1f, 0.3f, edge);
        //return VisualizeBinaryData.renderBinary(edge, false, null);
        return VisualizeImageData.grayMagnitude(edge, null, -1);
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

    public static BufferedImage highlightWhiteText(BufferedImage image) {
        BufferedImage whiteTextImage = keepWhiteTextOnly(image);
        BufferedImage cannyImage = cannyEdge(whiteTextImage);

        return outlineText(whiteTextImage, cannyImage);
    }

    private static BufferedImage keepWhiteTextOnly(BufferedImage image) {
        //float hue = 0f; // angle in radians
        float saturation = 0f; // 0..1
        float value = 255f; // 0..255

        Planar<GrayF32> input = ConvertBufferedImage.convertFromMulti(image, null, true, GrayF32.class);
        Planar<GrayF32> hsv = input.createSameShape();
        ColorHsv.rgbToHsv_F32(input, hsv);
        //GrayF32 H = hsv.getBand(0);
        GrayF32 S = hsv.getBand(1);
        GrayF32 V = hsv.getBand(2);

        BufferedImage output = new BufferedImage(input.width, input.height, BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < hsv.height; y++) {
            for (int x = 0; x < hsv.width; x++) {
                float ds = Math.abs(S.unsafe_get(x, y) - saturation);
                float dv = Math.abs(V.unsafe_get(x, y) - value) / 255f;

                if (ds <= 0.25f && dv <= 0.35f) {
                    output.setRGB(x, y, image.getRGB(x, y));
                } else {
                    output.setRGB(x, y, Color.BLACK.getRGB());
                }
            }
        }

        return output;
    }

    private static BufferedImage outlineText(BufferedImage whiteTextImage, BufferedImage cannyImage) {
        final int outlineColor = new Color(64, 64, 64).getRGB();

        BufferedImage output = new BufferedImage(whiteTextImage.getWidth(), whiteTextImage.getHeight(), BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < whiteTextImage.getHeight(); y++) {
            for (int x = 0; x < whiteTextImage.getWidth(); x++) {
                int rgb = Color.BLACK.getRGB();
                if (cannyImage.getRGB(x, y) != Color.BLACK.getRGB()) {
                    rgb = outlineColor;
                }
                if (whiteTextImage.getRGB(x, y) != Color.BLACK.getRGB()) {
                    rgb = whiteTextImage.getRGB(x, y);
                }
                output.setRGB(x, y, rgb);
            }
        }
        return output;
    }

}
