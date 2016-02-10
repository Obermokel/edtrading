package borg.edtrading.boofcv;

import java.awt.image.BufferedImage;

import boofcv.alg.color.ColorHsv;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.ImageFloat32;
import boofcv.struct.image.MultiSpectral;
import georegression.metric.UtilAngle;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * ScreenshotSimplifier
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScreenshotSimplifier {

    static final Logger logger = LogManager.getLogger(ScreenshotSimplifier.class);

    public static BufferedImage simplifyScreenshot(BufferedImage image) {
        BufferedImage cropped = crop3440x1440(image);
        BufferedImage simplified = keepOrangeTextOnly(cropped);
        return simplified;
    }

    /**
     * Only useful for the commodity screen
     */
    private static BufferedImage crop3440x1440(BufferedImage image) {
        return image.getSubimage(540, 0, 1620, 1440);
    }

    private static BufferedImage keepOrangeTextOnly(BufferedImage image) {
        return keepTextOnly(image, (float) Math.toRadians(25), 0.91f, 252f);
    }

    private static BufferedImage keepTextOnly(BufferedImage image, float hue, float saturation, float value) {
        MultiSpectral<ImageFloat32> input = ConvertBufferedImage.convertFromMulti(image, null, true, ImageFloat32.class);
        MultiSpectral<ImageFloat32> hsv = input.createSameShape();

        // Convert into HSV
        ColorHsv.rgbToHsv_F32(input, hsv);

        // Euclidean distance squared threshold for deciding which pixels are members of the selected set
        float maxDist2 = 0.5f * 0.5f;

        // Extract hue and saturation bands which are independent of intensity
        ImageFloat32 H = hsv.getBand(0);
        ImageFloat32 S = hsv.getBand(1);
        ImageFloat32 V = hsv.getBand(2);

        // step through each pixel and mark how close it is to the selected color
        BufferedImage output = new BufferedImage(input.width, input.height, BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < hsv.height; y++) {
            for (int x = 0; x < hsv.width; x++) {
                // Hue is an angle in radians, so simple subtraction doesn't work
                float dh = UtilAngle.dist(H.unsafe_get(x, y), hue) / (float) Math.PI;
                float ds = (S.unsafe_get(x, y) - saturation);
                float dv = (V.unsafe_get(x, y) - value) / 255f;

                // this distance measure is a bit naive, but good enough for to demonstrate the concept
                float dist2 = dh * dh + ds * ds + dv * dv;
                if (dist2 <= maxDist2) {
                    output.setRGB(x, y, image.getRGB(x, y));
                }
            }
        }

        return output;
    }

}
