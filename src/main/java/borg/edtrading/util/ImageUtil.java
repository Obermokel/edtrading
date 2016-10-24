package borg.edtrading.util;

import boofcv.abst.distort.FDistort;
import boofcv.alg.interpolate.TypeInterpolate;
import boofcv.alg.misc.ImageMiscOps;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Transparency;
import java.awt.image.BufferedImage;

/**
 * ImageUtil
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public abstract class ImageUtil {

    static final Logger logger = LogManager.getLogger(ImageUtil.class);

    private static final float ASPECT_RATIO_16_BY_9 = 16f / 9f;

    public static BufferedImage toFullHd(BufferedImage original) {
        return scaleAndCrop(original, 1920, 1080);
    }

    public static BufferedImage toFourK(BufferedImage original) {
        return scaleAndCrop(original, 3840, 2160);
    }

    public static BufferedImage toQuadFourK(BufferedImage original) {
        return scaleAndCrop(original, 7680, 4320);
    }

    public static BufferedImage scaleAndCrop(BufferedImage original, int targetWidth, int targetHeight) {
        if (original.getWidth() == targetWidth && original.getHeight() == targetHeight) {
            // Perfect!
            return original;
        } else {
            // Scale to the target height
            float scale = original.getHeight() / (float) targetHeight;
            int newHeight = Math.round(original.getHeight() / scale);
            int newWidth = Math.round(original.getWidth() / scale);

            if (newWidth >= targetWidth) {
                // Greater or equal to the target width is okay.
                // If equal we can leave the width as it is, if greater we can crop to the target width.
            } else {
                // If the new width is less than the target width we need to scale by width instead.
                // The new height will then be greater than the target height, so we can crop to the target height.
                scale = original.getWidth() / (float) targetWidth;
                newWidth = Math.round(original.getWidth() / scale);
                newHeight = Math.round(original.getHeight() / scale);
            }

            // Scale!
            BufferedImage scaled = scaleTo(original, newWidth, newHeight);

            // Maybe return a cropped subimage
            return cropTo(scaled, targetWidth, targetHeight);
        }
    }

    public static BufferedImage scaleTo(BufferedImage original, int targetWidth, int targetHeight) {
        if (original.getWidth() == targetWidth && original.getHeight() == targetHeight) {
            return original;
        } else {
            int resultType = original.getTransparency() == Transparency.OPAQUE ? BufferedImage.TYPE_INT_RGB : BufferedImage.TYPE_INT_ARGB;
            BufferedImage resultImage = new BufferedImage(targetWidth, targetHeight, resultType);
            Image scaledImage = original.getScaledInstance(targetWidth, targetHeight, Image.SCALE_SMOOTH);
            Graphics2D resultGraphics = (Graphics2D) resultImage.getGraphics();
            resultGraphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
            resultGraphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            resultGraphics.drawImage(scaledImage, 0, 0, null);
            return resultImage;
        }
    }

    public static BufferedImage to16by9(BufferedImage original) {
        float currentAspectRatio = (float) original.getWidth() / (float) original.getHeight();
        float deltaTo16by9 = currentAspectRatio - ASPECT_RATIO_16_BY_9;

        if (deltaTo16by9 > 0.01) {
            // Crop width
            int targetWidth = Math.round(original.getHeight() * ASPECT_RATIO_16_BY_9);
            return cropTo(original, targetWidth, original.getHeight());
        } else if (deltaTo16by9 < 0.01) {
            // Crop height
            int targetHeight = Math.round(original.getWidth() / ASPECT_RATIO_16_BY_9);
            return cropTo(original, original.getWidth(), targetHeight);
        } else {
            return original;
        }
    }

    public static BufferedImage cropTo(BufferedImage original, int targetWidth, int targetHeight) {
        if (original.getWidth() > targetWidth && original.getHeight() > targetHeight) {
            int x = (original.getWidth() - targetWidth) / 2;
            int y = (original.getHeight() - targetHeight) / 2;
            return original.getSubimage(x, y, targetWidth, targetHeight);
        } else if (original.getWidth() > targetWidth) {
            int x = (original.getWidth() - targetWidth) / 2;
            return original.getSubimage(x, 0, targetWidth, targetHeight);
        } else if (original.getHeight() > targetHeight) {
            int y = (original.getHeight() - targetHeight) / 2;
            return original.getSubimage(0, y, targetWidth, targetHeight);
        } else {
            return original;
        }
    }

    public static void cropAndScaleTo(GrayU8 input, GrayU8 output) {
        // First do the cropping, which is a simple subimage.
        // The less we have to scale, the faster.
        // Also allows re-using memory.
        final float currentAspectRatio = (float) input.width / (float) input.height;
        final float targetAspectRatio = (float) output.width / (float) output.height;

        GrayU8 cropped = null;
        if (currentAspectRatio > targetAspectRatio) {
            cropped = cropTo(input, Math.round(input.height * targetAspectRatio), input.height); // Crop width
        } else if (currentAspectRatio < targetAspectRatio) {
            cropped = cropTo(input, input.width, Math.round(input.width / targetAspectRatio)); // Crop height
        } else {
            cropped = input;
        }

        // Then do the scaling
        scaleTo(cropped, output, output.width, output.height);
    }

    public static GrayU8 cropTo(GrayU8 original, int targetWidth, int targetHeight) {
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

    public static GrayU8 scaleTo(GrayU8 original, GrayU8 prevScaled, int targetWidth, int targetHeight) {
        GrayU8 scaled = prevScaled != null && prevScaled.width == targetWidth && prevScaled.height == targetHeight ? prevScaled : new GrayU8(targetWidth, targetHeight);

        if (original.width == targetWidth && original.height == targetHeight) {
            ImageMiscOps.copy(0, 0, 0, 0, targetWidth, targetHeight, original, scaled);
        } else {
            new FDistort().input(original).output(scaled).interp(TypeInterpolate.BICUBIC).scale().apply();
        }

        return scaled;
    }

    public static GrayF32 normalize(GrayF32 original) {
        float max = 0f;
        for (int y = 0; y < original.height; y++) {
            for (int x = 0; x < original.width; x++) {
                max = Math.max(max, original.unsafe_get(x, y));
            }
        }
        if (max == 1f) {
            return original;
        } else {
            GrayF32 normalized = original.createSameShape();
            for (int y = 0; y < original.height; y++) {
                for (int x = 0; x < original.width; x++) {
                    normalized.unsafe_set(x, y, original.unsafe_get(x, y) / max);
                }
            }
            return normalized;
        }
    }

}
