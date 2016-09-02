package borg.edtrading.util;

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

}
