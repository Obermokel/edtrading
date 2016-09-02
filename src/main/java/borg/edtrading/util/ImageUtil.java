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

    public static BufferedImage toFullHd(BufferedImage original) {
        if (original.getWidth() == 1920 && original.getHeight() == 1080) {
            return original;
        } else {
            float scale = original.getHeight() / 1080.0f;
            int targetHeight = Math.round(original.getHeight() / scale);
            int targetWidth = Math.round(original.getWidth() / scale);

            // This would scale 3440x1440 (21:9) to 2580x1080, which allows to crop the image to FullHD.
            // However 1600x1200 (4:3) would be scaled to 1440x1080, which is too small in width to be cropped.
            // In this case scale by width instead of height.
            if (targetWidth < 1920) {
                scale = original.getWidth() / 1920.0f;
                targetWidth = Math.round(original.getWidth() / scale);
                targetHeight = Math.round(original.getHeight() / scale);
            }

            BufferedImage scaled = scaleImage(original, targetWidth, targetHeight);

            if (scaled.getWidth() > 1920) {
                int x = (scaled.getWidth() - 1920) / 2;
                return scaled.getSubimage(x, 0, 1920, 1080);
            } else if (scaled.getHeight() > 1080) {
                int y = (scaled.getHeight() - 1080) / 2;
                return scaled.getSubimage(0, y, 1920, 1080);
            } else {
                return scaled;
            }
        }
    }

    public static BufferedImage toFourK(BufferedImage original) {
        if (original.getWidth() == 3840 && original.getHeight() == 2160) {
            return original;
        } else {
            float scale = original.getHeight() / 2160.0f;
            int targetHeight = Math.round(original.getHeight() / scale);
            int targetWidth = Math.round(original.getWidth() / scale);

            // This would scale 3440x1440 (21:9) to 5160x2160, which allows to crop the image to 4k.
            // However 1600x1200 (4:3) would be scaled to 2880x2160, which is too small in width to be cropped.
            // In this case scale by width instead of height.
            if (targetWidth < 3840) {
                scale = original.getWidth() / 3840.0f;
                targetWidth = Math.round(original.getWidth() / scale);
                targetHeight = Math.round(original.getHeight() / scale);
            }

            BufferedImage scaled = scaleImage(original, targetWidth, targetHeight);

            if (scaled.getWidth() > 3840) {
                int x = (scaled.getWidth() - 3840) / 2;
                return scaled.getSubimage(x, 0, 3840, 2160);
            } else if (scaled.getHeight() > 2160) {
                int y = (scaled.getHeight() - 2160) / 2;
                return scaled.getSubimage(0, y, 3840, 2160);
            } else {
                return scaled;
            }
        }
    }

    public static BufferedImage toDoubleFourK(BufferedImage original) {
        if (original.getWidth() == 7680 && original.getHeight() == 4320) {
            return original;
        } else {
            float scale = original.getHeight() / 4320.0f;
            int targetHeight = Math.round(original.getHeight() / scale);
            int targetWidth = Math.round(original.getWidth() / scale);

            if (targetWidth < 7680) {
                scale = original.getWidth() / 7680.0f;
                targetWidth = Math.round(original.getWidth() / scale);
                targetHeight = Math.round(original.getHeight() / scale);
            }

            BufferedImage scaled = scaleImage(original, targetWidth, targetHeight);

            if (scaled.getWidth() > 7680) {
                int x = (scaled.getWidth() - 7680) / 2;
                return scaled.getSubimage(x, 0, 7680, 4320);
            } else if (scaled.getHeight() > 4320) {
                int y = (scaled.getHeight() - 4320) / 2;
                return scaled.getSubimage(0, y, 7680, 4320);
            } else {
                return scaled;
            }
        }
    }

    public static BufferedImage fourKToHd(BufferedImage original) {
        float scale = 3840 / 1280.0f;
        int targetWidth = Math.round(original.getWidth() / scale);
        int targetHeight = Math.round(original.getHeight() / scale);

        return scaleImage(original, targetWidth, targetHeight);
    }

    public static BufferedImage fourKToFullHd(BufferedImage original) {
        float scale = 3840 / 1920.0f;
        int targetWidth = Math.round(original.getWidth() / scale);
        int targetHeight = Math.round(original.getHeight() / scale);

        return scaleImage(original, targetWidth, targetHeight);
    }

    private static BufferedImage scaleImage(BufferedImage original, int targetWidth, int targetHeight) {
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

}
