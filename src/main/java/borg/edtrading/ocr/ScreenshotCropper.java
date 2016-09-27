package borg.edtrading.ocr;

import borg.edtrading.util.ImageUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;

/**
 * Crops screenshots to special regions of interest.
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public abstract class ScreenshotCropper {

    static final Logger logger = LogManager.getLogger(ScreenshotCropper.class);

    public static BufferedImage cropSystemMapToPlanetMaterials(BufferedImage systemMapScreenshot) {
        BufferedImage defaultAspectRatio = ImageUtil.to16by9(systemMapScreenshot);

        final float xPercent = 20f / 3840f; // x=20 on 4k
        final float yPercent = 1600f / 2160f; // y=1600 on 4k
        final float wPercent = 820f / 3840f; // width=820 on 4k
        final float hPercent = 430f / 2160f; // height=430 on 4k

        int x = Math.round(xPercent * defaultAspectRatio.getWidth());
        int y = Math.round(yPercent * defaultAspectRatio.getHeight());
        int w = Math.round(wPercent * defaultAspectRatio.getWidth());
        int h = Math.round(hPercent * defaultAspectRatio.getHeight());

        return defaultAspectRatio.getSubimage(x, y, w, h);
    }

    public static BufferedImage cropSystemMapToBodyInfo(BufferedImage systemMapScreenshot) {
        BufferedImage defaultAspectRatio = ImageUtil.to16by9(systemMapScreenshot);

        final float xPercent = 20f / 3840f; // x=20 on 4k
        final float yPercent = 340f / 2160f; // y=340 on 4k
        final float wPercent = 820f / 3840f; // width=820 on 4k
        final float hPercent = 1690f / 2160f; // height=1690 on 4k

        int x = Math.round(xPercent * defaultAspectRatio.getWidth());
        int y = Math.round(yPercent * defaultAspectRatio.getHeight());
        int w = Math.round(wPercent * defaultAspectRatio.getWidth());
        int h = Math.round(hPercent * defaultAspectRatio.getHeight());

        return defaultAspectRatio.getSubimage(x, y, w, h);
    }

    public static BufferedImage cropSystemMapToBodyName(BufferedImage systemMapScreenshot) {
        BufferedImage defaultAspectRatio = ImageUtil.to16by9(systemMapScreenshot);

        final float xPercent = 2110f / 3840f; // x=2110 on 4k
        final float yPercent = 635f / 2160f; // y=635 on 4k
        final float wPercent = 850f / 3840f; // width=850 on 4k
        final float hPercent = 185f / 2160f; // height=185 on 4k

        int x = Math.round(xPercent * defaultAspectRatio.getWidth());
        int y = Math.round(yPercent * defaultAspectRatio.getHeight());
        int w = Math.round(wPercent * defaultAspectRatio.getWidth());
        int h = Math.round(hPercent * defaultAspectRatio.getHeight());

        return defaultAspectRatio.getSubimage(x, y, w, h);
    }

}
