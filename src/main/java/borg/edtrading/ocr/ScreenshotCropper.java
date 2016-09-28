package borg.edtrading.ocr;

import boofcv.alg.distort.RemovePerspectiveDistortion;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.ImageType;
import boofcv.struct.image.Planar;
import borg.edtrading.util.ImageUtil;
import georegression.struct.point.Point2D_F64;
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

    public static BufferedImage cropToInventory(BufferedImage screenshot) {
        BufferedImage screenshot4k = ImageUtil.toFourK(screenshot);
        BufferedImage correctedImage = correctDistort(screenshot4k);
        return correctedImage.getSubimage(135, 0, 1280, 720); // Crop to inventory
    }

    private static BufferedImage correctDistort(BufferedImage fourKImage) {
        Planar<GrayF32> input = ConvertBufferedImage.convertFromMulti(fourKImage, null, true, GrayF32.class);

        RemovePerspectiveDistortion<Planar<GrayF32>> removePerspective = new RemovePerspectiveDistortion<Planar<GrayF32>>(1600, 800, ImageType.pl(3, GrayF32.class));

        // Specify the corners in the input image of the region.
        // Order matters! top-left, top-right, bottom-right, bottom-left
        //@formatter:off
        if (!removePerspective.apply(input,
                new Point2D_F64(1702,779),
                new Point2D_F64(3400,871),
                new Point2D_F64(3311,1688),
                new Point2D_F64(1678,1502))) {
            throw new RuntimeException("Failed!?!?");
        }
        //@formatter:on

        Planar<GrayF32> output = removePerspective.getOutput();

        return ConvertBufferedImage.convertTo_F32(output, null, true);
    }

}
