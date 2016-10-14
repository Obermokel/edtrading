package borg.edtrading.ocrOLD;

import boofcv.alg.distort.RemovePerspectiveDistortion;
import boofcv.alg.filter.binary.GThresholdImageOps;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageType;
import boofcv.struct.image.Planar;
import borg.edtrading.Constants;
import borg.edtrading.util.ImageUtil;
import georegression.struct.point.Point2D_F64;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

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

        //        final float xPercent = 2100f / 3840f; // x=2100 on 4k
        //        final float yPercent = 635f / 2160f; // y=635 on 4k
        //        final float wPercent = 860f / 3840f; // width=860 on 4k
        //        final float hPercent = 185f / 2160f; // height=185 on 4k
        final float xPercent = 1900f / 3840f; // x=2100 on 4k
        final float yPercent = 600f / 2160f; // y=635 on 4k
        final float wPercent = 1200f / 3840f; // width=860 on 4k
        final float hPercent = 500f / 2160f; // height=185 on 4k

        int x = Math.round(xPercent * defaultAspectRatio.getWidth());
        int y = Math.round(yPercent * defaultAspectRatio.getHeight());
        int w = Math.round(wPercent * defaultAspectRatio.getWidth());
        int h = Math.round(hPercent * defaultAspectRatio.getHeight());

        return defaultAspectRatio.getSubimage(x, y, w, h);
    }

    public static BufferedImage cropAndHighlightSystemMapToBodyName(BufferedImage systemMapScreenshot) throws IOException {
        BufferedImage fullHdImage = ImageUtil.toFullHd(systemMapScreenshot);
        BufferedImage bodyNameImage = ScreenshotCropper.cropSystemMapToBodyName(fullHdImage);
        GrayF32 f32 = ConvertBufferedImage.convertFrom(bodyNameImage, (GrayF32) null);
        GrayU8 u8 = GThresholdImageOps.threshold(f32, null, 64.0, false);

        File hakenFile = new File(Constants.TEMPLATES_DIR, "Body Name Misc\\Haken\\HakenThresholdFullHd2.png");
        GrayU8 hakenU8 = ConvertBufferedImage.convertFrom(ImageIO.read(hakenFile), (GrayU8) null);

        // Search the haken!
        Integer bestError = null;
        Integer bestX = null;
        Integer bestY = null;
        for (int yInImage = 0; yInImage < u8.height - hakenU8.height; yInImage++) {
            for (int xInImage = 0; xInImage < u8.width - hakenU8.width; xInImage++) {
                int error = 0;
                for (int yInTemplate = 0; yInTemplate < hakenU8.height; yInTemplate++) {
                    for (int xInTemplate = 0; xInTemplate < hakenU8.width; xInTemplate++) {
                        if (hakenU8.unsafe_get(xInTemplate, yInTemplate) > 0) {
                            if (u8.unsafe_get(xInImage + xInTemplate, yInImage + yInTemplate) <= 0) {
                                error++;
                                if (bestError != null && error >= bestError) {
                                    break;
                                }
                            }
                        }
                    }
                    if (bestError != null && error >= bestError) {
                        break;
                    }
                }
                if (bestError == null || error < bestError) {
                    bestError = error;
                    bestX = xInImage;
                    bestY = yInImage;
                }
            }
        }

        // Extract from 4k
        if (bestError == null) {
            return null;
        } else {
            int xIn4k = (bestX + 42) * 2;
            int yIn4k = bestY * 2;
            int wIn4k = (hakenU8.width - 42) * 2;
            int hIn4k = hakenU8.height * 2;
            BufferedImage fourKImage = ImageUtil.toFourK(systemMapScreenshot);
            BufferedImage areaImage = ScreenshotCropper.cropSystemMapToBodyName(fourKImage);
            BufferedImage si = areaImage.getSubimage(xIn4k, yIn4k, wIn4k, hIn4k);
            BufferedImage ti = ScreenshotPreprocessor.highlightWhiteText(si);
            for (int y = 60; y < 75; y++) {
                for (int x = 0; x < ti.getWidth(); x++) {
                    ti.setRGB(x, y, 0xff000000);
                }
            }
            return ti;
        }
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
