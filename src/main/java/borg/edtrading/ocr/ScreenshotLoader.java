package borg.edtrading.ocr;

import boofcv.alg.filter.binary.GThresholdImageOps;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

/**
 * ScreenshotLoader
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScreenshotLoader {

    static final Logger logger = LogManager.getLogger(ScreenshotLoader.class);

    public Screenshot loadSystemMapScreenshot(File screenshotFile) throws IOException {
        BufferedImage bi = ImageIO.read(screenshotFile);
        GrayF32 grayF32 = ConvertBufferedImage.convertFrom(bi, (GrayF32) null);
        GrayU8 thresholded = GThresholdImageOps.threshold(grayF32, null, 0.5, false);

        return new Screenshot(screenshotFile, grayF32);
    }

}
