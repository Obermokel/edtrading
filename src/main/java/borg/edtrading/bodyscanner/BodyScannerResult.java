package borg.edtrading.bodyscanner;

import borg.edtrading.screenshots.Screenshot;

import java.awt.image.BufferedImage;

/**
 * BodyScannerResult
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyScannerResult {

    private Screenshot screenshot = null;
    private BufferedImage alphanumTemplatesDebugImage = null;

    public BodyScannerResult(Screenshot screenshot) {
        this.setScreenshot(screenshot);
    }

    public Screenshot getScreenshot() {
        return this.screenshot;
    }

    public void setScreenshot(Screenshot screenshot) {
        this.screenshot = screenshot;
    }

    public BufferedImage getAlphanumTemplatesDebugImage() {
        return this.alphanumTemplatesDebugImage;
    }

    public void setAlphanumTemplatesDebugImage(BufferedImage alphanumTemplatesDebugImage) {
        this.alphanumTemplatesDebugImage = alphanumTemplatesDebugImage;
    }

}
