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
    private BufferedImage alphanumTextLinesDebugImage = null;
    private BufferedImage allTemplatesDebugImage = null;
    private BufferedImage allTextLinesDebugImage = null;
    private ScannedBodyInfo scannedBodyInfo = null;

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

    public BufferedImage getAlphanumTextLinesDebugImage() {
        return this.alphanumTextLinesDebugImage;
    }

    public void setAlphanumTextLinesDebugImage(BufferedImage alphanumTextLinesDebugImage) {
        this.alphanumTextLinesDebugImage = alphanumTextLinesDebugImage;
    }

    public BufferedImage getAllTemplatesDebugImage() {
        return this.allTemplatesDebugImage;
    }

    public void setAllTemplatesDebugImage(BufferedImage allTemplatesDebugImage) {
        this.allTemplatesDebugImage = allTemplatesDebugImage;
    }

    public BufferedImage getAllTextLinesDebugImage() {
        return this.allTextLinesDebugImage;
    }

    public void setAllTextLinesDebugImage(BufferedImage allTextLinesDebugImage) {
        this.allTextLinesDebugImage = allTextLinesDebugImage;
    }

    public ScannedBodyInfo getScannedBodyInfo() {
        return this.scannedBodyInfo;
    }

    public void setScannedBodyInfo(ScannedBodyInfo scannedBodyInfo) {
        this.scannedBodyInfo = scannedBodyInfo;
    }

}
