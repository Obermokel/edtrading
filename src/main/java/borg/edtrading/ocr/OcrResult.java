package borg.edtrading.ocr;

import borg.edtrading.Constants;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.imageio.ImageIO;

/**
 * OcrResult
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class OcrResult {

    static final Logger logger = LogManager.getLogger(OcrResult.class);

    private OcrTask ocrTask = null;
    private List<TextLine> textLines = null;
    private BufferedImage thresholdDebugImage = null;
    private BufferedImage blurredDebugImage = null;
    private BufferedImage alphanumTemplatesDebugImage = null;
    private BufferedImage alphanumTextLinesDebugImage = null;
    private BufferedImage allTemplatesDebugImage = null;
    private BufferedImage allTextLinesDebugImage = null;

    public OcrResult(OcrTask ocrTask) {
        this.setOcrTask(ocrTask);
        this.setTextLines(new ArrayList<>(0));
    }

    public void writeDebugImages() throws IOException {
        File screenshotFile = this.getOcrTask().getScreenshotRegion().getScreenshot().getFile();

        if (this.getThresholdDebugImage() != null) {
            ImageIO.write(this.getThresholdDebugImage(), "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", " 00_ThresholdDebugImage.png")));
        }
        if (this.getBlurredDebugImage() != null) {
            ImageIO.write(this.getBlurredDebugImage(), "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", " 01_BlurredDebugImage.png")));
        }
        if (this.getAlphanumTemplatesDebugImage() != null) {
            ImageIO.write(this.getAlphanumTemplatesDebugImage(), "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", " 02_AlphanumTemplatesDebugImage.png")));
        }
        if (this.getAlphanumTextLinesDebugImage() != null) {
            ImageIO.write(this.getAlphanumTextLinesDebugImage(), "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", " 03_AlphanumTextLinesDebugImage.png")));
        }
        if (this.getAllTemplatesDebugImage() != null) {
            ImageIO.write(this.getAllTemplatesDebugImage(), "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", " 04_AllTemplatesDebugImage.png")));
        }
        if (this.getAllTextLinesDebugImage() != null) {
            ImageIO.write(this.getAllTextLinesDebugImage(), "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", " 05_AllTextLinesDebugImage.png")));
        }
    }

    public OcrTask getOcrTask() {
        return this.ocrTask;
    }

    public void setOcrTask(OcrTask ocrTask) {
        this.ocrTask = ocrTask;
    }

    public List<TextLine> getTextLines() {
        return this.textLines;
    }

    public void setTextLines(List<TextLine> textLines) {
        this.textLines = textLines;
    }

    public BufferedImage getThresholdDebugImage() {
        return this.thresholdDebugImage;
    }

    public void setThresholdDebugImage(BufferedImage thresholdDebugImage) {
        this.thresholdDebugImage = thresholdDebugImage;
    }

    public BufferedImage getBlurredDebugImage() {
        return this.blurredDebugImage;
    }

    public void setBlurredDebugImage(BufferedImage blurredDebugImage) {
        this.blurredDebugImage = blurredDebugImage;
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

}
