package borg.edtrading.ocr;

import borg.edtrading.ocr.screenshots.Region;
import borg.edtrading.ocr.templatematching.Template;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;

/**
 * OcrTask
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class OcrTask {

    static final Logger logger = LogManager.getLogger(OcrTask.class);

    private Region screenshotRegion = null;
    private CharacterLocator characterLocator = null;
    private List<Template> templates = null;
    private boolean debugThresholdImage = false;
    private boolean debugBlurredImage = false;
    private boolean debugAlphanumTemplates = false;
    private boolean debugAlphanumTextLines = false;
    private boolean debugAllTemplates = false;
    private boolean debugAllTextLines = false;

    public OcrTask(Region screenshotRegion, CharacterLocator characterLocator, List<Template> templates) {
        this.setScreenshotRegion(screenshotRegion);
        this.setCharacterLocator(characterLocator);
        this.setTemplates(templates);
    }

    public Region getScreenshotRegion() {
        return this.screenshotRegion;
    }

    public void setScreenshotRegion(Region screenshotRegion) {
        this.screenshotRegion = screenshotRegion;
    }

    public CharacterLocator getCharacterLocator() {
        return this.characterLocator;
    }

    public void setCharacterLocator(CharacterLocator characterLocator) {
        this.characterLocator = characterLocator;
    }

    public List<Template> getTemplates() {
        return this.templates;
    }

    public void setTemplates(List<Template> templates) {
        this.templates = templates;
    }

    public boolean isDebugThresholdImage() {
        return this.debugThresholdImage;
    }

    public void setDebugThresholdImage(boolean debugThresholdImage) {
        this.debugThresholdImage = debugThresholdImage;
    }

    public boolean isDebugBlurredImage() {
        return this.debugBlurredImage;
    }

    public void setDebugBlurredImage(boolean debugBlurredImage) {
        this.debugBlurredImage = debugBlurredImage;
    }

    public boolean isDebugAlphanumTemplates() {
        return this.debugAlphanumTemplates;
    }

    public void setDebugAlphanumTemplates(boolean debugAlphanumTemplates) {
        this.debugAlphanumTemplates = debugAlphanumTemplates;
    }

    public boolean isDebugAlphanumTextLines() {
        return this.debugAlphanumTextLines;
    }

    public void setDebugAlphanumTextLines(boolean debugAlphanumTextLines) {
        this.debugAlphanumTextLines = debugAlphanumTextLines;
    }

    public boolean isDebugAllTemplates() {
        return this.debugAllTemplates;
    }

    public void setDebugAllTemplates(boolean debugAllTemplates) {
        this.debugAllTemplates = debugAllTemplates;
    }

    public boolean isDebugAllTextLines() {
        return this.debugAllTextLines;
    }

    public void setDebugAllTextLines(boolean debugAllTextLines) {
        this.debugAllTextLines = debugAllTextLines;
    }

}
