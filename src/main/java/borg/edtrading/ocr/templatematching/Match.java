package borg.edtrading.ocr.templatematching;

import borg.edtrading.screenshots.Region;

import java.awt.Rectangle;
import java.util.List;
import java.util.Locale;

/**
 * Successful match of a template against a screenshot region
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Match {

    private final Region region;
    private final Template template;
    private final int xInRegion;
    private final int yInRegion;
    private final int width;
    private final int height;
    private final float error;
    private final float errorPerPixel;

    private String shouldHaveBeen = null;

    public Match(Region region, Template template, int xInRegion, int yInRegion, int width, int height, float error, float errorPerPixel) {
        this.region = region;
        this.template = template;
        this.xInRegion = xInRegion;
        this.yInRegion = yInRegion;
        this.width = width;
        this.height = height;
        this.error = error;
        this.errorPerPixel = errorPerPixel;
    }

    @Override
    public String toString() {
        return "<" + this.getTemplate().getText() + "> (" + String.format(Locale.US, "%.3f", this.getErrorPerPixel()) + ")";
    }

    public boolean overlapsWithAny(List<Match> other, int border) {
        Rectangle rThis = new Rectangle(this.getxInScreenshot() - border, this.getyInScreenshot() - border, this.getWidth() + 2 * border, this.getHeight() + 2 * border);
        for (Match m : other) {
            Rectangle rOther = new Rectangle(m.getxInScreenshot() - border, m.getyInScreenshot() - border, m.getWidth() + 2 * border, m.getHeight() + 2 * border);
            if (rThis.intersects(rOther)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Screenshot region that has successfully been matched against
     */
    public Region getRegion() {
        return this.region;
    }

    /**
     * The template that has matched
     */
    public Template getTemplate() {
        return this.template;
    }

    /**
     * X coord in the region
     */
    public int getxInRegion() {
        return this.xInRegion;
    }

    /**
     * Y coord in the region
     */
    public int getyInRegion() {
        return this.yInRegion;
    }

    public int getWidth() {
        return this.width;
    }

    public int getHeight() {
        return this.height;
    }

    /**
     * X coord in the screenshot
     */
    public int getxInScreenshot() {
        return this.getxInRegion() + this.getRegion().getxInScreenshot();
    }

    /**
     * Y coord in the screenshot
     */
    public int getyInScreenshot() {
        return this.getyInRegion() + this.getRegion().getyInScreenshot();
    }

    public float getError() {
        return this.error;
    }

    public float getErrorPerPixel() {
        return this.errorPerPixel;
    }

    public String getShouldHaveBeen() {
        return this.shouldHaveBeen;
    }

    public void setShouldHaveBeen(String shouldHaveBeen) {
        this.shouldHaveBeen = shouldHaveBeen;
    }

}
