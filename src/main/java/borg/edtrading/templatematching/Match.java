package borg.edtrading.templatematching;

/**
 * Successful match of a template against a screenshot region
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class Match {

    private final ScreenshotRegion region;
    private final Template template;
    private final int x;
    private final int y;
    private final float error;
    private final float errorPerPixel;

    public Match(ScreenshotRegion region, Template template, int x, int y, float error, float errorPerPixel) {
        this.region = region;
        this.template = template;
        this.x = x;
        this.y = y;
        this.error = error;
        this.errorPerPixel = errorPerPixel;
    }

    /**
     * Screenshot region that has successfully been matched against
     */
    public ScreenshotRegion getRegion() {
        return this.region;
    }

    /**
     * The template that has matched
     */
    public Template getTemplate() {
        return this.template;
    }

    /**
     * X coord in the screenshot region
     */
    public int getX() {
        return this.x;
    }

    /**
     * Y coord in the screenshot region
     */
    public int getY() {
        return this.y;
    }

    public float getError() {
        return this.error;
    }

    public float getErrorPerPixel() {
        return this.errorPerPixel;
    }

}
