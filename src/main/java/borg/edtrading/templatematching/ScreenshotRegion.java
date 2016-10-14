package borg.edtrading.templatematching;

import boofcv.struct.image.GrayF32;
import borg.edtrading.ocr.Screenshot;

/**
 * Region (pixel area) of a full screenshot
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScreenshotRegion {

    private final Screenshot screenshot;
    private final GrayF32 pixels;
    private final int x;
    private final int y;

    public ScreenshotRegion(Screenshot screenshot, GrayF32 pixels, int x, int y) {
        this.screenshot = screenshot;
        this.pixels = pixels;
        this.x = x;
        this.y = y;
    }

    @Override
    public String toString() {
        return this.getWidth() + "x" + this.getHeight() + " region of " + this.getScreenshot();
    }

    /**
     * Full screenshot this region is from
     */
    public Screenshot getScreenshot() {
        return this.screenshot;
    }

    /**
     * Pixels of the region
     */
    public GrayF32 getPixels() {
        return this.pixels;
    }

    /**
     * X coord in the full screenshot
     */
    public int getX() {
        return this.x;
    }

    /**
     * Y coord in the full screenshot
     */
    public int getY() {
        return this.y;
    }

    /**
     * Width in pixels of this region
     */
    public int getWidth() {
        return this.pixels.width;
    }

    /**
     * Height in pixels of this region
     */
    public int getHeight() {
        return this.pixels.height;
    }

}
