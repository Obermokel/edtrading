package borg.edtrading.ocr;

import boofcv.alg.misc.ImageMiscOps;
import boofcv.alg.misc.ImageStatistics;
import boofcv.struct.image.GrayU8;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

/**
 * CharacterLocator
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CharacterLocator {

    static final Logger logger = LogManager.getLogger(CharacterLocator.class);

    private final int minWidth;
    private final int maxWidth;
    private final int minHeight;
    private final int maxHeight;
    private final int border;

    public CharacterLocator(int minWidth, int maxWidth, int minHeight, int maxHeight, int border) {
        this.minWidth = minWidth;
        this.maxWidth = maxWidth;
        this.minHeight = minHeight;
        this.maxHeight = maxHeight;
        this.border = border;
    }

    public List<Rectangle> locateCharacters(GrayU8 image) {
        List<Rectangle> result = new ArrayList<>();

        GrayU8 copy = image.clone();
        for (int y = 0; y < copy.height; y++) {
            for (int x = 0; x < copy.width; x++) {
                if (copy.unsafe_get(x, y) > 0) {
                    Rectangle r = expand(copy, x, y);

                    // Fill black to not detect again
                    ImageMiscOps.fillRectangle(copy, 0, r.x, r.y, r.width, r.height);

                    // Check size and add to result
                    if (r.width >= this.minWidth && r.width <= this.maxWidth && r.height >= this.minHeight && r.height <= this.maxHeight) {
                        Rectangle rWithBorder = new Rectangle(r.x - this.border, r.y - this.border, r.width + 2 * this.border, r.height + 2 * this.border);
                        Rectangle fullSize = new Rectangle(0, 0, image.width, image.height);
                        result.add(rWithBorder.intersection(fullSize));
                    }
                }
            }
        }

        return result;
    }

    private Rectangle expand(GrayU8 image, int x, int y) {
        Rectangle r = new Rectangle(x, y, 1, 1);

        boolean expanded = false;
        boolean reachedRightBorder = false;
        boolean reachedBottomBorder = false;
        boolean reachedLeftBorder = false;
        boolean reachedTopBorder = false;
        do {
            expanded = false;

            // To the right
            try {
                if (!reachedRightBorder && ImageStatistics.max(image.subimage(r.x + r.width, r.y, r.x + r.width + 1, r.y + r.height)) > 0) {
                    r = new Rectangle(r.x, r.y, r.width + 1, r.height);
                    expanded = true;
                }
            } catch (IllegalArgumentException e) {
                reachedRightBorder = true;
            }

            // To the bottom
            try {
                if (!reachedBottomBorder && ImageStatistics.max(image.subimage(r.x, r.y + r.height, r.x + r.width, r.y + r.height + 1)) > 0) {
                    r = new Rectangle(r.x, r.y, r.width, r.height + 1);
                    expanded = true;
                }
            } catch (IllegalArgumentException e) {
                reachedBottomBorder = true;
            }

            // To the left
            try {
                if (!reachedLeftBorder && ImageStatistics.max(image.subimage(r.x - 1, r.y, r.x, r.y + r.height)) > 0) {
                    r = new Rectangle(r.x - 1, r.y, r.width + 1, r.height);
                    expanded = true;
                }
            } catch (IllegalArgumentException e) {
                reachedLeftBorder = true;
            }

            // To the top
            try {
                if (!reachedTopBorder && ImageStatistics.max(image.subimage(r.x, r.y - 1, r.x + r.width, r.y)) > 0) {
                    r = new Rectangle(r.x, r.y - 1, r.width, r.height + 1);
                    expanded = true;
                }
            } catch (IllegalArgumentException e) {
                reachedTopBorder = true;
            }
        } while (expanded);

        return r;
    }

}
