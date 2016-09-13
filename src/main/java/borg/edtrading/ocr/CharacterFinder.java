package borg.edtrading.ocr;

import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.alg.filter.binary.BinaryImageOps;
import boofcv.alg.filter.binary.Contour;
import boofcv.alg.misc.ImageMiscOps;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.ConnectRule;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import borg.edtrading.Constants;
import georegression.struct.point.Point2D_I32;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.ListIterator;
import java.util.Random;

import javax.imageio.ImageIO;

/**
 * CharacterFinder
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public abstract class CharacterFinder {

    static final Logger logger = LogManager.getLogger(CharacterFinder.class);

    public static List<Rectangle> findCharacterLocations(BufferedImage cannySuitableImage, boolean writeDebugImages) throws IOException {
        GrayF32 grayImage = ConvertBufferedImage.convertFromSingle(cannySuitableImage, null, GrayF32.class);

        // Find areas which could be characters (boxes of size 4x4 to 128x128).
        List<Rectangle> characterCandidateBoxes = findBoundingBoxes(grayImage, 4, 128, 4, 128); // TODO Won't detect very large or very small chars - either if they are really large/small, or because the resolution is high/low
        if (writeDebugImages) {
            ImageIO.write(markBoxes(cannySuitableImage, characterCandidateBoxes, Color.RED, 1), "PNG", new File(Constants.TEMP_DIR, "CharacterFinder 00 Candidates.png"));
        }

        // Get the median height of all found boxes. Because most characters have a similar height, the median height should be
        // almost the text height. This way we can filter out small dots or large boxes which very likely are not characters.
        int medianHeight = calculateMedianHeight(characterCandidateBoxes);

        // This will remove all candidate boxes which are to small/large. The remaining boxes are very likely boxes around characters.
        List<Rectangle> medianBoxes = findBoxesCloseToMedianHeight(characterCandidateBoxes, medianHeight);
        if (writeDebugImages) {
            ImageIO.write(markBoxes(cannySuitableImage, medianBoxes, Color.ORANGE, 1), "PNG", new File(Constants.TEMP_DIR, "CharacterFinder 01 Medians.png"));
        }

        // Expand all character boxes a bit so they do overlap. This allows us to again find bounding boxes, but this time the bounding
        // boxes will be around a whole line of characters.
        List<Rectangle> textLineBoxes = groupToTextLineBoxes(medianBoxes, cannySuitableImage.getWidth(), cannySuitableImage.getHeight());

        // Expand all text lines to 2x median line height, so they all have the same height.
        // This is because not all text lines have characters which go below the baseline or higher than usual.
        // Also move text lines which are only a few pixel away in y-direction to the same y-coord.
        medianHeight = calculateMedianHeight(textLineBoxes);
        textLineBoxes = expandTextLineBoxes(textLineBoxes, 2 * medianHeight);
        textLineBoxes = moveSimilarTextLineBoxes(textLineBoxes);
        if (writeDebugImages) {
            ImageIO.write(markBoxes(cannySuitableImage, textLineBoxes, Color.YELLOW, 1), "PNG", new File(Constants.TEMP_DIR, "CharacterFinder 02 Lines.png"));
        }

        // Finally go back to the candidates and see which ones are inside the text line boxes. Expand those which are
        // inside text line boxes to the full height of the text line.
        //List<Rectangle> characterLocations = createCharacterBoxesWithinTextLines(characterCandidateBoxes, textLineBoxes);
        List<Rectangle> characterLocations = extractCharBoxesFromTextLines(grayImage, textLineBoxes);
        if (writeDebugImages) {
            ImageIO.write(markBoxes(cannySuitableImage, characterLocations, Color.GREEN, 1), "PNG", new File(Constants.TEMP_DIR, "CharacterFinder 03 Chars.png"));
        }
        if (writeDebugImages) {
            //            BufferedImage combined = markBoxes(cannySuitableImage, characterCandidateBoxes, Color.RED, 1);
            //            combined = markBoxes(combined, medianBoxes, Color.ORANGE, 1);
            BufferedImage combined = markBoxes(cannySuitableImage, textLineBoxes, Color.YELLOW, 1);
            combined = markBoxes(combined, characterLocations, null, 1);
            ImageIO.write(combined, "PNG", new File(Constants.TEMP_DIR, "CharacterFinder 99 Combined.png"));
        }
        return characterLocations;
    }

    private static List<Rectangle> moveSimilarTextLineBoxes(List<Rectangle> textLineBoxes) {
        List<Rectangle> result = new ArrayList<>(textLineBoxes.size());

        while (!textLineBoxes.isEmpty()) {
            // Find
            Rectangle referenceBox = textLineBoxes.remove(0);
            int sumY = referenceBox.y;
            int avgY = sumY;
            List<Rectangle> similarBoxes = new ArrayList<>();
            ListIterator<Rectangle> it = textLineBoxes.listIterator();
            while (it.hasNext()) {
                Rectangle otherBox = it.next();
                if (Math.abs(otherBox.y - avgY) <= 10) {
                    similarBoxes.add(otherBox);
                    it.remove();
                    sumY += otherBox.y;
                    avgY = Math.round(sumY / (1.0f + similarBoxes.size()));
                }
            }
            similarBoxes.add(referenceBox);

            // Move
            for (Rectangle r : similarBoxes) {
                result.add(new Rectangle(r.x, avgY, r.width, r.height));
            }
        }

        return result;
    }

    private static List<Rectangle> expandTextLineBoxes(List<Rectangle> textLineBoxes, int targetHeight) {
        List<Rectangle> result = new ArrayList<>(textLineBoxes.size());

        for (Rectangle r : textLineBoxes) {
            int diff = targetHeight - r.height;
            int newY = r.y - (diff / 2);
            result.add(new Rectangle(r.x, newY, r.width, targetHeight));
        }

        return result;
    }

    private static List<Rectangle> extractCharBoxesFromTextLines(GrayF32 grayImage, List<Rectangle> textLineBoxes) {
        List<Rectangle> charBoxes = new ArrayList<>();

        for (Rectangle textLineBox : textLineBoxes) {
            int x = Math.max(0, textLineBox.x);

            while (x < Math.min(grayImage.width, textLineBox.x + textLineBox.width)) {
                // Search for char start, which is the first vertical line which is not all black
                boolean isAllBlack = true;
                for (int y = Math.max(0, textLineBox.y); y < Math.min(grayImage.height, textLineBox.y + textLineBox.height); y++) {
                    if (grayImage.unsafe_get(x, y) != 0f) {
                        isAllBlack = false;
                        break;
                    }
                }

                if (isAllBlack) {
                    x++; // Char not yet started
                } else {
                    int xFirstWhite = x; // Start pos (x) of char

                    while (x < Math.min(grayImage.width, textLineBox.x + textLineBox.width)) {
                        // Search for char end, which is the first vertical line which again is all black
                        isAllBlack = true;
                        for (int y = Math.max(0, textLineBox.y); y < Math.min(grayImage.height, textLineBox.y + textLineBox.height); y++) {
                            if (grayImage.unsafe_get(x, y) != 0f) {
                                isAllBlack = false;
                                break;
                            }
                        }

                        if (!isAllBlack) {
                            x++; // Char not yet ended
                        } else {
                            int xLastWhite = x - 1; // End pos (x) of char

                            // Add charBox and continue
                            Rectangle charBox = new Rectangle(xFirstWhite - 1, textLineBox.y, (xLastWhite - xFirstWhite) + 3, textLineBox.height);
                            charBoxes.add(charBox);
                            break;
                        }
                    }
                }
            }
        }

        return charBoxes;
    }

    private static List<Rectangle> findBoundingBoxes(GrayF32 cannySuitableImage, final int minWidth, final int maxWidth, final int minHeight, final int maxHeight) {
        // Find external contours
        GrayU8 binary = new GrayU8(cannySuitableImage.width, cannySuitableImage.height);
        CannyEdge<GrayF32, GrayF32> canny = FactoryEdgeDetectors.canny(3, false, true, GrayF32.class, GrayF32.class);
        canny.process(cannySuitableImage, 0.1f, 0.3f, binary);
        List<Contour> contours = BinaryImageOps.contour(binary, ConnectRule.EIGHT, null);

        // Create bounding boxes
        List<Rectangle> result = new ArrayList<>(contours.size());
        for (Contour c : contours) {
            Rectangle boundingBox = contourToBoundingBox(c, minWidth, maxWidth, minHeight, maxHeight);
            if (boundingBox != null) {
                result.add(boundingBox);
            }
        }

        // Finished
        return result;
    }

    private static Rectangle contourToBoundingBox(Contour c, final int minWidth, final int maxWidth, final int minHeight, final int maxHeight) {
        if (c.external.size() >= 2) {
            List<Integer> xList = new ArrayList<>(c.external.size());
            List<Integer> yList = new ArrayList<>(c.external.size());

            for (Point2D_I32 p : c.external) {
                xList.add(p.x);
                yList.add(p.y);
            }

            Collections.sort(xList);
            Collections.sort(yList);

            int x0 = xList.get(0);
            int x1 = xList.get(xList.size() - 1);
            int y0 = yList.get(0);
            int y1 = yList.get(yList.size() - 1);

            int width = x1 - x0;
            int height = y1 - y0;

            if (width >= minWidth && width <= maxWidth) {
                if (height >= minHeight && height <= maxHeight) {
                    return new Rectangle(x0, y0, width, height);
                }
            }
        }

        return null;
    }

    private static int calculateMedianWidth(List<Rectangle> boxes) {
        List<Integer> widths = new ArrayList<>(boxes.size());
        for (Rectangle r : boxes) {
            widths.add(r.width);
        }
        Collections.sort(widths);
        return widths.get(widths.size() / 2);
    }

    private static int calculateMedianHeight(List<Rectangle> boxes) {
        List<Integer> heights = new ArrayList<>(boxes.size());
        for (Rectangle r : boxes) {
            heights.add(r.height);
        }
        Collections.sort(heights);
        return heights.get(heights.size() / 2);
    }

    private static List<Rectangle> findBoxesCloseToMedianHeight(List<Rectangle> allBoxes, int medianHeight) {
        int minHeight = Math.round(medianHeight * 0.6f);
        int maxHeight = Math.round(medianHeight * 1.8f);

        List<Rectangle> result = new ArrayList<>();
        for (Rectangle b : allBoxes) {
            if (b.height >= minHeight && b.height <= maxHeight) {
                result.add(b);
            }
        }
        return result;
    }

    private static List<Rectangle> groupToTextLineBoxes(List<Rectangle> medianBoxes, int imageWidth, int imageHeight) {
        GrayF32 grayF32 = new GrayF32(imageWidth, imageHeight);
        ImageMiscOps.fill(grayF32, 0);
        for (Rectangle r : medianBoxes) {
            int expandedWidth = 3 * r.width;
            int movedLeft = r.x - r.width;
            ImageMiscOps.fillRectangle(grayF32, 1, movedLeft, r.y, expandedWidth, r.height);
        }
        return findBoundingBoxes(grayF32, 4, 4096, 16, 128);
    }

    private static List<Rectangle> createCharacterBoxesWithinTextLines(List<Rectangle> characterCandidateBoxes, List<Rectangle> textLineBoxes) {
        List<Rectangle> result = new ArrayList<>();

        for (Rectangle candidate : characterCandidateBoxes) {
            for (Rectangle line : textLineBoxes) {
                if (line.contains(candidate)) {
                    // Create box with full line height and 1 extra pixel left/right
                    Rectangle charBox = new Rectangle(candidate.x - 1, line.y, candidate.width + 2, line.height);
                    result.add(charBox);
                    break;
                }
            }
        }

        return result;
    }

    private static BufferedImage markBoxes(BufferedImage image, List<Rectangle> characterLocations, Color color, float strokeWidth) {
        Random random = new Random();
        BufferedImage result = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D g = result.createGraphics();
        try {
            g.drawImage(image, 0, 0, null);

            g.setStroke(new BasicStroke(strokeWidth));
            for (Rectangle r : characterLocations) {
                if (color != null) {
                    g.setColor(color);
                } else {
                    g.setColor(new Color(random.nextInt()));
                }
                g.drawRect(r.x, r.y, r.width, r.height);
            }
        } finally {
            g.dispose();
        }
        return result;
    }

}
