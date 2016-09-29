package borg.edtrading.ocr;

import boofcv.alg.color.ColorHsv;
import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.alg.filter.binary.GThresholdImageOps;
import boofcv.alg.filter.blur.BlurImageOps;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.gui.image.VisualizeImageData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayS16;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.Planar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.image.RasterFormatException;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * Preprocesses screenshots to make OCR easier.
 *
 * @see #localSquareThreshold(GrayF32, int, double, boolean)
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public abstract class ScreenshotPreprocessor {

    static final Logger logger = LogManager.getLogger(ScreenshotPreprocessor.class);

    public static BufferedImage darkenSaturatedAreas(BufferedImage originalImage) {
        Planar<GrayF32> rgb = ConvertBufferedImage.convertFromMulti(originalImage, null, true, GrayF32.class);
        Planar<GrayF32> hsv = rgb.createSameShape();

        ColorHsv.rgbToHsv_F32(rgb, hsv);
        GrayF32 S = hsv.getBand(1);
        GrayF32 V = hsv.getBand(2);
        for (int y = 0; y < hsv.height; y++) {
            for (int x = 0; x < hsv.width; x++) {
                float s = S.unsafe_get(x, y);
                if (s > 0.05f) {
                    S.unsafe_set(x, y, 0.05f);
                    float v = V.unsafe_get(x, y);
                    V.unsafe_set(x, y, v / 5.0f);
                }
            }
        }
        ColorHsv.hsvToRgb_F32(hsv, rgb);

        return ConvertBufferedImage.convertTo_F32(rgb, null, true);
    }

    public static BufferedImage gaussian(BufferedImage originalImage, int radius) {
        GrayU8 gray = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
        GrayU8 gaussian = BlurImageOps.gaussian(gray, null, /* sigma = */ -1, radius, null);
        return VisualizeImageData.grayMagnitude(gaussian, null, -1);
    }

    public static BufferedImage cannyEdge(BufferedImage originalImage) {
        return cannyEdge(originalImage, 3, 0.1f, 0.3f);
    }

    public static BufferedImage cannyEdge(BufferedImage originalImage, int blurRadius, float threshLow, float threshHigh) {
        GrayU8 gray = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
        GrayU8 edge = gray.createSameShape();
        CannyEdge<GrayU8, GrayS16> canny = FactoryEdgeDetectors.canny(blurRadius, true, true, GrayU8.class, GrayS16.class);
        canny.process(gray, threshLow, threshHigh, edge);
        //return VisualizeBinaryData.renderBinary(edge, false, null);
        return VisualizeImageData.grayMagnitude(edge, null, -1);
    }

    public static GrayU8 cannyEdge(GrayU8 gray, int blurRadius, float threshLow, float threshHigh) {
        GrayU8 edge = gray.createSameShape();
        CannyEdge<GrayU8, GrayS16> canny = FactoryEdgeDetectors.canny(blurRadius, true, true, GrayU8.class, GrayS16.class);
        canny.process(gray, threshLow, threshHigh, edge);
        return edge;
    }

    /**
     * Tuned for the white text on mostly black background of system map screenshots.
     */
    public static BufferedImage localSquareThresholdForSystemMap(BufferedImage originalImage) {
        GrayF32 grayImage = ConvertBufferedImage.convertFromSingle(originalImage, null, GrayF32.class);
        GrayU8 thresholdedImage = localSquareThresholdForSystemMap(grayImage);
        return VisualizeBinaryData.renderBinary(thresholdedImage, false, null);
    }

    public static GrayU8 localSquareThresholdForSystemMap(GrayF32 grayImage) {
        GrayU8 t20 = ScreenshotPreprocessor.localSquareThreshold(grayImage, 20, 0.5, false);
        GrayU8 t80 = ScreenshotPreprocessor.localSquareThreshold(grayImage, 540, 0.5, false);
        GrayU8 t160 = ScreenshotPreprocessor.localSquareThreshold(grayImage, 160, 0.5, false);
        GrayU8 tAll = t20.createSameShape();
        for (int y = 0; y < t20.height; y++) {
            for (int x = 0; x < t20.width; x++) {
                if (t20.unsafe_get(x, y) > 0 && t80.unsafe_get(x, y) > 0 && t160.unsafe_get(x, y) > 0) {
                    tAll.unsafe_set(x, y, t20.unsafe_get(x, y));
                } else {
                    tAll.unsafe_set(x, y, 0);
                }
            }
        }
        return tAll;
    }

    /**
     * Useful for keeping text only.
     *
     * @see #localSquareThresholdForSystemMap(BufferedImage)
     */
    public static GrayU8 localSquareThreshold(GrayF32 grayImage, int radius, double scale, boolean down) {
        return GThresholdImageOps.localSquare(grayImage, null, radius, scale, down, null, null);
    }

    public static BufferedImage highlightWhiteText(BufferedImage image) {
        //        BufferedImage whiteTextImage = keepWhiteTextOnly(image);
        //        BufferedImage cannyImage = cannyEdge(whiteTextImage);
        //
        //        return outlineText(whiteTextImage, cannyImage);
        BufferedImage whiteTextImage = keepWhiteTextOnly(image);
        return localSquareThresholdForSystemMap(whiteTextImage);
    }

    private static BufferedImage keepWhiteTextOnly(BufferedImage image) {
        //float hue = 0f; // angle in radians
        float saturation = 0f; // 0..1
        float value = 255f; // 0..255

        Planar<GrayF32> input = ConvertBufferedImage.convertFromMulti(image, null, true, GrayF32.class);
        Planar<GrayF32> hsv = input.createSameShape();
        ColorHsv.rgbToHsv_F32(input, hsv);
        //GrayF32 H = hsv.getBand(0);
        GrayF32 S = hsv.getBand(1);
        GrayF32 V = hsv.getBand(2);

        BufferedImage output = new BufferedImage(input.width, input.height, BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < hsv.height; y++) {
            for (int x = 0; x < hsv.width; x++) {
                float ds = Math.abs(S.unsafe_get(x, y) - saturation);
                float dv = Math.abs(V.unsafe_get(x, y) - value) / 255f;

                if (ds <= 0.25f && dv <= 0.35f) {
                    output.setRGB(x, y, image.getRGB(x, y));
                } else {
                    output.setRGB(x, y, Color.BLACK.getRGB());
                }
            }
        }

        return output;
    }

    private static BufferedImage outlineText(BufferedImage whiteTextImage, BufferedImage cannyImage) {
        final int outlineColor = new Color(64, 64, 64).getRGB();

        BufferedImage output = new BufferedImage(whiteTextImage.getWidth(), whiteTextImage.getHeight(), BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < whiteTextImage.getHeight(); y++) {
            for (int x = 0; x < whiteTextImage.getWidth(); x++) {
                int rgb = Color.BLACK.getRGB();
                if (cannyImage.getRGB(x, y) != Color.BLACK.getRGB()) {
                    rgb = outlineColor;
                }
                if (whiteTextImage.getRGB(x, y) != Color.BLACK.getRGB()) {
                    rgb = whiteTextImage.getRGB(x, y);
                }
                output.setRGB(x, y, rgb);
            }
        }
        return output;
    }

    public static BufferedImage keepInventoryTextOnly(BufferedImage croppedAndUndistortedImage) {
        //return fillCharacters(ScreenshotPreprocessor.cannyEdge(croppedAndUndistortedImage, 3, 0.1f, 0.3f));
        //return fillCharacters(ScreenshotPreprocessor.cannyEdge(croppedAndUndistortedImage, 2, 0.01f, 0.1f));
        GrayU8 grayU8 = ConvertBufferedImage.convertFromSingle(croppedAndUndistortedImage, null, GrayU8.class);
        GrayU8 cannyU8 = cannyEdge(grayU8, 2, 0.1f, 0.3f);
        BufferedImage cannyImage = VisualizeBinaryData.renderBinary(cannyU8, false, null);
        cannyU8 = ConvertBufferedImage.convertFromSingle(cannyImage, null, GrayU8.class);
        //GrayU8 gaussianU8 = BlurImageOps.gaussian(cannyU8, null, /* sigma = */ -1, /* radius = */ 1, null);
        //GrayU8 gaussianU8 = BlurImageOps.mean(cannyU8, null, 1, null);
        // median is really bad GrayU8 gaussianU8 = BlurImageOps.median(cannyU8, null, 2);
        //BufferedImage resultImage = VisualizeBinaryData.renderBinary(gaussianU8, false, null);
        //        BufferedImage resultImage = ConvertBufferedImage.convertTo(gaussianU8, null);
        //        return resultImage;

        GrayU8 thresholdU8 = GThresholdImageOps.localGaussian(cannyU8, null, 1, 0.5, false, null, null);
        return VisualizeBinaryData.renderBinary(thresholdU8, false, null);
    }

    private static BufferedImage fillCharacters(BufferedImage cannyImage) {
        BufferedImage filledImage = new BufferedImage(cannyImage.getWidth(), cannyImage.getHeight(), BufferedImage.TYPE_INT_RGB);

        Set<Point> gapPoints = new LinkedHashSet<>();
        Set<Point> grayOutlinePoints = new LinkedHashSet<>();
        for (int y = 0; y < cannyImage.getHeight(); y++) {
            for (int x = 0; x < cannyImage.getWidth(); x++) {
                int rgb = cannyImage.getRGB(x, y);
                if (rgb == 0xff000000) {
                    if (isGap(cannyImage, new Point(x, y))) {
                        gapPoints.add(new Point(x, y));
                    }
                    filledImage.setRGB(x, y, 0xff000000); // Keep black
                } else {
                    filledImage.setRGB(x, y, 0xff808080); // White to gray
                    grayOutlinePoints.add(new Point(x, y)); // Track gray outline pixels
                }
            }
        }
        for (Point gapPoint : gapPoints) {
            filledImage.setRGB(gapPoint.x, gapPoint.y, 0xff808080); // Close the gap by making it gray
        }

        Set<Point> almostBlackPoints = new LinkedHashSet<>();
        Set<Point> innerPoints = new LinkedHashSet<>();
        for (Point grayOutlinePoint : grayOutlinePoints) {
            LinkedList<Point> pureBlackNeighbourPixels = findNeighbourPixels(filledImage, grayOutlinePoint, 0xff000000);
            for (Point blackPoint : pureBlackNeighbourPixels) {
                int filledPixels = fillArea(filledImage, blackPoint, 0xffff0000); // Fill with pure white
                if (filledPixels >= 2500) { // 50x50
                    // Very large areas are background.
                    // Fill with ALMOST black to avoid processing them again and again.
                    fillArea(filledImage, blackPoint, 0xff010101);
                    almostBlackPoints.add(blackPoint);
                } else if (filledPixels <= 2) { // 1x2
                    // Very small areas are holes in the border.
                    // Do not count them as inner points.
                } else {
                    innerPoints.add(blackPoint); // Now filled white
                }
            }
        }

        for (Point almostBlackPoint : almostBlackPoints) {
            fillArea(filledImage, almostBlackPoint, 0xff000000);
        }
        for (Point innerPoint : innerPoints) {
            if (grayBorderAndWhiteCharInAllDirections(filledImage, innerPoint)) {
                fillArea(filledImage, innerPoint, 0xff00ff00);
            }
        }

        // At this point we have a very colorful image for debugging purpuses.
        // Now make it pure black and white.
        // red -> white
        // everything else -> black
        for (int y = 0; y < filledImage.getHeight(); y++) {
            for (int x = 0; x < filledImage.getWidth(); x++) {
                int rgb = filledImage.getRGB(x, y);
                if (rgb == 0xffff0000) {
                    filledImage.setRGB(x, y, 0xffffffff);
                } else {
                    filledImage.setRGB(x, y, 0xff000000);
                }
            }
        }

        return filledImage;
    }

    private static boolean grayBorderAndWhiteCharInAllDirections(BufferedImage filledImage, Point startPoint) {
        Point p = startPoint;
        boolean reachedBorder = false;

        // Go right
        p = startPoint;
        reachedBorder = false;
        while ((p = pointRight(p, filledImage)) != null) {
            int rgb = filledImage.getRGB(p.x, p.y);
            if (!reachedBorder) {
                reachedBorder = rgb == 0xff808080;
            } else {
                if (rgb == 0xff808080) {
                    // Still on border
                } else if (rgb == 0xffff0000) {
                    break; // White char in this direction
                } else {
                    return false; // Something else in this direction
                }
            }
        }
        if (p == null) {
            return false; // Reached end-of-image
        }

        // Go left
        p = startPoint;
        reachedBorder = false;
        while ((p = pointLeft(p, filledImage)) != null) {
            int rgb = filledImage.getRGB(p.x, p.y);
            if (!reachedBorder) {
                reachedBorder = rgb == 0xff808080;
            } else {
                if (rgb == 0xff808080) {
                    // Still on border
                } else if (rgb == 0xffff0000) {
                    break; // White char in this direction
                } else {
                    return false; // Something else in this direction
                }
            }
        }
        if (p == null) {
            return false; // Reached end-of-image
        }

        // Go up
        p = startPoint;
        reachedBorder = false;
        while ((p = pointAbove(p, filledImage)) != null) {
            int rgb = filledImage.getRGB(p.x, p.y);
            if (!reachedBorder) {
                reachedBorder = rgb == 0xff808080;
            } else {
                if (rgb == 0xff808080) {
                    // Still on border
                } else if (rgb == 0xffff0000) {
                    break; // White char in this direction
                } else {
                    return false; // Something else in this direction
                }
            }
        }
        if (p == null) {
            return false; // Reached end-of-image
        }

        // Go down
        p = startPoint;
        reachedBorder = false;
        while ((p = pointBelow(p, filledImage)) != null) {
            int rgb = filledImage.getRGB(p.x, p.y);
            if (!reachedBorder) {
                reachedBorder = rgb == 0xff808080;
            } else {
                if (rgb == 0xff808080) {
                    // Still on border
                } else if (rgb == 0xffff0000) {
                    break; // White char in this direction
                } else {
                    return false; // Something else in this direction
                }
            }
        }
        if (p == null) {
            return false; // Reached end-of-image
        }

        return true;
    }

    private static boolean isGap(BufferedImage image, Point p) {
        // -- V1 --
        //        final int x = p.x;
        //        final int y = p.y;
        //
        //        // Check above
        //        if (hasNonBlack(image, x - 1, y - 1, x, y - 1, x + 1, y - 1)) {
        //            // Check below
        //            if (hasNonBlack(image, x - 1, y + 1, x, y + 1, x + 1, y + 1)) {
        //                return true;
        //            }
        //        }
        //
        //        // Check right
        //        if (hasNonBlack(image, x + 1, y - 1, x + 1, y, x + 1, y + 1)) {
        //            // Check left
        //            if (hasNonBlack(image, x - 1, y - 1, x - 1, y, x - 1, y + 1)) {
        //                return true;
        //            }
        //        }

        // -- V2 --
        //        for (int y1 = p.y - 1; y1 <= p.y + 1; y1++) {
        //            for (int x1 = p.x - 1; x1 <= p.x + 1; x1++) {
        //                if (insideImage(x1, y1, image) && image.getRGB(x1, y1) != 0xff000000) {
        //                    // Found one non-black pixel
        //                    for (int y2 = p.y - 1; y2 <= p.y + 1; y2++) {
        //                        for (int x2 = p.x - 1; x2 <= p.x + 1; x2++) {
        //                            if (!(x2 == x1 && y2 == y1) && insideImage(x2, y2, image) && image.getRGB(x2, y2) != 0xff000000) {
        //                                // Found another non-black pixel
        //                                double distanceSq = Point.distanceSq(x1, y1, x2, y2);
        //                                if (distanceSq > 1.0 && distanceSq < 92.5) { // TODO 92.5?!?
        //                                    return true;
        //                                }
        //                            }
        //                        }
        //                    }
        //                }
        //            }
        //        }

        // -- V3 --
        List<Point> nonBlackPoints = new ArrayList<>();
        for (int y1 = p.y - 1; y1 <= p.y + 1; y1++) {
            for (int x1 = p.x - 1; x1 <= p.x + 1; x1++) {
                if (insideImage(x1, y1, image) && image.getRGB(x1, y1) != 0xff000000) {
                    nonBlackPoints.add(new Point(x1, y1));
                }
            }
        }
        if (nonBlackPoints.size() == 2) {
            Point p1 = nonBlackPoints.get(0);
            Point p2 = nonBlackPoints.get(1);
            if (Point.distanceSq(p1.x, p1.y, p2.x, p2.y) > 2) {
                return true;
            }
        }

        return false;
    }

    private static Point pointAbove(Point current, BufferedImage image) {
        Point other = new Point(current.x, current.y - 1);
        if (insideImage(other, image)) {
            return other;
        } else {
            return null;
        }
    }

    private static Point pointRight(Point current, BufferedImage image) {
        Point other = new Point(current.x + 1, current.y);
        if (insideImage(other, image)) {
            return other;
        } else {
            return null;
        }
    }

    private static Point pointBelow(Point current, BufferedImage image) {
        Point other = new Point(current.x, current.y + 1);
        if (insideImage(other, image)) {
            return other;
        } else {
            return null;
        }
    }

    private static Point pointLeft(Point current, BufferedImage image) {
        Point other = new Point(current.x - 1, current.y);
        if (insideImage(other, image)) {
            return other;
        } else {
            return null;
        }
    }

    private static boolean insideImage(Point p, BufferedImage i) {
        return insideImage(p.x, p.y, i);
    }

    private static boolean insideImage(int x, int y, BufferedImage i) {
        return x >= 0 && y >= 0 && x < i.getWidth() && y < i.getHeight();
    }

    private static LinkedList<Point> findNeighbourPixels(BufferedImage image, Point p, int rgb) {
        LinkedList<Point> result = new LinkedList<>();
        Point above = pointAbove(p, image);
        if (above != null && image.getRGB(above.x, above.y) == rgb) {
            result.add(above);
        }
        Point right = pointRight(p, image);
        if (right != null && image.getRGB(right.x, right.y) == rgb) {
            result.add(right);
        }
        Point below = pointBelow(p, image);
        if (below != null && image.getRGB(below.x, below.y) == rgb) {
            result.add(below);
        }
        Point left = pointLeft(p, image);
        if (left != null && image.getRGB(left.x, left.y) == rgb) {
            result.add(left);
        }
        return result;
    }

    private static int fillArea(BufferedImage image, Point startPoint, int fillColor) {
        int replacedColor = image.getRGB(startPoint.x, startPoint.y);
        image.setRGB(startPoint.x, startPoint.y, fillColor);
        int filledPixels = 1;

        if (replacedColor == fillColor) {
            return 0;
        }

        LinkedList<Point> open = findNeighbourPixels(image, startPoint, replacedColor);
        while (!open.isEmpty()) {
            //            System.out.println(open.size());
            Point nextPoint = open.pop();
            image.setRGB(nextPoint.x, nextPoint.y, fillColor);
            filledPixels++;
            LinkedList<Point> nextNeighbours = findNeighbourPixels(image, nextPoint, replacedColor);
            for (Point nextNeighbour : nextNeighbours) {
                if (!open.contains(nextNeighbour)) {
                    open.add(nextNeighbour);
                }
            }
        }

        return filledPixels;
    }

    private static boolean areaContainsSquare(BufferedImage image, Point startPoint, int squareSize) {
        // Collect all area pixels
        int areaColor = image.getRGB(startPoint.x, startPoint.y);
        LinkedList<Point> closed = new LinkedList<>();
        closed.add(startPoint);
        LinkedList<Point> open = findNeighbourPixels(image, startPoint, areaColor);
        while (!open.isEmpty()) {
            Point nextPoint = open.pop();
            closed.add(nextPoint);
            LinkedList<Point> nextNeighbours = findNeighbourPixels(image, nextPoint, areaColor);
            for (Point nextNeighbour : nextNeighbours) {
                if (!open.contains(nextNeighbour) && !closed.contains(nextNeighbour)) {
                    open.add(nextNeighbour);
                }
            }
        }

        // Check each place of the area
        for (Point areaPoint : closed) {
            try {
                BufferedImage squareImage = image.getSubimage(areaPoint.x, areaPoint.y, squareSize, squareSize);
                boolean fitsSquare = true;
                for (int y = 0; y < squareSize; y++) {
                    for (int x = 0; x < squareSize; x++) {
                        if (squareImage.getRGB(x, y) != areaColor) {
                            fitsSquare = false;
                            break;
                        }
                    }
                    if (!fitsSquare) {
                        break;
                    }
                }
                if (fitsSquare) {
                    return true;
                }
            } catch (RasterFormatException e) {
                // Ignore
            }
        }

        return false;
    }

}
