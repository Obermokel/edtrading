package borg.edtrading;

import boofcv.alg.distort.RemovePerspectiveDistortion;
import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayS16;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageType;
import boofcv.struct.image.Planar;
import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import borg.edtrading.util.ImageUtil;
import borg.edtrading.util.MiscUtil;
import georegression.struct.point.Point2D_F64;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.image.RasterFormatException;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

/**
 * CraftingApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class CraftingApp {

    static final Logger logger = LogManager.getLogger(CraftingApp.class);

    public static void main(String[] args) throws IOException {
        List<Template> templates = TemplateMatcher.loadTemplates("Inventory");

        scanNewInventoryScreenshots(templates);
    }

    private static void scanNewInventoryScreenshots(List<Template> templates) throws IOException {
        List<Template> alphanumTemplates = templates.stream().filter(t -> t.getText().matches("\\w")).collect(Collectors.toList());
        List<File> unscannedScreenshotFiles = getUnscannedScreenshotFiles();
        for (File unscannedScreenshotFile : unscannedScreenshotFiles) {
            // TODO Remove date constraint
            //if (!unscannedScreenshotFile.getName().startsWith("elitedangerous64_2016-07-")) {
            if (!unscannedScreenshotFile.getName().startsWith("elitedangerous64_2016-07-13_08-52")) {
                continue;
            }

            BufferedImage originalImage = ImageIO.read(unscannedScreenshotFile);
            BufferedImage fourKImage = ImageUtil.toFourK(originalImage); // Scale up to 4K to make canny edge detection easier
            BufferedImage correctedImage = correctDistort(fourKImage); // Remove perspective distortion
            BufferedImage croppedImage = correctedImage.getSubimage(135, 0, 1280, 720); // Crop to inventory
            BufferedImage cannyImage = cannyEdge(croppedImage); // Mark edges
            BufferedImage filledImage = fillCharacters(cannyImage); // Fill characters

            //            List<TemplateMatch> alphanumMatches = findTemplateMatches(cannyImage, alphanumTemplates);
            //            if (alphanumMatches.size() >= 20) {
            //                List<MatchGroup> matchGroups = MatchSorter.sortMatches(alphanumMatches);
            //
            //                BufferedImage out = new BufferedImage(cannyImage.getWidth(), cannyImage.getHeight(), BufferedImage.TYPE_INT_RGB);
            //                Graphics2D g = out.createGraphics();
            //                g.drawImage(cannyImage, 0, 0, null);
            //                g.setColor(Color.YELLOW);
            //                g.setFont(new Font("Arial", Font.PLAIN, 12));
            //                for (TemplateMatch m : alphanumMatches) {
            //                    g.drawString(m.getTemplate().getText(), m.getMatch().x, m.getMatch().y);
            //                }
            //                g.setColor(Color.RED);
            //                g.setFont(new Font("Arial", Font.BOLD, 16));
            //                for (MatchGroup mg : matchGroups) {
            //                    g.drawString(mg.getText(), mg.getX(), mg.getY());
            //                }
            //                ImageIO.write(out, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_matches.png")));
            //                g.dispose();
            //            }

            //            ImageIO.write(fourKImage, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_0fourK.png")));
            //            ImageIO.write(correctedImage, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_1corrected.png")));
            //            ImageIO.write(croppedImage, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_2cropped.png")));
            //            ImageIO.write(cannyImage, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_3canny.png")));
            ImageIO.write(filledImage, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_4filled.png")));

            //            BufferedImage firstRow = fullHdInventory.getSubimage(18, 18, 520, 70);
            //            ImageIO.write(firstRow, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_1stRow.png")));
            //            BufferedImage secondRow = fullHdInventory.getSubimage(16, 90, 520, 70);
            //            ImageIO.write(secondRow, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_2ndRow.png")));
            //            BufferedImage thirdRow = fullHdInventory.getSubimage(14, 160, 520, 70);
            //            ImageIO.write(thirdRow, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_3rdRow.png")));
            //            BufferedImage fourthRow = fullHdInventory.getSubimage(10, 234, 520, 70);
            //            ImageIO.write(fourthRow, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_4thRow.png")));
            //            BufferedImage fifthRow = fullHdInventory.getSubimage(8, 304, 520, 70);
            //            ImageIO.write(fifthRow, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_5thRow.png")));

            //            TemplateMatch match = isInventoryScreenshot(inventoryImage, inventoryTemplates);
            //            if (match != null) {
            //                logger.info(unscannedScreenshotFile + ": epp=" + match.getErrorPerPixel() + " / q=" + match.getMatchQuality());
            //            }
        }
    }

    private static BufferedImage cannyEdge(BufferedImage originalImage) {
        GrayU8 gray = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
        GrayU8 edge = gray.createSameShape();
        CannyEdge<GrayU8, GrayS16> canny = FactoryEdgeDetectors.canny(3, true, true, GrayU8.class, GrayS16.class);
        canny.process(gray, 0.1f, 0.3f, edge);
        return VisualizeBinaryData.renderBinary(edge, false, null);
    }

    private static BufferedImage correctDistort(BufferedImage fourKImage) {
        Planar<GrayF32> input = ConvertBufferedImage.convertFromMulti(fourKImage, null, true, GrayF32.class);

        RemovePerspectiveDistortion<Planar<GrayF32>> removePerspective = new RemovePerspectiveDistortion<Planar<GrayF32>>(1600, 800, ImageType.pl(3, GrayF32.class));

        // Specify the corners in the input image of the region.
        // Order matters! top-left, top-right, bottom-right, bottom-left
        //@formatter:off
        if (!removePerspective.apply(input,
                new Point2D_F64(1702,779),
                new Point2D_F64(3400,871),
                new Point2D_F64(3311,1688),
                new Point2D_F64(1678,1502))) {
            throw new RuntimeException("Failed!?!?");
        }
        //@formatter:on

        Planar<GrayF32> output = removePerspective.getOutput();

        BufferedImage flat = ConvertBufferedImage.convertTo_F32(output, null, true);
        return flat;
    }

    private static BufferedImage fillCharacters(BufferedImage originalImage) {
        BufferedImage filledImage = new BufferedImage(originalImage.getWidth(), originalImage.getHeight(), BufferedImage.TYPE_INT_RGB);

        Set<Point> gapPoints = new LinkedHashSet<>();
        Set<Point> grayOutlinePoints = new LinkedHashSet<>();
        for (int y = 0; y < originalImage.getHeight(); y++) {
            for (int x = 0; x < originalImage.getWidth(); x++) {
                int rgb = originalImage.getRGB(x, y);
                if (rgb == 0xff000000) {
                    if (isGap(originalImage, new Point(x, y))) {
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

        for (Point grayOutlinePoint : grayOutlinePoints) {
            LinkedList<Point> pureBlackNeighbourPixels = findNeighbourPixels(filledImage, grayOutlinePoint, 0xff000000);
            for (Point blackPoint : pureBlackNeighbourPixels) {
                int filledPixels = fillArea(filledImage, blackPoint, 0xffffffff); // Fill with pure white
                if (filledPixels >= 1600) { // 40x40
                    // Very large areas are background.
                    // Fill with ALMOST black to avoid processing them again and again.
                    fillArea(filledImage, blackPoint, 0xff010101);
                } else {
                    // Small areas could also be inside closed chars like D or O.
                    // Simple test: A "large" 5x5 square should only fit inside chars, but not inside thin outlines.
                    if (areaContainsSquare(filledImage, blackPoint, 5)) {
                        fillArea(filledImage, blackPoint, 0xff00ff00);
                    }
                }
            }
        }

        return filledImage;
    }

    //    private static boolean isGap(BufferedImage image, Point p) {
    //        Point above = pointAbove(p, image);
    //        Point below = pointBelow(p, image);
    //
    //        if (above != null && below != null) {
    //            if (image.getRGB(above.x, above.y) != 0xff000000 && image.getRGB(below.x, below.y) != 0xff000000) {
    //                return true;
    //            }
    //        }
    //
    //        Point right = pointRight(p, image);
    //        Point left = pointLeft(p, image);
    //
    //        if (right != null && left != null) {
    //            if (image.getRGB(right.x, right.y) != 0xff000000 && image.getRGB(left.x, left.y) != 0xff000000) {
    //                return true;
    //            }
    //        }
    //
    //        return false;
    //    }

    private static boolean isGap(BufferedImage image, Point p) {
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

        for (int y1 = p.y - 1; y1 <= p.y + 1; y1++) {
            for (int x1 = p.x - 1; x1 <= p.x + 1; x1++) {
                if (insideImage(x1, y1, image) && image.getRGB(x1, y1) != 0xff000000) {
                    // Found one non-black pixel
                    for (int y2 = p.y - 1; y2 <= p.y + 1; y2++) {
                        for (int x2 = p.x - 1; x2 <= p.x + 1; x2++) {
                            if (!(x2 == x1 && y2 == y1) && insideImage(x2, y2, image) && image.getRGB(x2, y2) != 0xff000000) {
                                // Found another non-black pixel
                                if (Point.distanceSq(x1, y1, x2, y2) > 1.0 && Point.distanceSq(x1, y1, x2, y2) < 2.5) {
                                    System.out.println(Point.distanceSq(x1, y1, x2, y2));
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }

        return false;
    }

    private static boolean hasNonBlack(BufferedImage image, int x1, int y1, int x2, int y2, int x3, int y3) {
        if (insideImage(x1, y1, image) && image.getRGB(x1, y1) != 0xff000000) {
            return true;
        }
        if (insideImage(x2, y2, image) && image.getRGB(x2, y2) != 0xff000000) {
            return true;
        }
        if (insideImage(x3, y3, image) && image.getRGB(x3, y3) != 0xff000000) {
            return true;
        }

        return false;
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

    private static List<TemplateMatch> findTemplateMatches(BufferedImage cannyInventory, List<Template> templates) {
        // Find all matches
        List<TemplateMatch> matches = new ArrayList<>();
        for (Template t : templates) {
            matches.addAll(TemplateMatcher.findTemplateMatches(cannyInventory, t, /* maxMatches = */ 50));
        }

        // Sort by score
        Collections.sort(matches, new Comparator<TemplateMatch>() {
            @Override
            public int compare(TemplateMatch m1, TemplateMatch m2) {
                // Real score
                //return new Double(-1 * m1.getMatch().score).compareTo(new Double(-1 * m2.getMatch().score));

                // Unprecise score/pixel
                //return new Integer(m1.getMatchQuality()).compareTo(new Integer(m2.getMatchQuality()));

                // Precise score/pixel
                return new Double(-1 * m1.getMatch().score / (m1.getTemplate().getImage().width * m1.getTemplate().getImage().height))
                        .compareTo(new Double(-1 * m2.getMatch().score / (m2.getTemplate().getImage().width * m2.getTemplate().getImage().height)));
            }
        });

        // Select all non-overlapping with a good quality
        final double goodQuality = 10000;
        List<TemplateMatch> nonOverlapping = new ArrayList<>();
        for (TemplateMatch m : matches) {
            if ((-1 * m.getMatch().score / (m.getTemplate().getImage().width * m.getTemplate().getImage().height)) < goodQuality) {
                if (!m.overlapsWithAny(nonOverlapping)) {
                    nonOverlapping.add(m);
                }
            }
        }

        // Finished
        return nonOverlapping;
    }

    private static Date getLastScannedInventoryScreenshotDate() {
        File lastScannedInventoryScreenshotDateFile = new File(Constants.EDTRADING_BASE_DIR, "ED_lastScannedInventoryScreenshotDate.txt");
        if (!lastScannedInventoryScreenshotDateFile.exists()) {
            return new Date(0);
        } else {
            try {
                return MiscUtil.getAsDate(FileUtils.readFileToString(lastScannedInventoryScreenshotDateFile), new Date(0));
            } catch (IOException e) {
                throw new RuntimeException("Failed to read " + lastScannedInventoryScreenshotDateFile, e);
            }
        }
    }

    private static void setLastScannedInventoryScreenshotDate(Date date) {
        File lastScannedInventoryScreenshotDateFile = new File(Constants.EDTRADING_BASE_DIR, "ED_lastScannedInventoryScreenshotDate.txt");
        if (date == null && lastScannedInventoryScreenshotDateFile.exists()) {
            lastScannedInventoryScreenshotDateFile.delete();
        } else {
            try {
                FileUtils.write(lastScannedInventoryScreenshotDateFile, new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date), false);
            } catch (IOException e) {
                throw new RuntimeException("Failed to write " + lastScannedInventoryScreenshotDateFile, e);
            }
        }
    }

    /**
     * Get all screenshot files that are newer than the last successfully scanned inventory screenshot.
     * This will include screenshots which do not show the inventory tab.
     *
     * @return Sorted ascending by date
     */
    private static List<File> getUnscannedScreenshotFiles() {
        final Date lastScannedScreenshotDate = CraftingApp.getLastScannedInventoryScreenshotDate();
        File[] screenshotFiles = Constants.SCREENSHOTS_DIR.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().startsWith("elitedangerous64_") && file.getName().endsWith(".png") && file.lastModified() > lastScannedScreenshotDate.getTime();
            }
        });
        List<File> sortableList = new ArrayList<>(Arrays.asList(screenshotFiles));
        Collections.sort(sortableList, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                return new Long(f1.lastModified()).compareTo(new Long(f2.lastModified()));
            }
        });
        return sortableList;
    }

}
