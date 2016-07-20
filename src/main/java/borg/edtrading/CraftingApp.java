package borg.edtrading;

import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayS16;
import boofcv.struct.image.GrayU8;
import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import borg.edtrading.util.ImageUtil;
import borg.edtrading.util.InventoryUtil;
import borg.edtrading.util.MatchSorter;
import borg.edtrading.util.MatchSorter.MatchGroup;
import borg.edtrading.util.MiscUtil;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
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
        List<Template> templates = TemplateMatcher.loadTemplates();

        scanNewInventoryScreenshots(templates);
    }

    private static void scanNewInventoryScreenshots(List<Template> templates) throws IOException {
        List<Template> alphanumTemplates = templates.stream().filter(t -> t.getText().matches("\\w")).collect(Collectors.toList());
        List<File> unscannedScreenshotFiles = getUnscannedScreenshotFiles();
        for (File unscannedScreenshotFile : unscannedScreenshotFiles) {
            // TODO Remove date constraint
            if (!unscannedScreenshotFile.getName().startsWith("elitedangerous64_2016-07-")) {
                continue;
            }

            BufferedImage originalImage = ImageIO.read(unscannedScreenshotFile);
            BufferedImage fourKImage = ImageUtil.toFourK(originalImage); // Scale up to 4K to make canny edge detection easier
            BufferedImage inventoryImage = InventoryUtil.fourKToInventory(fourKImage); // Crop inventory region
            BufferedImage cannyInventory = cannyEdge(inventoryImage);
            BufferedImage fullHdInventory = ImageUtil.fourKToFullHd(cannyInventory); // Scale down to full HD to make template matching easier

            List<TemplateMatch> alphanumMatches = findTemplateMatches(fullHdInventory, alphanumTemplates);
            if (alphanumMatches.size() >= 20) {
                List<MatchGroup> matchGroups = MatchSorter.sortMatches(alphanumMatches);

                BufferedImage out = new BufferedImage(fullHdInventory.getWidth(), fullHdInventory.getHeight(), BufferedImage.TYPE_INT_RGB);
                Graphics2D g = out.createGraphics();
                g.drawImage(fullHdInventory, 0, 0, null);
                g.setColor(Color.YELLOW);
                g.setFont(new Font("Arial", Font.PLAIN, 12));
                for (TemplateMatch m : alphanumMatches) {
                    g.drawString(m.getTemplate().getText(), m.getMatch().x, m.getMatch().y);
                }
                g.setColor(Color.RED);
                g.setFont(new Font("Arial", Font.BOLD, 16));
                for (MatchGroup mg : matchGroups) {
                    g.drawString(mg.getText(), mg.getX(), mg.getY());
                }
                ImageIO.write(out, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_matches.png")));
                g.dispose();
            }

            //            ImageIO.write(fullHdInventory, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_0Inventory.png")));
            //
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

    private static BufferedImage correctDistort(BufferedImage originalImage) {
        GrayU8 gray = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
        GrayU8 edge = gray.createSameShape();
        CannyEdge<GrayU8, GrayS16> canny = FactoryEdgeDetectors.canny(3, true, true, GrayU8.class, GrayS16.class);
        canny.process(gray, 0.1f, 0.3f, edge);
        return VisualizeBinaryData.renderBinary(edge, false, null);
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
        final double goodQuality = 4000;
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
