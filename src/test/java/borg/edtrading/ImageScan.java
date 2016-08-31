package borg.edtrading;

import boofcv.gui.ListDisplayPanel;
import boofcv.gui.image.ShowImages;
import boofcv.io.image.UtilImageIO;
import borg.edtrading.boofcv.ScreenshotScanner;
import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.List;
import java.util.Map;

/**
 * ImageScan
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ImageScan {

    static final Logger logger = LogManager.getLogger(ImageScan.class);

    static final ListDisplayPanel GUI = new ListDisplayPanel();

    public static void main(String[] args) {
        //ScreenshotScanner.scanScreenshot(new File(Constants.SCREENSHOTS_DIR, "elitedangerous64_2016-02-09_21-34-33.png"));

        //String imageName = "test.png";
        //String imageName = "elitedangerous64_2016-02-09_21-34-33.png";
        //String imageName = "elitedangerous64_2016-02-09_21-47-18.png";
        //String imageName = "elitedangerous64_2016-02-09_21-47-41.png";
        String imageName = "elitedangerous64_2016-02-09_22-01-21.png";
        File imageFile = new File(Constants.SCREENSHOTS_DIR, imageName);
        BufferedImage image = UtilImageIO.loadImage(imageFile.getAbsolutePath());
        List<Template> templates = TemplateMatcher.loadTemplates("Commodities");
        GUI.addImage(image, "image");
        BufferedImage orangeTextImage = ScreenshotScanner.keepOrangeTextOnly(image);
        GUI.addImage(orangeTextImage, "orangeTextImage");
        BufferedImage croppedImage = ScreenshotScanner.cropToCommoditiesMarket(orangeTextImage, templates);
        GUI.addImage(croppedImage, "croppedImage");
        Map<String, Integer> columns = ScreenshotScanner.findColumns(croppedImage, templates);
        List<Integer> rows = ScreenshotScanner.findRows(croppedImage, templates, columns);
        BufferedImage colsAndRowsImage = drawColsAndRows(croppedImage, columns, rows);
        GUI.addImage(colsAndRowsImage, "colsAndRowsImage");
        BufferedImage noHairlineImage = ScreenshotScanner.removeHairlines(croppedImage, rows, columns);
        GUI.addImage(noHairlineImage, "noHairlineImage");
        Map<Integer, Map<String, List<TemplateMatch>>> groupedMatches = ScreenshotScanner.findAndGroupMatches(noHairlineImage, templates, rows, columns);
        BufferedImage matchesImage = drawMatches(noHairlineImage, groupedMatches, columns, rows);
        GUI.addImage(matchesImage, "matchesImage");
        Map<Integer, Map<String, String>> texts = ScreenshotScanner.extractTexts(groupedMatches);
        for (Map<String, String> row : texts.values()) {
            String ware = row.get("WAREN");
            String verkauf = row.get("VERKAUF");
            String einkauf = row.get("EINKAUF");
            String fracht = row.get("FRACHT");
            String nachfrage = row.get("NACHFRAGE");
            String auflager = row.get("AUFLAGER");
            String durchschnitt = row.get("DURCHSCHNITT");
            System.out.println(String.format("%30s %10s %10s %10s %10s %10s %10s", ware, verkauf, einkauf, fracht, nachfrage, auflager, durchschnitt));
        }

        ShowImages.showWindow(GUI, imageName);
    }

    private static BufferedImage copyImage(BufferedImage image) {
        BufferedImage copy = new BufferedImage(image.getWidth(), image.getHeight(), image.getType());
        Graphics2D g = copy.createGraphics();
        g.drawImage(image, 0, 0, image.getWidth(), image.getHeight(), null);
        g.dispose();
        return copy;
    }

    private static BufferedImage drawColsAndRows(BufferedImage image, Map<String, Integer> columns, List<Integer> rows) {
        BufferedImage copy = copyImage(image);
        Graphics2D g = copy.createGraphics();
        g.setColor(Color.BLUE);
        for (String text : columns.keySet()) {
            int x = columns.get(text);
            g.drawLine(x, 0, x, copy.getHeight());
            g.drawString(text, x, 20);
        }
        for (Integer y : rows) {
            g.drawLine(0, y, copy.getWidth(), y);
        }
        g.dispose();
        return copy;
    }

    private static BufferedImage drawMatches(BufferedImage image, Map<Integer, Map<String, List<TemplateMatch>>> groupedMatches, Map<String, Integer> columns, List<Integer> rows) {
        BufferedImage copy = copyImage(image);
        Graphics2D g = copy.createGraphics();
        g.setColor(Color.BLUE);
        g.setFont(new Font("Consolas", Font.PLAIN, 20));
        for (Integer rowNum : groupedMatches.keySet()) {
            Map<String, List<TemplateMatch>> rowMatches = groupedMatches.get(rowNum);
            for (String columnName : rowMatches.keySet()) {
                List<TemplateMatch> columnMatches = rowMatches.get(columnName);
                for (TemplateMatch tm : columnMatches) {
                    //int colX = columns.get(columnName);
                    int rowY = rows.get(rowNum);
                    g.drawString(tm.getTemplate().getText(), tm.getMatch().x, tm.getMatch().y + rowY);
                }
            }
        }
        g.dispose();
        return copy;
    }

}
