package borg.edtrading;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.List;

import boofcv.alg.color.ColorHsv;
import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.alg.feature.detect.edge.EdgeContour;
import boofcv.alg.feature.detect.template.TemplateMatching;
import boofcv.alg.filter.binary.BinaryImageOps;
import boofcv.alg.filter.binary.Contour;
import boofcv.alg.filter.binary.GThresholdImageOps;
import boofcv.alg.misc.ImageStatistics;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.factory.feature.detect.template.FactoryTemplateMatching;
import boofcv.factory.feature.detect.template.TemplateScoreType;
import boofcv.gui.ListDisplayPanel;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.gui.image.ImagePanel;
import boofcv.gui.image.ShowImages;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.io.image.UtilImageIO;
import boofcv.struct.ConnectRule;
import boofcv.struct.feature.Match;
import boofcv.struct.image.ImageFloat32;
import boofcv.struct.image.ImageSInt16;
import boofcv.struct.image.ImageUInt8;
import boofcv.struct.image.MultiSpectral;
import borg.edtrading.boofcv.ScreenshotScanner;
import georegression.metric.UtilAngle;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * ImageScan
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ImageScan {

    static final Logger logger = LogManager.getLogger(ImageScan.class);

    private static final File BASE_DIR = new File("X:\\Game Screenshots\\elitedangerous64");

    private static final ListDisplayPanel GUI = new ListDisplayPanel();

    public static void main(String[] args) {
        //ScreenshotScanner.scanScreenshot(new File("C:\\Users\\Guenther\\Pictures\\elitedangerous64\\test.png"));

        String imageName = "threshold_test.png";
        File imageFile = new File(BASE_DIR, imageName);
        BufferedImage image = UtilImageIO.loadImage(imageFile.getAbsolutePath());
        GUI.addImage(image, "ORIGINAL");
        //colorSegmentClicker(image);
        GUI.addImage(ScreenshotScanner.keepOrangeTextOnly(image), "Orange Text");
        //colorSegment(image, (float) Math.toRadians(25), 0.91f, 252f);
        ShowImages.showWindow(GUI, imageName);
    }

    /**
     * Shows a color image and allows the user to select a pixel, convert it to HSV, print the HSV values, and calls the function below to display similar pixels.
     */
    public static void colorSegmentClicker(BufferedImage image) {
        ImagePanel gui = new ImagePanel(image);
        gui.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                float[] color = new float[3];
                int rgb = image.getRGB(e.getX(), e.getY());
                ColorHsv.rgbToHsv((rgb >> 16) & 0xFF, (rgb >> 8) & 0xFF, rgb & 0xFF, color);
                System.out.println("H = " + color[0] + " S = " + color[1] + " V = " + color[2]);

                colorSegment(image, color[0], color[1], color[2]);
            }
        });

        ShowImages.showWindow(gui, "Color Selector");
    }

    static void colorSegment(BufferedImage image, float hue, float saturation, float value) {
        //        orange text: H = 0.4000025 S = 0.966805 V = 241.0
        MultiSpectral<ImageFloat32> input = ConvertBufferedImage.convertFromMulti(image, null, true, ImageFloat32.class);
        MultiSpectral<ImageFloat32> hsv = input.createSameShape();

        // Convert into HSV
        ColorHsv.rgbToHsv_F32(input, hsv);

        // Euclidean distance squared threshold for deciding which pixels are members of the selected set
        float maxDist2 = 0.5f * 0.5f;

        // Extract hue and saturation bands which are independent of intensity
        ImageFloat32 H = hsv.getBand(0);
        ImageFloat32 S = hsv.getBand(1);
        ImageFloat32 V = hsv.getBand(2);

        // step through each pixel and mark how close it is to the selected color
        BufferedImage inputHue = new BufferedImage(input.width, input.height, BufferedImage.TYPE_INT_RGB);
        BufferedImage inputSaturation = new BufferedImage(input.width, input.height, BufferedImage.TYPE_INT_RGB);
        BufferedImage inputValue = new BufferedImage(input.width, input.height, BufferedImage.TYPE_INT_RGB);
        BufferedImage output = new BufferedImage(input.width, input.height, BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < hsv.height; y++) {
            for (int x = 0; x < hsv.width; x++) {
                int h = (int) (H.unsafe_get(x, y) * 255 / (Math.PI * 2));
                inputHue.setRGB(x, y, new Color(h, h, h).getRGB());
                int s = (int) (S.unsafe_get(x, y) * 255);
                inputSaturation.setRGB(x, y, new Color(s, s, s).getRGB());
                int v = (int) (V.unsafe_get(x, y));
                inputValue.setRGB(x, y, new Color(v, v, v).getRGB());

                // Hue is an angle in radians, so simple subtraction doesn't work
                float dh = UtilAngle.dist(H.unsafe_get(x, y), hue) / (float) Math.PI;
                float ds = (S.unsafe_get(x, y) - saturation);
                float dv = (V.unsafe_get(x, y) - value) / 255f;

                // this distance measure is a bit naive, but good enough for to demonstrate the concept
                float dist2 = dh * dh + ds * ds + dv * dv;
                if (dist2 <= maxDist2) {
                    output.setRGB(x, y, image.getRGB(x, y));
                }
            }
        }

        GUI.addImage(inputHue, "Hue");
        GUI.addImage(inputSaturation, "Saturation");
        GUI.addImage(inputValue, "Value");
        GUI.addImage(output, "Color segmented");
    }

    static void threshold() {
        String imageName = "elitedangerous64_2016-02-09_21-34-33.png";
        File imageFile = new File(BASE_DIR, imageName);
        BufferedImage image = UtilImageIO.loadImage(imageFile.getAbsolutePath());

        // convert into a usable format
        ImageFloat32 input = ConvertBufferedImage.convertFromSingle(image, null, ImageFloat32.class);
        ImageUInt8 binary = new ImageUInt8(input.width, input.height);

        // Global Methods
        GThresholdImageOps.threshold(input, binary, ImageStatistics.mean(input), true);
        GUI.addImage(VisualizeBinaryData.renderBinary(binary, false, null), "Global: Mean");
        GThresholdImageOps.threshold(input, binary, GThresholdImageOps.computeOtsu(input, 0, 255), true);
        GUI.addImage(VisualizeBinaryData.renderBinary(binary, false, null), "Global: Otsu");
        GThresholdImageOps.threshold(input, binary, GThresholdImageOps.computeEntropy(input, 0, 255), true);
        GUI.addImage(VisualizeBinaryData.renderBinary(binary, false, null), "Global: Entropy");

        // Local method
        GThresholdImageOps.localSquare(input, binary, 28, 1.0, true, null, null);
        GUI.addImage(VisualizeBinaryData.renderBinary(binary, false, null), "Local: Square");
        GThresholdImageOps.localBlockMinMax(input, binary, 10, 1.0, true, 15);
        GUI.addImage(VisualizeBinaryData.renderBinary(binary, false, null), "Local: Block Min-Max");
        GThresholdImageOps.localGaussian(input, binary, 42, 1.0, true, null, null);
        GUI.addImage(VisualizeBinaryData.renderBinary(binary, false, null), "Local: Gaussian");
        GThresholdImageOps.localSauvola(input, binary, 5, 0.30f, true);
        GUI.addImage(VisualizeBinaryData.renderBinary(binary, false, null), "Local: Sauvola");

        // Sauvola is tuned for text image.  Change radius to make it run better in others.

        // Show the image image for reference
        GUI.addImage(ConvertBufferedImage.convertTo(input, null), "Input Image");
    }

    static void templateMatch() {
        //test.png
        //elitedangerous64_2016-02-08_18-47-15.jpg

        ImageFloat32 image = UtilImageIO.loadImage("C:\\Users\\Guenther\\Pictures\\elitedangerous64\\test.png", ImageFloat32.class);
        ImageFloat32 templateCursor = UtilImageIO.loadImage("C:\\Users\\Guenther\\Pictures\\elitedangerous64\\u.png", ImageFloat32.class);
        //        ImageFloat32 maskCursor = UtilImageIO.loadImage(directory , "cursor_mask.png", ImageFloat32.class);
        //        ImageFloat32 templatePaint = UtilImageIO.loadImage(directory , "paint.png", ImageFloat32.class);

        // create output image to show results
        BufferedImage output = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_BGR);
        ConvertBufferedImage.convertTo(image, output);
        Graphics2D g2 = output.createGraphics();

        //        // Search for the cursor in the image.  For demonstration purposes it has been pasted 3 times
        //        g2.setColor(Color.RED); g2.setStroke(new BasicStroke(5));
        //        drawRectangles(g2, image, templateCursor, maskCursor, 3);
        //        // show match intensity image for this template
        //        showMatchIntensity(image, templateCursor, maskCursor);

        // Now it's try finding the cursor without a mask.  it will get confused when the background is black
        g2.setColor(Color.BLUE);
        g2.setStroke(new BasicStroke(2));
        drawRectangles(g2, image, templateCursor, null, 20);

        //        // Now it searches for a specific icon for which there is only one match
        //        g2.setColor(Color.ORANGE); g2.setStroke(new BasicStroke(3));
        //        drawRectangles(g2, image, templatePaint, null, 1);

        ShowImages.showWindow(output, "Found Matches", true);
    }

    /**
     * Helper function will is finds matches and displays the results as colored rectangles
     */
    private static void drawRectangles(Graphics2D g2,
    ImageFloat32 image, ImageFloat32 template, ImageFloat32 mask,
    int expectedMatches) {
        List<Match> found = findMatches(image, template, mask, expectedMatches);

        int r = 2;
        int w = template.width + 2 * r;
        int h = template.height + 2 * r;

        for (Match m : found) {
            // the return point is the template's top left corner
            int x0 = m.x - r;
            int y0 = m.y - r;
            int x1 = x0 + w;
            int y1 = y0 + h;

            g2.drawLine(x0, y0, x1, y0);
            g2.drawLine(x1, y0, x1, y1);
            g2.drawLine(x1, y1, x0, y1);
            g2.drawLine(x0, y1, x0, y0);

            g2.drawString("" + m.score, m.x, m.y);
        }
    }

    /**
     * Demonstrates how to search for matches of a template inside an image
     *
     * @param image
     *            Image being searched
     * @param template
     *            Template being looked for
     * @param mask
     *            Mask which determines the weight of each template pixel in the match score
     * @param expectedMatches
     *            Number of expected matches it hopes to find
     * @return List of match location and scores
     */
    private static List<Match> findMatches(ImageFloat32 image, ImageFloat32 template, ImageFloat32 mask,
    int expectedMatches) {
        // create template matcher.
        TemplateMatching<ImageFloat32> matcher = FactoryTemplateMatching.createMatcher(TemplateScoreType.SUM_DIFF_SQ, ImageFloat32.class);

        // Find the points which match the template the best
        matcher.setTemplate(template, mask, expectedMatches);
        matcher.process(image);

        return matcher.getResults().toList();

    }

    static void canny() {
        BufferedImage image = UtilImageIO.loadImage("C:\\Users\\Guenther\\Pictures\\elitedangerous64\\test.png");

        ImageUInt8 gray = ConvertBufferedImage.convertFrom(image, (ImageUInt8) null);
        ImageUInt8 edgeImage = gray.createSameShape();

        // Create a canny edge detector which will dynamically compute the threshold based on maximum edge intensity
        // It has also been configured to save the trace as a graph.  This is the graph created while performing
        // hysteresis thresholding.
        CannyEdge<ImageUInt8, ImageSInt16> canny = FactoryEdgeDetectors.canny(1, true, true, ImageUInt8.class, ImageSInt16.class);

        // The edge image is actually an optional parameter.  If you don't need it just pass in null
        canny.process(gray, 0.001f, 0.2f, edgeImage);

        // First get the contour created by canny
        List<EdgeContour> edgeContours = canny.getContours();
        // The 'edgeContours' is a tree graph that can be difficult to process.  An alternative is to extract
        // the contours from the binary image, which will produce a single loop for each connected cluster of pixels.
        // Note that you are only interested in external contours.
        List<Contour> contours = BinaryImageOps.contour(edgeImage, ConnectRule.EIGHT, null);

        // display the results
        BufferedImage visualBinary = VisualizeBinaryData.renderBinary(edgeImage, false, null);
        BufferedImage visualCannyContour = VisualizeBinaryData.renderContours(edgeContours, null,
        gray.width, gray.height, null);
        BufferedImage visualEdgeContour = new BufferedImage(gray.width, gray.height, BufferedImage.TYPE_INT_RGB);
        VisualizeBinaryData.renderExternal(contours, (int[]) null, visualEdgeContour);

        ListDisplayPanel panel = new ListDisplayPanel();
        panel.addImage(visualBinary, "Binary Edges from Canny");
        panel.addImage(visualCannyContour, "Canny Trace Graph");
        panel.addImage(visualEdgeContour, "Contour from Canny Binary");
        ShowImages.showWindow(panel, "Canny Edge", true);
    }

}
