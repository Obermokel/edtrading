package bord.edtrading;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.List;

import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.alg.feature.detect.edge.EdgeContour;
import boofcv.alg.feature.detect.template.TemplateMatching;
import boofcv.alg.filter.binary.BinaryImageOps;
import boofcv.alg.filter.binary.Contour;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.factory.feature.detect.template.FactoryTemplateMatching;
import boofcv.factory.feature.detect.template.TemplateScoreType;
import boofcv.gui.ListDisplayPanel;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.gui.image.ShowImages;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.io.image.UtilImageIO;
import boofcv.struct.ConnectRule;
import boofcv.struct.feature.Match;
import boofcv.struct.image.ImageFloat32;
import boofcv.struct.image.ImageSInt16;
import boofcv.struct.image.ImageUInt8;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * ImageScan
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ImageScan {

    static final Logger logger = LogManager.getLogger(ImageScan.class);

    public static void main(String[] args) {
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
        drawRectangles(g2, image, templateCursor, null, 10);

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
