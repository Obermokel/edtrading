package borg.edtrading.boofcv;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * TemplateHighlighter
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TemplateHighlighter {

    static final Logger logger = LogManager.getLogger(TemplateHighlighter.class);

    public static void highlightMatches(BufferedImage image, List<TemplateMatch> matches) {
        int r = 2;

        Graphics2D g2 = image.createGraphics();
        g2.setStroke(new BasicStroke(r));
        g2.setFont(new Font("Arial", Font.PLAIN, 14));

        for (TemplateMatch match : matches) {
            int w = match.getTemplate().getImage().width + 2 * r;
            int h = match.getTemplate().getImage().height + 2 * r;

            // the return point is the template's top left corner
            int x0 = match.getMatch().x - r;
            int y0 = match.getMatch().y - r;
            int x1 = x0 + w;
            int y1 = y0 + h;

            g2.setColor(Color.BLUE);
            g2.drawLine(x0, y0, x1, y0);
            g2.drawLine(x1, y0, x1, y1);
            g2.drawLine(x1, y1, x0, y1);
            g2.drawLine(x0, y1, x0, y0);

            g2.setColor(Color.WHITE);
            g2.drawString(match.getTemplate().getText(), match.getMatch().x, match.getMatch().y);
            //g2.drawString(match.getTemplate().getText() + "=" + match.getErrorPerPixel(), match.getMatch().x, match.getMatch().y);
        }
    }

}
