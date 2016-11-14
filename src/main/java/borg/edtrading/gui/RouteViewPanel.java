package borg.edtrading.gui;

import borg.edtrading.aystar.Path;
import borg.edtrading.data.Galaxy;
import borg.edtrading.data.StarSystem;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.util.Locale;

import javax.swing.BorderFactory;
import javax.swing.JPanel;

/**
 * RouteViewPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class RouteViewPanel extends JPanel {

    private static final long serialVersionUID = 7678578244418094654L;

    static final Logger logger = LogManager.getLogger(RouteViewPanel.class);

    // Constructor input
    private final String viewName;
    private final Galaxy galaxy;
    private final StarSystem fromSystem;
    private final StarSystem toSystem;

    // Updated at runtime
    private Path path = null;

    public RouteViewPanel(String viewName, Galaxy galaxy, StarSystem fromSystem, StarSystem toSystem) {
        this.setBorder(BorderFactory.createLineBorder(Color.GRAY));

        this.viewName = viewName;
        this.galaxy = galaxy;
        this.fromSystem = fromSystem;
        this.toSystem = toSystem;
    }

    @Override
    public Dimension getPreferredSize() {
        return new Dimension(PathViewPanel.PREFERRED_HEIGHT_SIDE * PathViewPanel.PREFERRED_FACTOR_SIDE - PathViewPanel.PREFERRED_SIZE_TOP, PathViewPanel.PREFERRED_SIZE_TOP);
    }

    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);

        g.setFont(new Font("Consolas", Font.PLAIN, 9));
        int x = 5;
        int y = 12;

        // Draw label
        g.setColor(Color.BLACK);
        g.drawString(String.format(Locale.US, "%s -> %s (%.0f Ly)", fromSystem.getName(), toSystem.getName(), fromSystem.distanceTo(toSystem)), x, y);
        y += 20;

        // Draw route
        g.setColor(Color.GRAY);
        if (this.path != null) {
            Path p = this.path;
            while (p.getPrev() != null) {
                StarSystem from = p.getPrev().getStarSystem(galaxy);
                StarSystem to = p.getStarSystem(galaxy);
                g.drawString(String.format(Locale.US, "#%d: %s (+%.0f Ly) %s (=%.0f Ly)", p.getTotalJumps(), from.getName(), from.distanceTo(to), to.getName(), p.getTravelledDistanceLy()), x, y);
                p = p.getPrev();
                y += 12;
            }
        }
    }

    public void updatePath(Path path) {
        this.path = path;

        this.repaint();
    }

}