package borg.edtrading.gui;

import borg.edtrading.aystar.Path;
import borg.edtrading.data.Coord;
import borg.edtrading.data.Galaxy;
import borg.edtrading.data.StarSystem;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.swing.BorderFactory;
import javax.swing.JPanel;

/**
 * PathViewPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PathViewPanel extends JPanel {

    private static final long serialVersionUID = -4865601248519686022L;

    static final Logger logger = LogManager.getLogger(PathViewPanel.class);

    // Constructor input
    private final String viewName;
    private final Galaxy galaxy;
    private final StarSystem fromSystem;
    private final StarSystem toSystem;

    // Calculated
    private float xmin;
    private float xmax;
    private float ymin;
    private float ymax;
    private float zmin;
    private float zmax;
    private float sizely;
    private List<StarSystem> referenceSystems;

    // Updated at runtime
    private Path path = null;

    public PathViewPanel(String viewName, Galaxy galaxy, StarSystem fromSystem, StarSystem toSystem) {
        this.setBorder(BorderFactory.createLineBorder(Color.BLACK));

        this.viewName = viewName;
        this.galaxy = galaxy;
        this.fromSystem = fromSystem;
        this.toSystem = toSystem;

        for (StarSystem system : Arrays.asList(fromSystem, toSystem)) {
            this.xmin = Math.min(this.xmin, system.getCoord().getX());
            this.xmax = Math.max(this.xmax, system.getCoord().getX());
            this.ymin = Math.min(this.ymin, system.getCoord().getY());
            this.ymax = Math.max(this.ymax, system.getCoord().getY());
            this.zmin = Math.min(this.zmin, system.getCoord().getZ());
            this.zmax = Math.max(this.zmax, system.getCoord().getZ());
            this.sizely = Math.max(this.sizely, this.xmax - this.xmin);
            this.sizely = Math.max(this.sizely, this.ymax - this.ymin);
            this.sizely = Math.max(this.sizely, this.zmax - this.zmin);
        }
        this.sizely *= 1.25f;
        if (this.xmax - this.xmin < this.sizely) {
            float diff = this.sizely - (this.xmax - this.xmin);
            this.xmin -= (diff / 2);
            this.xmax += (diff / 2);
        }
        if (this.ymax - this.ymin < this.sizely) {
            float diff = this.sizely - (this.ymax - this.ymin);
            this.ymin -= (diff / 2);
            this.ymax += (diff / 2);
        }
        if (this.zmax - this.zmin < this.sizely) {
            float diff = this.sizely - (this.zmax - this.zmin);
            this.zmin -= (diff / 2);
            this.zmax += (diff / 2);
        }

        this.referenceSystems = this.generateReferenceSystems(galaxy, fromSystem, toSystem);
    }

    private List<StarSystem> generateReferenceSystems(Galaxy galaxy, StarSystem fromSystem, StarSystem toSystem) {
        Set<StarSystem> result = new HashSet<>();
        result.add(galaxy.searchStarSystemByName("Sol"));
        result.add(galaxy.searchStarSystemByName("Sagittarius A*"));
        result.add(galaxy.searchStarSystemByName("Colonia"));
        result.add(galaxy.searchStarSystemByName("VY Canis Majoris"));
        result.add(galaxy.searchStarSystemByName("Crab Pulsar"));
        result.remove(fromSystem);
        result.remove(toSystem);

        // @formatter:off
        return result.stream().filter(ss ->
                ss.getCoord().getX() > this.xmin && ss.getCoord().getX() < this.xmax &&
                ss.getCoord().getY() > this.ymin && ss.getCoord().getY() < this.ymax &&
                ss.getCoord().getZ() > this.zmin && ss.getCoord().getZ() < this.zmax)
                .collect(Collectors.toList());
        // @formatter:on
    }

    @Override
    public Dimension getPreferredSize() {
        return new Dimension(400, 400);
    }

    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);

        // Draw label
        g.setColor(Color.GRAY);
        g.drawString(this.viewName, 10, 20);

        // Draw reference systems
        g.setColor(Color.YELLOW);
        for (StarSystem refSystem : this.referenceSystems) {
            Coord coord = refSystem.getCoord();
            Point point = this.coordToPoint(coord);
            String name = refSystem.getName();
            g.fillOval(point.x, point.y, 5, 5);
            g.drawString(name, point.x, point.y);
        }

        // Draw from-system
        g.setColor(Color.BLUE);
        Point pFrom = this.coordToPoint(this.fromSystem.getCoord());
        g.fillOval(pFrom.x, pFrom.y, 5, 5);
        g.drawString(this.fromSystem.getName(), pFrom.x, pFrom.y);

        // Draw to-system
        g.setColor(Color.RED);
        Point pTo = this.coordToPoint(this.toSystem.getCoord());
        g.fillOval(pTo.x, pTo.y, 5, 5);
        g.drawString(this.toSystem.getName(), pTo.x, pTo.y);

        // Draw path
        g.setColor(Color.BLACK);
        if (this.path != null) {
            Path p = this.path;
            while (p.getPrev() != null) {
                Coord fromCoord = p.getPrev().getMinimizedStarSystem().getCoord();
                Coord toCoord = p.getMinimizedStarSystem().getCoord();
                Point fromPoint = this.coordToPoint(fromCoord);
                Point toPoint = this.coordToPoint(toCoord);
                g.drawLine(fromPoint.x, fromPoint.y, toPoint.x, toPoint.y);
                p = p.getPrev();
            }
        }
    }

    public void updatePath(Path path) {
        this.path = path;

        this.repaint();
    }

    private Point coordToPoint(Coord coord) {
        float xPercent = (coord.getX() - this.xmin) / this.sizely;
        float yPercent = 1.0f - ((coord.getZ() - this.zmin) / this.sizely);
        float zPercent = 1.0f - ((coord.getY() - this.ymin) / this.sizely);

        if ("Top view".equals(this.viewName)) {
            return new Point(Math.round(xPercent * this.getWidth()), Math.round(yPercent * this.getHeight()));
        } else if ("Left view".equals(this.viewName)) {
            return new Point(Math.round(yPercent * this.getWidth()), Math.round(zPercent * this.getHeight()));
        } else if ("Front view".equals(this.viewName)) {
            return new Point(Math.round(xPercent * this.getWidth()), Math.round(zPercent * this.getHeight()));
        } else {
            return null;
        }
    }

}
