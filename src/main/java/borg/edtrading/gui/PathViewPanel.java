package borg.edtrading.gui;

import borg.edtrading.aystar.Path;
import borg.edtrading.data.Coord;
import borg.edtrading.data.Galaxy;
import borg.edtrading.eddb.data.StarSystem;
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

    private static final int POINT_SIZE = 7;
    private static final int POINT_OFFSET = (POINT_SIZE - 1) / 2;

    static final Logger logger = LogManager.getLogger(PathViewPanel.class);

    static final int PREFERRED_SIZE_TOP = 850;
    static final int PREFERRED_HEIGHT_SIDE = 80;
    static final int PREFERRED_FACTOR_SIDE = 16;

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
    private float sizeXZ;
    private float sizeY;
    private List<StarSystem> referenceSystems;

    // Updated at runtime
    private Path path = null;

    public PathViewPanel(String viewName, Galaxy galaxy, StarSystem fromSystem, StarSystem toSystem) {
        this.setBorder(BorderFactory.createLineBorder(Color.GRAY));

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
            this.sizeXZ = Math.max(this.sizeXZ, this.xmax - this.xmin);
            this.sizeY = Math.max(this.sizeY, this.ymax - this.ymin);
            this.sizeXZ = Math.max(this.sizeXZ, this.zmax - this.zmin);
        }
        this.sizeXZ *= 1.5f;
        this.sizeY *= 1.5f;
        if (this.sizeY < this.sizeXZ / PREFERRED_FACTOR_SIDE) {
            this.sizeY = this.sizeXZ / PREFERRED_FACTOR_SIDE;
        } else if (this.sizeXZ < PREFERRED_FACTOR_SIDE * this.sizeY) {
            this.sizeXZ = PREFERRED_FACTOR_SIDE * this.sizeY;
        }
        if (this.xmax - this.xmin < this.sizeXZ) {
            float diff = this.sizeXZ - (this.xmax - this.xmin);
            this.xmin -= (diff / 2);
            this.xmax += (diff / 2);
        }
        if (this.ymax - this.ymin < this.sizeY) {
            float diff = this.sizeY - (this.ymax - this.ymin);
            this.ymin -= (diff / 2);
            this.ymax += (diff / 2);
        }
        if (this.zmax - this.zmin < this.sizeXZ) {
            float diff = this.sizeXZ - (this.zmax - this.zmin);
            this.zmin -= (diff / 2);
            this.zmax += (diff / 2);
        }

        this.referenceSystems = this.generateReferenceSystems(galaxy, fromSystem, toSystem);
    }

    private List<StarSystem> generateReferenceSystems(Galaxy galaxy, StarSystem fromSystem, StarSystem toSystem) {
        Set<StarSystem> result = new HashSet<>();
        result.add(galaxy.searchStarSystemByName("Sol"));
        result.add(galaxy.searchStarSystemByName("Maia"));
        result.add(galaxy.searchStarSystemByName("VY Canis Majoris"));
        result.add(galaxy.searchStarSystemByName("Crab Pulsar"));
        result.add(galaxy.searchStarSystemByName("Rho Cassiopeiae"));
        result.add(galaxy.searchStarSystemByName("Colonia"));
        result.add(galaxy.searchStarSystemByName("Sagittarius A*"));
        result.add(galaxy.searchStarSystemByName("Beagle Point"));
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
        if ("Top view".equals(this.viewName)) {
            return new Dimension(PREFERRED_SIZE_TOP, PREFERRED_SIZE_TOP);
        } else {
            return new Dimension(PREFERRED_HEIGHT_SIDE * PREFERRED_FACTOR_SIDE, PREFERRED_HEIGHT_SIDE);
        }
    }

    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);

        // Black background
        g.setColor(Color.BLACK);
        g.fillRect(0, 0, this.getWidth(), this.getHeight());

        // Draw label
        g.setColor(Color.LIGHT_GRAY);
        g.drawString(this.viewName, 20, 20);

        // Draw -1000Ly line
        g.setColor(Color.CYAN);
        if ("Left view".equals(this.viewName)) {
            Point fromPoint = this.coordToPoint(new Coord(0, -1000, -sizeXZ));
            Point toPoint = this.coordToPoint(new Coord(0, -1000, sizeXZ));
            g.drawLine(fromPoint.x, fromPoint.y, toPoint.x, toPoint.y);
        } else if ("Front view".equals(this.viewName)) {
            Point fromPoint = this.coordToPoint(new Coord(-sizeXZ, -1000, 0));
            Point toPoint = this.coordToPoint(new Coord(sizeXZ, -1000, 0));
            g.drawLine(fromPoint.x, fromPoint.y, toPoint.x, toPoint.y);
        }

        // Draw reference systems
        g.setColor(Color.GRAY);
        for (StarSystem refSystem : this.referenceSystems) {
            Coord coord = refSystem.getCoord();
            Point point = this.coordToPoint(coord);
            String name = refSystem.getName();
            g.fillOval(point.x - POINT_OFFSET, point.y - POINT_OFFSET, POINT_SIZE, POINT_SIZE);
            g.drawString(name, point.x - POINT_OFFSET, point.y - POINT_OFFSET);
        }

        // Draw from-system
        g.setColor(Color.GRAY);
        Point pFrom = this.coordToPoint(this.fromSystem.getCoord());
        g.fillOval(pFrom.x - POINT_OFFSET, pFrom.y - POINT_OFFSET, POINT_SIZE, POINT_SIZE);
        g.drawString(this.fromSystem.getName(), pFrom.x - POINT_OFFSET, pFrom.y - POINT_OFFSET);

        // Draw to-system
        g.setColor(Color.GRAY);
        Point pTo = this.coordToPoint(this.toSystem.getCoord());
        g.fillOval(pTo.x - POINT_OFFSET, pTo.y - POINT_OFFSET, POINT_SIZE, POINT_SIZE);
        g.drawString(this.toSystem.getName(), pTo.x - POINT_OFFSET, pTo.y - POINT_OFFSET);

        // Draw path
        g.setColor(Color.ORANGE);
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
        float xPercent = (coord.getX() - this.xmin) / this.sizeXZ;
        float yPercent = 1.0f - ((coord.getZ() - this.zmin) / this.sizeXZ);
        float zPercent = 1.0f - ((coord.getY() - this.ymin) / this.sizeY);

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
