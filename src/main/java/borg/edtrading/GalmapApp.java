package borg.edtrading;

import boofcv.alg.misc.ImageMiscOps;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.struct.image.GrayU8;
import borg.edtrading.data.Body;
import borg.edtrading.data.Coord;
import borg.edtrading.data.Galaxy;
import borg.edtrading.data.StarSystem;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Point;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import javax.imageio.ImageIO;

/**
 * GalmapApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GalmapApp {

    static final Logger logger = LogManager.getLogger(GalmapApp.class);

    public static void main(String[] args) throws Exception {
        int imageSize = 16384;

        Galaxy galaxy = Galaxy.readDataFromFiles();
        float xmin = 0;
        float xmax = 0;
        float zmin = 0;
        float zmax = 0;
        List<Body> arrivalNeutronStars = findArrivalNeutronStars(galaxy.getBodiesById().values());
        Set<StarSystem> systems = bodiesToSystems(arrivalNeutronStars);
        //Collection<StarSystem> systems = galaxy.getStarSystemsById().values();
        for (StarSystem system : systems) {
            xmin = Math.min(xmin, system.getCoord().getX());
            xmax = Math.max(xmax, system.getCoord().getX());
            zmin = Math.min(zmin, system.getCoord().getZ());
            zmax = Math.max(zmax, system.getCoord().getZ());
        }
        xmin -= 100;
        xmax += 100;
        zmin -= 100;
        zmax += 100;
        float galaxySize = Math.max(xmax - xmin, zmax - zmin);
        logger.debug(String.format(Locale.US, "%.0f < x < %.0f", xmin, xmax));
        logger.debug(String.format(Locale.US, "%.0f < z < %.0f", zmin, zmax));
        logger.debug(String.format(Locale.US, "galaxySize = %.0f", galaxySize));

        GrayU8 image = new GrayU8(imageSize, imageSize);
        ImageMiscOps.fill(image, 0);
        int psize = 11;
        for (StarSystem system : systems) {
            Point p = coordToPoint(system.getCoord(), imageSize, xmin, zmin, galaxySize);
            //image.unsafe_set(p.x, p.y, 1);
            ImageMiscOps.fillRectangle(image, 1, p.x - psize / 2 + 1, p.y - psize / 2 + 1, psize, psize);
        }
        ImageIO.write(VisualizeBinaryData.renderBinary(image, false, null), "png", new File(Constants.TEMP_DIR, "galmap.png"));
    }

    private static final Long TYPE_ID_NEUTRON_STAR = new Long(3);

    private static List<Body> findArrivalNeutronStars(Collection<Body> bodies) {
        List<Body> result = new ArrayList<>();

        for (Body body : bodies) {
            if (TYPE_ID_NEUTRON_STAR.equals(body.getTypeId())) {
                if (Boolean.TRUE.equals(body.getIs_main_star())) {
                    result.add(body);
                }
            }
        }

        logger.trace(result.size() + " of all " + bodies.size() + " bodies are arrival neutron stars");

        return result;
    }

    private static Set<StarSystem> bodiesToSystems(Collection<Body> bodies) {
        Set<StarSystem> result = new HashSet<>(bodies.size());

        for (Body body : bodies) {
            if (body.getStarSystem() != null) {
                result.add(body.getStarSystem());
            }
        }

        logger.trace(result.size() + " of all " + bodies.size() + " bodies have a known star system");

        return result;
    }

    private static Point coordToPoint(Coord coord, int imageSize, float xmin, float zmin, float galaxySize) {
        float xPercent = (coord.getX() - xmin) / galaxySize;
        float yPercent = 1.0f - ((coord.getZ() - zmin) / galaxySize);
        return new Point(Math.round(xPercent * imageSize), Math.round(yPercent * imageSize));
    }

}
