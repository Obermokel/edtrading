package borg.edtrading;

import boofcv.alg.misc.ImageMiscOps;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.struct.image.GrayU8;
import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.services.EddbService;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.awt.Point;
import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.imageio.ImageIO;

/**
 * GalmapApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GalmapApp {

    static final Logger logger = LogManager.getLogger(GalmapApp.class);

    private static final AnnotationConfigApplicationContext APPCTX = new AnnotationConfigApplicationContext(Config.class);

    public static void main(String[] args) throws Exception {
        int imageSize = 16384;

        writeStatsCsv();

        EddbService eddbService = APPCTX.getBean(EddbService.class);
        float xmin = 0;
        float xmax = 0;
        float zmin = 0;
        float zmax = 0;
        Map<String, List<EddbBody>> arrivalStarsBySpectralClass = eddbService.mapStarsBySpectralClass(/* arrivalOnly = */ true);
        List<EddbBody> arrivalNeutronStars = eddbService.retainStarsOfSpectralClasses(arrivalStarsBySpectralClass, "NS");
        Set<EddbSystem> systems = bodiesToSystems(arrivalNeutronStars);
        //Collection<StarSystem> systems = galaxy.getStarSystemsById().values();
        for (EddbSystem system : systems) {
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
        for (EddbSystem system : systems) {
            Point p = coordToPoint(system.getCoord(), imageSize, xmin, zmin, galaxySize);
            //image.unsafe_set(p.x, p.y, 1);
            ImageMiscOps.fillRectangle(image, 1, p.x - psize / 2 + 1, p.y - psize / 2 + 1, psize, psize);
        }
        ImageIO.write(VisualizeBinaryData.renderBinary(image, false, null), "png", new File(Constants.TEMP_DIR, "galmap.png"));
    }

    private static Set<EddbSystem> bodiesToSystems(Collection<EddbBody> bodies) {
        Set<EddbSystem> result = new HashSet<>(bodies.size());

        for (EddbBody body : bodies) {
            if (body.getSystem() != null) {
                result.add(body.getSystem());
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

    private static void writeStatsCsv() throws IOException, ParseException {

        EddbService eddbService = APPCTX.getBean(EddbService.class);
        Map<String, List<EddbBody>> arrivalStarsBySpectralClass = eddbService.mapStarsBySpectralClass(/* arrivalOnly = */ true);

        File csvFile = new File(Constants.TEMP_DIR, "arrivalStars.csv");
        FileUtils.write(csvFile, "Datum", "ISO-8859-1", false);
        for (String spectralClass : arrivalStarsBySpectralClass.keySet()) {
            FileUtils.write(csvFile, ";" + spectralClass, "ISO-8859-1", true);
        }
        FileUtils.write(csvFile, ";TOTAL\r\n", "ISO-8859-1", true);

        long date = new SimpleDateFormat("dd.MM.yyyy HH:mm").parse("25.10.2016 12:00").getTime();

        while (date < System.currentTimeMillis()) {
            String datum = new SimpleDateFormat("dd.MM.yyyy").format(new Date(date));
            FileUtils.write(csvFile, datum, "ISO-8859-1", true);
            int total = 0;
            for (String spectralClass : arrivalStarsBySpectralClass.keySet()) {
                int n = 0;
                for (EddbBody star : arrivalStarsBySpectralClass.get(spectralClass)) {
                    if (star.getCreatedAt().getTime() <= date) {
                        n++;
                        total++;
                    }
                }
                FileUtils.write(csvFile, ";" + n, "ISO-8859-1", true);
            }
            FileUtils.write(csvFile, ";" + total + "\r\n", "ISO-8859-1", true);
            date += DateUtils.MILLIS_PER_DAY;
        }
    }

}
