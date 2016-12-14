package borg.edtrading;

import boofcv.alg.misc.ImageMiscOps;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.gui.image.VisualizeImageData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.Planar;
import borg.edtrading.cfg.Config;
import borg.edtrading.cfg.Constants;
import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.services.EddbService;
import borg.edtrading.util.MiscUtil;
import borg.edtrading.util.StarUtil;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.awt.Color;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

/**
 * GalmapApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GalmapApp {

    static final Logger logger = LogManager.getLogger(GalmapApp.class);

    private static final AnnotationConfigApplicationContext APPCTX = new AnnotationConfigApplicationContext(Config.class);

    static int imageSize = 4096; // Top view, x/z
    static int imageHeight = 512; // Side view, y
    static int psize = 51;
    static int poffset = (psize - 1) / 2;

    static float xmin = 0;
    static float xmax = 0;
    static float ymin = 0;
    static float ymax = 0;
    static float zmin = 0;
    static float zmax = 0;
    static float galaxySize = 0;
    static float galaxyHeight = 0;

    public static void main(String[] args) throws Exception {
        writeStatsCsv();

        EddbService eddbService = APPCTX.getBean(EddbService.class);
        Map<String, List<Coord>> coordsByArrivalSpectralClass = mapCoordsByArrivalSpectralClass();
        List<Coord> arrivalNeutronStarCoords = coordsByArrivalSpectralClass.get("NS");
        List<Coord> allSystemCoords = eddbService.loadAllSystems().parallelStream().map(s -> s.getCoord()).collect(Collectors.toList());
        computeGalaxySize(allSystemCoords);

        writeNeutronMap(arrivalNeutronStarCoords);
        psize = 1;
        poffset = (psize - 1) / 2;
        writeArrivalStarMap(coordsByArrivalSpectralClass);
        writeCompleteStarMap(allSystemCoords, coordsByArrivalSpectralClass);
    }

    private static Map<String, List<Coord>> mapCoordsByArrivalSpectralClass() {
        Map<String, List<Coord>> coordsBySpectralClass = new HashMap<>();
        Map<String, List<EddbBody>> bodiesBySpectralClass = APPCTX.getBean(EddbService.class).mapStarsBySpectralClass(/* arrivalOnly = */ true);
        for (String spectralClass : bodiesBySpectralClass.keySet()) {
            List<Coord> coords = bodiesBySpectralClass.get(spectralClass).parallelStream().map(b -> b.getCoord()).collect(Collectors.toList());
            coordsBySpectralClass.put(spectralClass, coords);
        }
        return coordsBySpectralClass;
    }

    private static void writeNeutronMap(List<Coord> arrivalNeutronStarCoords) throws IOException {
        GrayU8 gray = new GrayU8(imageSize, imageSize);
        ImageMiscOps.fill(gray, 0);
        for (Coord c : arrivalNeutronStarCoords) {
            Point p = coordToPoint(c);
            if (psize <= 1 && gray.isInBounds(p.x, p.y)) {
                gray.unsafe_set(p.x, p.y, 1);
            } else {
                ImageMiscOps.fillRectangle(gray, 1, p.x - poffset, p.y - poffset, psize, psize);
            }
        }
        ImageIO.write(VisualizeBinaryData.renderBinary(gray, false, null), "png", new File(Constants.TEMP_DIR, "galmap neutron.png"));

        psize = ((psize - 1) / 5) + 1;
        poffset = (psize - 1) / 2;
        gray = new GrayU8(imageSize, imageHeight);
        ImageMiscOps.fill(gray, 0);
        for (Coord c : arrivalNeutronStarCoords) {
            Point p = coordToPointFromLeft(c);
            for (int y = p.y - poffset; y <= p.y + poffset; y++) {
                for (int x = p.x - poffset; x <= p.x + poffset; x++) {
                    if (gray.isInBounds(x, y)) {
                        gray.unsafe_set(x, y, Math.min(255, gray.get(x, y) + 32));
                    }
                }
            }
        }
        int minus1000Line = coordToPointFromLeft(new Coord(0f, -1000f, 0f)).y;
        ImageMiscOps.fillRectangle(gray, 255, 0, minus1000Line, gray.width, 1);
        int solX = coordToPointFromLeft(new Coord(0f, 0f, 0f)).x;
        ImageMiscOps.fillRectangle(gray, 255, solX, 0, 1, gray.height);
        ImageIO.write(VisualizeImageData.grayMagnitude(gray, null, -1), "png", new File(Constants.TEMP_DIR, "galmap neutron left.png"));
    }

    private static void writeArrivalStarMap(Map<String, List<Coord>> coordsByArrivalSpectralClass) throws IOException {
        LinkedHashMap<String, Integer> spectralClassFrequencies = new LinkedHashMap<>();
        for (String spectralClass : coordsByArrivalSpectralClass.keySet()) {
            spectralClassFrequencies.put(spectralClass, coordsByArrivalSpectralClass.get(spectralClass).size());
        }
        MiscUtil.sortMapByValueReverse(spectralClassFrequencies); // Draw most frequent first, least frequent last

        Planar<GrayU8> planar = new Planar<>(GrayU8.class, imageSize, imageSize, 3);
        ImageMiscOps.fill(planar.getBand(0), 0);
        ImageMiscOps.fill(planar.getBand(1), 0);
        ImageMiscOps.fill(planar.getBand(2), 0);
        for (String spectralClass : spectralClassFrequencies.keySet()) {
            Color color = StarUtil.spectralClassToColor(spectralClass);
            int r = color.getRed();
            int g = color.getGreen();
            int b = color.getBlue();
            for (Coord c : coordsByArrivalSpectralClass.get(spectralClass)) {
                Point p = coordToPoint(c);
                float alpha = 1;//coordToAlpha(c);
                if (psize <= 1 && planar.isInBounds(p.x, p.y)) {
                    planar.getBand(0).unsafe_set(p.x, p.y, Math.round(r * alpha));
                    planar.getBand(1).unsafe_set(p.x, p.y, Math.round(g * alpha));
                    planar.getBand(2).unsafe_set(p.x, p.y, Math.round(b * alpha));
                } else {
                    ImageMiscOps.fillRectangle(planar.getBand(0), Math.round(r * alpha), p.x - poffset, p.y - poffset, psize, psize);
                    ImageMiscOps.fillRectangle(planar.getBand(1), Math.round(g * alpha), p.x - poffset, p.y - poffset, psize, psize);
                    ImageMiscOps.fillRectangle(planar.getBand(2), Math.round(b * alpha), p.x - poffset, p.y - poffset, psize, psize);
                }
            }
        }
        BufferedImage bi = new BufferedImage(imageSize, imageSize, BufferedImage.TYPE_INT_RGB);
        bi = ConvertBufferedImage.convertTo_U8(planar, bi, true);
        ImageIO.write(bi, "png", new File(Constants.TEMP_DIR, "galmap arrival.png"));
    }

    private static void writeCompleteStarMap(List<Coord> allSystemCoords, Map<String, List<Coord>> coordsByArrivalSpectralClass) throws IOException {
        LinkedHashMap<String, Integer> spectralClassFrequencies = new LinkedHashMap<>();
        for (String spectralClass : coordsByArrivalSpectralClass.keySet()) {
            spectralClassFrequencies.put(spectralClass, coordsByArrivalSpectralClass.get(spectralClass).size());
        }
        MiscUtil.sortMapByValueReverse(spectralClassFrequencies); // Draw most frequent first, least frequent last

        Planar<GrayU8> planar = new Planar<>(GrayU8.class, imageSize, imageSize, 3);
        ImageMiscOps.fill(planar.getBand(0), 0);
        ImageMiscOps.fill(planar.getBand(1), 0);
        ImageMiscOps.fill(planar.getBand(2), 0);
        for (Coord c : allSystemCoords) {
            Point p = coordToPoint(c);
            float alpha = 1;//coordToAlpha(c);
            if (psize <= 1 && planar.isInBounds(p.x, p.y)) {
                planar.getBand(0).unsafe_set(p.x, p.y, Math.round(80 * alpha));
                planar.getBand(1).unsafe_set(p.x, p.y, Math.round(80 * alpha));
                planar.getBand(2).unsafe_set(p.x, p.y, Math.round(80 * alpha));
            } else {
                ImageMiscOps.fillRectangle(planar.getBand(0), Math.round(80 * alpha), p.x - poffset, p.y - poffset, psize, psize);
                ImageMiscOps.fillRectangle(planar.getBand(1), Math.round(80 * alpha), p.x - poffset, p.y - poffset, psize, psize);
                ImageMiscOps.fillRectangle(planar.getBand(2), Math.round(80 * alpha), p.x - poffset, p.y - poffset, psize, psize);
            }
        }
        for (String spectralClass : spectralClassFrequencies.keySet()) {
            Color color = StarUtil.spectralClassToColor(spectralClass);
            int r = color.getRed();
            int g = color.getGreen();
            int b = color.getBlue();
            for (Coord c : coordsByArrivalSpectralClass.get(spectralClass)) {
                Point p = coordToPoint(c);
                float alpha = 1;//coordToAlpha(c);
                if (psize <= 1 && planar.isInBounds(p.x, p.y)) {
                    planar.getBand(0).unsafe_set(p.x, p.y, Math.round(r * alpha));
                    planar.getBand(1).unsafe_set(p.x, p.y, Math.round(g * alpha));
                    planar.getBand(2).unsafe_set(p.x, p.y, Math.round(b * alpha));
                } else {
                    ImageMiscOps.fillRectangle(planar.getBand(0), Math.round(r * alpha), p.x - poffset, p.y - poffset, psize, psize);
                    ImageMiscOps.fillRectangle(planar.getBand(1), Math.round(g * alpha), p.x - poffset, p.y - poffset, psize, psize);
                    ImageMiscOps.fillRectangle(planar.getBand(2), Math.round(b * alpha), p.x - poffset, p.y - poffset, psize, psize);
                }
            }
        }
        BufferedImage bi = new BufferedImage(imageSize, imageSize, BufferedImage.TYPE_INT_RGB);
        bi = ConvertBufferedImage.convertTo_U8(planar, bi, true);
        ImageIO.write(bi, "png", new File(Constants.TEMP_DIR, "galmap all.png"));
    }

    private static void computeGalaxySize(List<Coord> allSystems) {
        for (Coord system : allSystems) {
            xmin = Math.min(xmin, system.getX());
            xmax = Math.max(xmax, system.getX());
            ymin = Math.min(ymin, system.getY());
            ymax = Math.max(ymax, system.getY());
            zmin = Math.min(zmin, system.getZ());
            zmax = Math.max(zmax, system.getZ());
        }
        xmin -= 100;
        xmax += 100;
        ymin -= 100;
        ymax += 100;
        zmin -= 100;
        zmax += 100;
        xmin = -12000;
        xmax = 12000;
        zmin = -2000;
        zmax = 22000;
        ymin = -2000;
        ymax = 2000;
        //        xmin = -10000;
        //        xmax = 10000;
        //        zmin = -10000;
        //        zmax = 10000;
        galaxySize = Math.max(xmax - xmin, zmax - zmin);
        galaxyHeight = ymax - ymin;
        imageHeight = Math.round(imageSize / galaxySize * galaxyHeight);
        logger.debug(String.format(Locale.US, "x: %.0f .. %.0f = %.0f Ly", xmin, xmax, xmax - xmin));
        logger.debug(String.format(Locale.US, "z: %.0f .. %.0f = %.0f Ly", zmin, zmax, zmax - zmin));
        logger.debug(String.format(Locale.US, "galaxy size:    %.0f Ly", galaxySize));
        logger.debug(String.format(Locale.US, "y: %.0f .. %.0f = %.0f Ly", ymin, ymax, ymax - ymin));
        logger.debug(String.format(Locale.US, "galaxy height:  %.0f Ly", galaxyHeight));
    }

    private static Point coordToPoint(Coord coord) {
        float xPercent = (coord.getX() - xmin) / galaxySize;
        float zPercent = 1.0f - ((coord.getZ() - zmin) / galaxySize);
        return new Point(Math.round(xPercent * imageSize), Math.round(zPercent * imageSize));
    }

    private static Point coordToPointFromLeft(Coord coord) {
        float yPercent = 1.0f - ((coord.getY() - ymin) / galaxyHeight);
        float zPercent = 1.0f - ((coord.getZ() - zmin) / galaxySize);
        return new Point(Math.round(zPercent * imageSize), Math.round(yPercent * imageHeight));
    }

    private static float coordToAlpha(Coord coord) {
        float yPercent = (coord.getY() - ymin) / galaxyHeight;
        float fromMiddlePercent = Math.abs((yPercent - 0.5f) * 2f);
        //return 255 - Math.round(fromMiddlePercent * 255); // 255=visible, 0=invisible
        return 1f - fromMiddlePercent; // 1=visible, 0=invisible
    }

    private static void writeStatsCsv() throws IOException, ParseException {
        EddbService eddbService = APPCTX.getBean(EddbService.class);
        Map<String, List<EddbBody>> arrivalStarsBySpectralClass = eddbService.mapStarsBySpectralClass(/* arrivalOnly = */ true);

        LinkedHashMap<String, Integer> spectralClassFrequencies = new LinkedHashMap<>();
        for (String spectralClass : arrivalStarsBySpectralClass.keySet()) {
            spectralClassFrequencies.put(spectralClass, arrivalStarsBySpectralClass.get(spectralClass).size());
        }
        MiscUtil.sortMapByValueReverse(spectralClassFrequencies); // Write most frequent first, least frequent last

        File csvFile = new File(Constants.TEMP_DIR, "arrivalStars.csv");
        FileUtils.write(csvFile, "Datum", "ISO-8859-1", false);
        for (String spectralClass : spectralClassFrequencies.keySet()) {
            FileUtils.write(csvFile, ";" + spectralClass, "ISO-8859-1", true);
        }
        FileUtils.write(csvFile, ";TOTAL\r\n", "ISO-8859-1", true);

        long date = new SimpleDateFormat("dd.MM.yyyy HH:mm").parse("25.10.2016 12:00").getTime();

        while (date < System.currentTimeMillis()) {
            String datum = new SimpleDateFormat("dd.MM.yyyy").format(new Date(date));
            FileUtils.write(csvFile, datum, "ISO-8859-1", true);
            int total = 0;
            for (String spectralClass : spectralClassFrequencies.keySet()) {
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
