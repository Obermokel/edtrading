package borg.edtrading;

import boofcv.abst.feature.describe.ConfigBrief;
import boofcv.abst.feature.describe.DescribeRegionPoint;
import boofcv.abst.feature.detdesc.DetectDescribePoint;
import boofcv.abst.feature.detect.interest.ConfigGeneralDetector;
import boofcv.abst.feature.detect.interest.InterestPointDetector;
import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.alg.feature.detect.interest.GeneralFeatureDetector;
import boofcv.alg.filter.binary.BinaryImageOps;
import boofcv.alg.filter.binary.Contour;
import boofcv.alg.filter.binary.GThresholdImageOps;
import boofcv.alg.filter.derivative.GImageDerivativeOps;
import boofcv.alg.misc.ImageMiscOps;
import boofcv.core.image.ConvertImage;
import boofcv.factory.feature.describe.FactoryDescribeRegionPoint;
import boofcv.factory.feature.detdesc.FactoryDetectDescribe;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.factory.feature.detect.interest.FactoryDetectPoint;
import boofcv.factory.feature.detect.interest.FactoryInterestPoint;
import boofcv.gui.image.VisualizeImageData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.ConnectRule;
import boofcv.struct.feature.BrightFeature;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayU8;
import borg.edtrading.util.ImageUtil;
import borg.edtrading.util.MiscUtil;
import georegression.struct.point.Point2D_F64;
import georegression.struct.point.Point2D_I32;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import javax.imageio.ImageIO;

/**
 * FittingTest
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FittingTest {

    static final Logger logger = LogManager.getLogger(FittingTest.class);

    public static void main(String[] args) throws Exception {
        File sourceFile = new File(Constants.SURFACE_MATS_DIR, "_ALL_\\2016-08-31 21-52-21 HIP 11131.png");
        Map<String, CharDescription> referenceDescriptions = readReferenceDescriptions(new File(Constants.EDTRADING_BASE_DIR, "Feature Refs"));

        BufferedImage originalImage = ImageIO.read(sourceFile);
        BufferedImage fourKImage = ImageUtil.scaleAndCrop(originalImage, 2 * 3840, 2 * 2160);
        BufferedImage croppedImage = fourKImage.getSubimage(2 * 20, 2 * 20, 2 * 820, 2 * 2120);

        GrayF32 grayImage = ConvertBufferedImage.convertFromSingle(croppedImage, null, GrayF32.class);
        ImageIO.write(VisualizeImageData.grayMagnitude(grayImage, null, 255), "PNG", new File(Constants.TEMP_DIR, "gray.png"));

        GrayU8 localSquareImage = GThresholdImageOps.localSquare(grayImage, null, 160, 0.7, false, null, null);
        ImageIO.write(VisualizeImageData.grayMagnitude(localSquareImage, null, 255), "PNG", new File(Constants.TEMP_DIR, "squareU8.png"));

        GrayF32 localSquareImageF32 = ConvertImage.convert(localSquareImage, (GrayF32) null);
        ImageIO.write(VisualizeImageData.grayMagnitude(localSquareImageF32, null, 255), "PNG", new File(Constants.TEMP_DIR, "squareF32.png"));

        List<GrayF32> charImages = extractChars(localSquareImageF32);
        List<CharDescription> charDescriptions = new ArrayList<>(charImages.size());
        for (int i = 0; i < charImages.size(); i++) {
            GrayF32 charImage = charImages.get(i);
            CharDescription desc = CharDescription.create(charImage);
            charDescriptions.add(desc);
        }

        ListIterator<CharDescription> itKnown = charDescriptions.listIterator();
        while (itKnown.hasNext()) {
            CharDescription charDesc = itKnown.next();
            String name = lookupName(charDesc, referenceDescriptions);
            if (name != null) {
                File dir = new File(Constants.TEMP_DIR, name);
                if (!dir.exists()) {
                    dir.mkdirs();
                }
                charDesc.visualize(new File(dir, System.currentTimeMillis() + ".png"));
                itKnown.remove();
            }
        }

        //        int radius = 96; // 128-160 looks good
        //        double scale = 0.5; //  <0.5 is too dark  //  0.5-0.7 looks good  //  >0.7 is too bright
        //        for (radius = 96; radius <= 160; radius += 16) {
        //            for (scale = 0.5; scale <= 0.8; scale += 0.1) {
        //                GrayU8 squareImage = GThresholdImageOps.localSquare(grayImage, null, radius, scale, false, null, null);
        //                ImageIO.write(VisualizeBinaryData.renderBinary(squareImage, false, null), "PNG", new File(Constants.TEMP_DIR, "square_radius" + radius + "_scale" + scale + ".png"));
        //            }
        //        }

        int refId = 0;
        while (!charDescriptions.isEmpty()) {
            refId++;
            CharDescription refDesc = charDescriptions.remove(0);
            List<CharDescription> similarDescs = new ArrayList<>();

            ListIterator<CharDescription> it = charDescriptions.listIterator();
            while (it.hasNext()) {
                CharDescription otherDesc = it.next();
                if (otherDesc.isSimilar(refDesc)) {
                    it.remove();
                    similarDescs.add(otherDesc);
                }
            }

            File dir = new File(Constants.TEMP_DIR, "ref" + refId);
            if (!dir.exists()) {
                dir.mkdirs();
            }
            refDesc.write(new File(dir, "_REF.png"));
            for (int i = 0; i < similarDescs.size(); i++) {
                similarDescs.get(i).write(new File(dir, i + ".png"));
            }
        }
    }

    private static String lookupName(CharDescription charDesc, Map<String, CharDescription> referenceDescriptions) {
        for (String name : referenceDescriptions.keySet()) {
            CharDescription refDesc = referenceDescriptions.get(name);
            if (charDesc.isSimilar(refDesc)) {
                return name;
            }
        }
        return null;
    }

    private static Map<String, CharDescription> readReferenceDescriptions(File baseDir) throws IOException {
        Map<String, CharDescription> result = new LinkedHashMap<>();

        File[] subDirs = baseDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.isDirectory();
            }
        });
        for (File subDir : subDirs) {
            String name = subDir.getName();
            CharDescription referenceCharDescription = createReferenceCharDescription(subDir);
            result.put(name, referenceCharDescription);
        }

        return result;
    }

    public static List<GrayF32> extractChars(GrayF32 input) {
        List<GrayF32> result = new ArrayList<>();

        // Finds external contours
        GrayU8 binary = new GrayU8(input.width, input.height);
        CannyEdge<GrayF32, GrayF32> canny = FactoryEdgeDetectors.canny(2, false, true, GrayF32.class, GrayF32.class);
        canny.process(input, 0.1f, 0.3f, binary);
        List<Contour> contours = BinaryImageOps.contour(binary, ConnectRule.EIGHT, null);

        // Extract chars
        for (Contour c : contours) {
            GrayF32 charImage = extractCharImage(input, c);
            if (charImage != null) {
                result.add(charImage);
            }
        }

        return result;
    }

    private static CharDescription createReferenceCharDescription(File dirWithReferenceImages) throws IOException {
        File[] charImageFiles = dirWithReferenceImages.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().endsWith(".png");
            }
        });

        float avgNumberOfFeatures = 0;
        List<CharDescription> charDescriptions = new ArrayList<>(charImageFiles.length);
        for (File charImageFile : charImageFiles) {
            BufferedImage image = ImageIO.read(charImageFile);
            GrayF32 grayImage = ConvertBufferedImage.convertFromSingle(image, null, GrayF32.class);
            CharDescription charDescription = CharDescription.create(grayImage);
            charDescriptions.add(charDescription);
            avgNumberOfFeatures += charDescription.featureLocations.size();
        }

        avgNumberOfFeatures /= charDescriptions.size();

        LinkedHashMap<Point2D_Percent, Integer> countByPoint = new LinkedHashMap<>();
        for (CharDescription currentDescription : charDescriptions) {
            for (Point2D_Percent currentPoint : currentDescription.featureLocations) {
                for (CharDescription otherDescription : charDescriptions) {
                    for (Point2D_Percent otherPoint : otherDescription.featureLocations) {
                        double distance = currentPoint.distanceTo(otherPoint);
                        if (distance <= CharDescription.MAX_FEATURE_DIST_PERCENT) {
                            int count = countByPoint.getOrDefault(currentPoint, 0);
                            count++;
                            countByPoint.put(currentPoint, count);
                        }
                    }
                }
            }
        }

        MiscUtil.sortMapByValue(countByPoint, new Comparator<Integer>() {
            @Override
            public int compare(Integer c1, Integer c2) {
                return -1 * c1.compareTo(c2);
            }
        });

        List<Point2D_Percent> bestPoints = new ArrayList<>();
        for (Point2D_Percent p : countByPoint.keySet()) {
            bestPoints.add(p);
            if (bestPoints.size() >= avgNumberOfFeatures) {
                break;
            }
        }

        return new CharDescription(null, bestPoints);
    }

    private static GrayF32 extractCharImage(GrayF32 input, Contour c) {
        if (c.external.size() >= 2) {
            List<Integer> xList = new ArrayList<>(c.external.size());
            List<Integer> yList = new ArrayList<>(c.external.size());

            for (Point2D_I32 p : c.external) {
                xList.add(p.x);
                yList.add(p.y);
            }

            Collections.sort(xList);
            Collections.sort(yList);

            int x0 = xList.get(0);
            int x1 = xList.get(xList.size() - 1);
            int y0 = yList.get(0);
            int y1 = yList.get(yList.size() - 1);

            int width = x1 - x0;
            int height = y1 - y0;

            if (width >= 8 && width <= 100) {
                if (height >= 32 && height <= 100) {
                    return input.subimage(x0, y0, x1, y1, null);
                }
            }
        }

        return null;
    }

    public static class CharDescription {

        private static final boolean STORE_IMAGE_FOR_DEBUGGING = true;

        private static final double MAX_FEATURE_DIST_PERCENT = 0.1;
        private static final double MIN_MATCHES = 0.75;

        private final GrayF32 charImage;
        private final List<Point2D_Percent> featureLocations;

        private CharDescription(GrayF32 charImage, List<Point2D_Percent> featureLocations) {
            this.charImage = charImage;
            this.featureLocations = featureLocations;
        }

        public void visualize(File file) throws IOException {
            GrayF32 surroundedImage = surround(charImage);
            DetectDescribePoint<GrayF32, BrightFeature> ddp = detect(surroundedImage);

            BufferedImage visualizedFeatures = VisualizeImageData.grayMagnitude(surroundedImage, null, -1);
            BufferedImage coloredFeatures = new BufferedImage(visualizedFeatures.getWidth(), visualizedFeatures.getHeight(), BufferedImage.TYPE_INT_RGB);
            Graphics2D g = coloredFeatures.createGraphics();
            g.drawImage(visualizedFeatures, 0, 0, null);
            g.setColor(Color.RED);
            g.setStroke(new BasicStroke(1));
            for (int f = 0; f < ddp.getNumberOfFeatures(); f++) {
                //coloredFeatures.setRGB((int) ddp.getLocation(f).x, (int) ddp.getLocation(f).y, Color.RED.getRGB());
                g.drawLine((int) ddp.getLocation(f).x - 5, (int) ddp.getLocation(f).y, (int) ddp.getLocation(f).x + 5, (int) ddp.getLocation(f).y);
                g.drawLine((int) ddp.getLocation(f).x, (int) ddp.getLocation(f).y - 5, (int) ddp.getLocation(f).x, (int) ddp.getLocation(f).y + 5);
            }
            g.dispose();
            ImageIO.write(coloredFeatures, "PNG", file);
        }

        public void write(File file) throws IOException {
            BufferedImage image = VisualizeImageData.grayMagnitude(charImage, null, -1);
            ImageIO.write(image, "PNG", file);
        }

        public static CharDescription create(GrayF32 charImage) {
            GrayF32 surroundedImage = surround(charImage);
            DetectDescribePoint<GrayF32, BrightFeature> ddp = detect(surroundedImage);

            List<Point2D_Percent> featureLocations = new ArrayList<>(ddp.getNumberOfFeatures());
            for (int idx = 0; idx < ddp.getNumberOfFeatures(); idx++) {
                featureLocations.add(new Point2D_Percent(ddp.getLocation(idx), surroundedImage));
            }
            return new CharDescription(STORE_IMAGE_FOR_DEBUGGING ? charImage : null, featureLocations);
        }

        private static DetectDescribePoint<GrayF32, BrightFeature> detect(GrayF32 surroundedImage) {
            Class derivType = GImageDerivativeOps.getDerivativeType(surroundedImage.getClass());
            GeneralFeatureDetector corner = FactoryDetectPoint.createShiTomasi(new ConfigGeneralDetector(1000, 2, 1), false, derivType);
            InterestPointDetector detector = FactoryInterestPoint.wrapPoint(corner, 1, surroundedImage.getClass(), derivType);
            DescribeRegionPoint describe = FactoryDescribeRegionPoint.brief(new ConfigBrief(true), surroundedImage.getClass());
            DetectDescribePoint<GrayF32, BrightFeature> ddp = FactoryDetectDescribe.fuseTogether(detector, null, describe);
            ddp.detect(surroundedImage);

            return ddp;
        }

        private static GrayF32 surround(GrayF32 original) {
            GrayF32 surrounded = new GrayF32(3 * original.width, 3 * original.height);
            ImageMiscOps.fill(surrounded, 0);
            ImageMiscOps.copy(0, 0, original.width, original.height, original.width, original.height, original, surrounded);
            return surrounded;
        }

        public boolean isSimilar(CharDescription other) {
            if (this.featureLocations.size() > other.featureLocations.size()) {
                return this.isSimilarInternal(other);
            } else {
                return other.isSimilarInternal(this);
            }
        }

        private boolean isSimilarInternal(CharDescription other) {
            int matchingFeatures = 0;
            for (Point2D_Percent pThis : this.featureLocations) {
                for (Point2D_Percent pOther : other.featureLocations) {
                    if (pThis.distanceTo(pOther) <= MAX_FEATURE_DIST_PERCENT) {
                        matchingFeatures++;
                        break;
                    }
                }
            }

            double matches = (double) matchingFeatures / (double) this.featureLocations.size();

            return matches >= MIN_MATCHES;
        }

    }

    public static class Point2D_Percent {

        private final double x;
        private final double y;

        public Point2D_Percent(double x, double y) {
            this.x = x;
            this.y = y;
        }

        public Point2D_Percent(Point2D_F64 p, GrayF32 surroundedImage) {
            this.x = (p.x - (surroundedImage.width / 3)) / (surroundedImage.width / 3);
            this.y = (p.y - (surroundedImage.height / 3)) / (surroundedImage.height / 3);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            long temp;
            temp = Double.doubleToLongBits(this.x);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            temp = Double.doubleToLongBits(this.y);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            Point2D_Percent other = (Point2D_Percent) obj;
            if (Double.doubleToLongBits(this.x) != Double.doubleToLongBits(other.x)) {
                return false;
            }
            if (Double.doubleToLongBits(this.y) != Double.doubleToLongBits(other.y)) {
                return false;
            }
            return true;
        }

        public double distanceTo(Point2D_Percent other) {
            double dx = this.x - other.x;
            double dy = this.y - other.y;

            return Math.sqrt(dx * dx + dy * dy);
        }

    }

}
