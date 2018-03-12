package borg.edtrading;

import boofcv.abst.feature.associate.AssociateDescription;
import boofcv.abst.feature.associate.ScoreAssociation;
import boofcv.abst.feature.detdesc.DetectDescribePoint;
import boofcv.abst.feature.detect.interest.ConfigFastHessian;
import boofcv.alg.descriptor.UtilFeature;
import boofcv.factory.feature.associate.FactoryAssociation;
import boofcv.factory.feature.detdesc.FactoryDetectDescribe;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.feature.AssociatedIndex;
import boofcv.struct.feature.BrightFeature;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageGray;
import boofcv.struct.image.ImageType;
import boofcv.struct.image.Planar;
import georegression.struct.point.Point2D_F64;
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics;
import org.ddogleg.struct.FastQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.imageio.ImageIO;

/**
 * LocateAndIdentifyCurrentTarget
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class LocateAndIdentifyCurrentTarget {

    static final Logger logger = LoggerFactory.getLogger(LocateAndIdentifyCurrentTarget.class);

    public static void main(String[] args) throws IOException {
        File baseDir = new File(System.getProperty("user.home"), "cruisecontrol");
        File refsDir = new File(baseDir, "refs");
        File screenshotsDir = new File(baseDir, "screenshots");
        File debugDir = new File(baseDir, "debug");

        Map<String, BufferedImage> refImages = loadRefImages(refsDir);

        for (File screenshotFile : screenshotsDir.listFiles()) {
            if (screenshotFile.getName().endsWith(".png")) {
                //if (!screenshotFile.getName().contains("2017-11-02 08-37-51 Groerd BB-A c0.png")) {
                //                if (!screenshotFile.getName().contains("2017-11-02 08-36-52 Groerd BB-A c0.png")) {
                //                    continue;
                //                }
                BufferedImage screenshotImage = ImageIO.read(screenshotFile);
                //                BufferedImage debugImage = locateAndIdentifyTarget(screenshotImage, refImages);
                //                ImageIO.write(debugImage, "PNG", new File(debugDir, screenshotFile.getName()));
                BufferedImage debugImageColor = locateAndIdentifyTargetColor(screenshotImage, refImages);
                ImageIO.write(debugImageColor, "PNG", new File(debugDir, screenshotFile.getName().replace(".png", "_color.png")));
            }
        }
    }

    private static Map<String, BufferedImage> loadRefImages(File refsDir) throws IOException {
        Map<String, BufferedImage> result = new HashMap<>();

        for (File refFile : refsDir.listFiles()) {
            if (refFile.getName().endsWith(".png")) {
                result.put(refFile.getName().replace(".png", ""), ImageIO.read(refFile));
            }
        }

        return result;
    }

    private static BufferedImage locateAndIdentifyTarget(BufferedImage screenshotImage, Map<String, BufferedImage> refImages) {
        String bestRefName = null;
        double bestRefStddev = Double.MAX_VALUE;
        double bestRefDx = 0;
        double bestRefDy = 0;

        for (String refName : refImages.keySet()) {
            System.out.println("==== " + refName + " ====");
            BufferedImage refImage = refImages.get(refName);
            GrayU8 objectGray = ConvertBufferedImage.convertFrom(refImage, (GrayU8) null);
            GrayU8 screenshotGray = ConvertBufferedImage.convertFrom(screenshotImage, (GrayU8) null);

            Class imageType = GrayU8.class;
            DetectDescribePoint<ImageGray, BrightFeature> detDesc = FactoryDetectDescribe.surfStable(new ConfigFastHessian(1, 2, 300, 1, 9, 4, 4), null, null, imageType);
            ScoreAssociation<BrightFeature> scorer = FactoryAssociation.defaultScore(detDesc.getDescriptionType());
            AssociateDescription<BrightFeature> associate = FactoryAssociation.greedy(scorer, Double.MAX_VALUE, true);

            List<Point2D_F64> pointsA = new ArrayList<>();
            List<Point2D_F64> pointsB = new ArrayList<>();
            FastQueue<BrightFeature> descA = UtilFeature.createQueue(detDesc, 100);
            FastQueue<BrightFeature> descB = UtilFeature.createQueue(detDesc, 100);
            describeImage(detDesc, objectGray, pointsA, descA);
            describeImage(detDesc, screenshotGray, pointsB, descB);
            associate.setSource(descA);
            associate.setDestination(descB);
            associate.associate();

            FastQueue<AssociatedIndex> matches = associate.getMatches();
            DescriptiveStatistics statsX = new DescriptiveStatistics();
            DescriptiveStatistics statsY = new DescriptiveStatistics();
            for (int i = 0; i < matches.size(); i++) {
                AssociatedIndex associatedIndex = matches.get(i);
                if (associatedIndex.fitScore < 0.1) {
                    Point2D_F64 pointA = pointsA.get(associatedIndex.src);
                    Point2D_F64 pointB = pointsB.get(associatedIndex.dst);
                    double dx = pointB.x - pointA.x;
                    double dy = pointB.y - pointA.y;
                    statsX.addValue(dx);
                    statsY.addValue(dy);
                    System.out.println(String.format(Locale.US, "%.1f / %.1f = %.4f", dx, dy, associatedIndex.fitScore));
                }
            }
            double stddev = statsX.getStandardDeviation() + statsY.getStandardDeviation();
            System.out.println(String.format(Locale.US, "%,.1f + %,.1f = %,.1f", statsX.getStandardDeviation(), statsY.getStandardDeviation(), stddev));

            if (stddev < bestRefStddev) {
                bestRefName = refName;
                bestRefStddev = stddev;
                bestRefDx = statsX.getPercentile(50);
                bestRefDy = statsY.getPercentile(50);
            }
        }

        BufferedImage debugImage = new BufferedImage(screenshotImage.getWidth(), screenshotImage.getHeight(), screenshotImage.getType());
        Graphics2D g = debugImage.createGraphics();
        g.drawImage(screenshotImage, 0, 0, null);
        if (bestRefName != null) {
            BufferedImage refImage = refImages.get(bestRefName);
            int x = (int) bestRefDx;
            int y = (int) bestRefDy;
            g.setColor(Color.MAGENTA);
            g.setStroke(new BasicStroke(2));
            g.setFont(new Font("Sans Serif", Font.BOLD, 20));
            g.drawRect(x, y, refImage.getWidth(), refImage.getHeight());
            g.drawString(String.format(Locale.US, "%s = %,.1f", bestRefName, bestRefStddev), x, y);
        }
        g.dispose();

        return debugImage;
    }

    /**
     * Detects features inside the two images and computes descriptions at those points.
     */
    private static void describeImage(DetectDescribePoint<ImageGray, BrightFeature> detDesc, GrayU8 input, List<Point2D_F64> points, FastQueue<BrightFeature> descs) {
        detDesc.detect(input);

        for (int i = 0; i < detDesc.getNumberOfFeatures(); i++) {
            points.add(detDesc.getLocation(i).copy());
            descs.grow().setTo(detDesc.getDescription(i));
        }
    }

    private static BufferedImage locateAndIdentifyTargetColor(BufferedImage screenshotImage, Map<String, BufferedImage> refImages) {
        String bestRefName = null;
        double bestRefStddev = Double.MAX_VALUE;
        double bestRefxMin = 999999999;
        double bestRefxMax = -999999999;
        double bestRefyMin = 999999999;
        double bestRefyMax = -999999999;

        for (String refName : refImages.keySet()) {
            System.out.println("==== " + refName + " ====");
            BufferedImage refImage = refImages.get(refName);
            Planar<GrayU8> objectGray = ConvertBufferedImage.convertFromMulti(refImage, (Planar<GrayU8>) null, true, GrayU8.class);
            Planar<GrayU8> screenshotGray = ConvertBufferedImage.convertFromMulti(screenshotImage, (Planar<GrayU8>) null, true, GrayU8.class);

            ImageType<Planar<GrayU8>> imageType = ImageType.pl(3, GrayU8.class);
            DetectDescribePoint<Planar<GrayU8>, BrightFeature> detDesc = FactoryDetectDescribe.surfColorStable(new ConfigFastHessian(1, 2, 300, 1, 9, 4, 4), null, null, imageType);
            ScoreAssociation<BrightFeature> scorer = FactoryAssociation.defaultScore(detDesc.getDescriptionType());
            AssociateDescription<BrightFeature> associate = FactoryAssociation.greedy(scorer, Double.MAX_VALUE, true);

            List<Point2D_F64> pointsA = new ArrayList<>();
            List<Point2D_F64> pointsB = new ArrayList<>();
            FastQueue<BrightFeature> descA = UtilFeature.createQueue(detDesc, 100);
            FastQueue<BrightFeature> descB = UtilFeature.createQueue(detDesc, 100);
            describeImageColor(detDesc, objectGray, pointsA, descA);
            describeImageColor(detDesc, screenshotGray, pointsB, descB);
            associate.setSource(descA);
            associate.setDestination(descB);
            associate.associate();

            FastQueue<AssociatedIndex> matches = associate.getMatches();
            DescriptiveStatistics statsX = new DescriptiveStatistics();
            DescriptiveStatistics statsY = new DescriptiveStatistics();
            for (int i = 0; i < matches.size(); i++) {
                AssociatedIndex associatedIndex = matches.get(i);
                if (associatedIndex.fitScore <= 0.05) {
                    Point2D_F64 pointA = pointsA.get(associatedIndex.src);
                    Point2D_F64 pointB = pointsB.get(associatedIndex.dst);
                    double dx = pointB.x - pointA.x;
                    double dy = pointB.y - pointA.y;
                    statsX.addValue(dx);
                    statsY.addValue(dy);
                    System.out.println(String.format(Locale.US, "%.1f / %.1f = %.4f", dx, dy, associatedIndex.fitScore));
                }
            }
            double stddev = statsX.getStandardDeviation() + statsY.getStandardDeviation();
            System.out.println(String.format(Locale.US, "%,.1f + %,.1f = %,.1f", statsX.getStandardDeviation(), statsY.getStandardDeviation(), stddev));

            if (stddev < bestRefStddev) {
                bestRefName = refName;
                bestRefStddev = stddev;
                for (int i = 0; i < matches.size(); i++) {
                    AssociatedIndex associatedIndex = matches.get(i);
                    if (associatedIndex.fitScore <= 0.05) {
                        Point2D_F64 pointB = pointsB.get(associatedIndex.dst);
                        bestRefxMin = Math.min(bestRefxMin, pointB.x);
                        bestRefxMax = Math.max(bestRefxMin, pointB.x);
                        bestRefyMin = Math.min(bestRefyMin, pointB.y);
                        bestRefyMax = Math.max(bestRefyMin, pointB.y);
                    }
                }
            }
        }

        BufferedImage debugImage = new BufferedImage(screenshotImage.getWidth(), screenshotImage.getHeight(), screenshotImage.getType());
        Graphics2D g = debugImage.createGraphics();
        g.drawImage(screenshotImage, 0, 0, null);
        if (bestRefName != null) {
            //BufferedImage refImage = refImages.get(bestRefName);
            int x = (int) bestRefxMin;
            int y = (int) bestRefyMin;
            int w = (int) bestRefxMax - x;
            int h = (int) bestRefyMax - y;
            g.setColor(Color.MAGENTA);
            g.setStroke(new BasicStroke(2));
            g.setFont(new Font("Sans Serif", Font.BOLD, 20));
            g.drawRect(x, y, w, h);
            g.drawString(String.format(Locale.US, "%s = %,.1f", bestRefName, bestRefStddev), x, y);
        }
        g.dispose();

        return debugImage;
    }

    /**
     * Detects features inside the two images and computes descriptions at those points.
     */
    private static void describeImageColor(DetectDescribePoint<Planar<GrayU8>, BrightFeature> detDesc, Planar<GrayU8> input, List<Point2D_F64> points, FastQueue<BrightFeature> descs) {
        detDesc.detect(input);

        for (int i = 0; i < detDesc.getNumberOfFeatures(); i++) {
            points.add(detDesc.getLocation(i).copy());
            descs.grow().setTo(detDesc.getDescription(i));
        }
    }

}
