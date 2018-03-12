package borg.edtrading;

import boofcv.abst.feature.associate.AssociateDescription;
import boofcv.abst.feature.associate.ScoreAssociation;
import boofcv.abst.feature.detdesc.DetectDescribePoint;
import boofcv.abst.feature.detect.interest.ConfigFastHessian;
import boofcv.abst.tracker.TrackerObjectQuad;
import boofcv.alg.descriptor.UtilFeature;
import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.factory.feature.associate.FactoryAssociation;
import boofcv.factory.feature.detdesc.FactoryDetectDescribe;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.factory.tracker.FactoryTrackerObjectQuad;
import boofcv.gui.image.VisualizeImageData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.feature.AssociatedIndex;
import boofcv.struct.feature.BrightFeature;
import boofcv.struct.feature.TupleDesc;
import boofcv.struct.image.GrayS16;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.ImageGray;
import georegression.struct.point.Point2D_F64;
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics;
import org.ddogleg.struct.FastQueue;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.imageio.ImageIO;

public class ExampleAssociatePointsModified<T extends ImageGray<T>, TD extends TupleDesc> {

    // algorithm used to detect and describe interest points
    DetectDescribePoint<T, TD> detDesc;
    // Associated descriptions together by minimizing an error metric
    AssociateDescription<TD> associate;

    // location of interest points
    public List<Point2D_F64> pointsA;
    public List<Point2D_F64> pointsB;

    Class<T> imageType;

    public ExampleAssociatePointsModified(DetectDescribePoint<T, TD> detDesc, AssociateDescription<TD> associate, Class<T> imageType) {
        this.detDesc = detDesc;
        this.associate = associate;
        this.imageType = imageType;
    }

    //    /**
    //     * Detect and associate point features in the two images.  Display the results.
    //     */
    //    public void associate(BufferedImage imageA, BufferedImage imageB) {
    //        T inputA = ConvertBufferedImage.convertFromSingle(imageA, null, imageType);
    //        T inputB = ConvertBufferedImage.convertFromSingle(imageB, null, imageType);
    //
    //        // stores the location of detected interest points
    //        pointsA = new ArrayList<>();
    //        pointsB = new ArrayList<>();
    //
    //        // stores the description of detected interest points
    //        FastQueue<TD> descA = UtilFeature.createQueue(detDesc, 100);
    //        FastQueue<TD> descB = UtilFeature.createQueue(detDesc, 100);
    //
    //        // describe each image using interest points
    //        describeImage(inputA, pointsA, descA);
    //        describeImage(inputB, pointsB, descB);
    //
    //        // Associate features between the two images
    //        associate.setSource(descA);
    //        associate.setDestination(descB);
    //        associate.associate();
    //
    //        // display the results
    //        AssociationPanel panel = new AssociationPanel(20);
    //        FastQueue<AssociatedIndex> matches = associate.getMatches();
    //        panel.setAssociation(pointsA, pointsB, matches);
    //        panel.setImages(imageA, imageB);
    //
    //        //ShowImages.showWindow(panel, "Associated Features", true);
    //
    //        for (int i = 0; i < matches.size(); i++) {
    //            AssociatedIndex associatedIndex = matches.get(i);
    //            Point2D_F64 pointA = pointsA.get(associatedIndex.src);
    //            Point2D_F64 pointB = pointsB.get(associatedIndex.dst);
    //            double dx = pointB.x - pointA.x;
    //            double dy = pointB.y - pointA.y;
    //            System.out.println(String.format(Locale.US, "%.1f / %.1f = %.4f", dx, dy, associatedIndex.fitScore));
    //        }
    //    }

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

    public static void main(String args[]) throws IOException {
        File ref_cropped = new File("C:\\Users\\Guenther\\ref_cropped.png");
        File test = new File("C:\\Users\\Guenther\\test_false.png");

        locateObjectInScreenshot(ref_cropped, test);
        //        Class imageType = GrayF32.class;
        //        //      Class imageType = GrayU8.class;
        //
        //        // select which algorithms to use
        //        DetectDescribePoint detDesc = FactoryDetectDescribe.surfStable(new ConfigFastHessian(1, 2, 300, 1, 9, 4, 4), null, null, imageType);
        //        //              sift(new ConfigCompleteSift(0,5,600));
        //
        //        ScoreAssociation scorer = FactoryAssociation.defaultScore(detDesc.getDescriptionType());
        //        AssociateDescription associate = FactoryAssociation.greedy(scorer, Double.MAX_VALUE, true);
        //
        //        // load and match images
        //        ExampleAssociatePoints app = new ExampleAssociatePoints(detDesc, associate, imageType);
        //
        //        BufferedImage imageA = UtilImageIO.loadImage("C:\\Users\\Guenther\\ref_cleaned_cropped.png");
        //        BufferedImage imageB = UtilImageIO.loadImage("C:\\Users\\Guenther\\test_false.png");
        //
        //        File ref = new File("C:\\Users\\Guenther\\ref.png");
        //        writeGrayImage(ref);
        //        writeCannyImage(ref);
        //
        //        app.associate(imageA, imageB);
    }

    private static Rectangle locateObjectInScreenshot(File objectImageFile, File screenshotImageFile) throws IOException {
        BufferedImage objectImage = ImageIO.read(objectImageFile);
        BufferedImage screenshotImage = ImageIO.read(screenshotImageFile);

        CannyEdge<GrayU8, GrayS16> canny = FactoryEdgeDetectors.canny(2, true, true, GrayU8.class, GrayS16.class);
        GrayU8 objectGray = ConvertBufferedImage.convertFrom(objectImage, (GrayU8) null);
        GrayU8 objectCanny = objectGray.createSameShape();
        canny.process(objectGray, 0.1f, 0.3f, objectCanny);
        GrayU8 screenshotGray = ConvertBufferedImage.convertFrom(screenshotImage, (GrayU8) null);
        GrayU8 screenshotCanny = screenshotGray.createSameShape();
        canny.process(screenshotGray, 0.1f, 0.3f, screenshotCanny);

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
        System.out.println("Total matches: " + matches.size());
        DescriptiveStatistics statsX = new DescriptiveStatistics();
        DescriptiveStatistics statsY = new DescriptiveStatistics();
        //        double[] valuesX = new double[matches.size()];
        //        double[] valuesY = new double[matches.size()];
        for (int i = 0; i < matches.size(); i++) {
            AssociatedIndex associatedIndex = matches.get(i);
            if (associatedIndex.fitScore < 0.1) {
                Point2D_F64 pointA = pointsA.get(associatedIndex.src);
                Point2D_F64 pointB = pointsB.get(associatedIndex.dst);
                double dx = pointB.x - pointA.x;
                double dy = pointB.y - pointA.y;
                System.out.println(String.format(Locale.US, "%.1f / %.1f = %.4f", dx, dy, associatedIndex.fitScore));
                //            valuesX[i] = dx;
                //            valuesY[i] = dy;
                statsX.addValue(dx);
                statsY.addValue(dy);
            }
        }
        TrackerObjectQuad tracker = FactoryTrackerObjectQuad.circulant(null, GrayU8.class);
        //        Percentile median = new Percentile();
        //        StandardDeviation stddev = new StandardDeviation();
        //        double stddevX = stddev.evaluate(valuesX, median.evaluate(valuesX));
        //        double stddevY = stddev.evaluate(valuesY, median.evaluate(valuesY));
        //        System.out.println(String.format(Locale.US, "stddev: x=%.3f / y=%.3f", stddevX, stddevY));
        System.out.println("var.x=" + Math.sqrt(statsX.getVariance()));
        System.out.println("var.y=" + Math.sqrt(statsY.getVariance()));
        System.out.println("popvar.x=" + Math.sqrt(statsX.getPopulationVariance()));
        System.out.println("popvar.y=" + Math.sqrt(statsY.getPopulationVariance()));
        System.out.println("stddev.x=" + statsX.getStandardDeviation());
        System.out.println("stddev.y=" + statsY.getStandardDeviation());
        System.out.println("skew.x=" + statsX.getSkewness());
        System.out.println("skew.y=" + statsY.getSkewness());

        return null;
    }

    static void writeGrayImage(File file) {
        try {
            BufferedImage originalImage = ImageIO.read(file);
            GrayU8 grayImage = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
            BufferedImage modifiedImage = ConvertBufferedImage.convertTo(grayImage, null);
            ImageIO.write(modifiedImage, "PNG", new File(file.getAbsolutePath().replace(".png", "_gray.png")));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static void writeCannyImage(File file) {
        try {
            BufferedImage originalImage = ImageIO.read(file);
            GrayU8 grayImage = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
            GrayU8 outputGrayU8 = grayImage.createSameShape();
            CannyEdge<GrayU8, GrayS16> canny = FactoryEdgeDetectors.canny(2, true, true, GrayU8.class, GrayS16.class);
            canny.process(grayImage, 0.1f, 0.3f, outputGrayU8);
            BufferedImage modifiedImage = VisualizeImageData.grayMagnitude(outputGrayU8, null, -1);
            ImageIO.write(modifiedImage, "PNG", new File(file.getAbsolutePath().replace(".png", "_canny.png")));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
