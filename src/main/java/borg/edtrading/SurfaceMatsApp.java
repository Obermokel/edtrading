package borg.edtrading;

import boofcv.alg.color.ColorHsv;
import boofcv.alg.feature.detect.edge.CannyEdge;
import boofcv.alg.filter.blur.BlurImageOps;
import boofcv.factory.feature.detect.edge.FactoryEdgeDetectors;
import boofcv.gui.binary.VisualizeBinaryData;
import boofcv.gui.image.VisualizeImageData;
import boofcv.io.image.ConvertBufferedImage;
import boofcv.struct.image.GrayF32;
import boofcv.struct.image.GrayS16;
import boofcv.struct.image.GrayU8;
import boofcv.struct.image.Planar;
import borg.edtrading.boofcv.Template;
import borg.edtrading.boofcv.TemplateMatch;
import borg.edtrading.boofcv.TemplateMatcher;
import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.util.ImageUtil;
import borg.edtrading.util.MatchSorter;
import borg.edtrading.util.MatchSorter.MatchGroup;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;

/**
 * SurfaceMatsApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SurfaceMatsApp {

    static final Logger logger = LogManager.getLogger(SurfaceMatsApp.class);

    private static final Pattern MATERIAL_PATTERN = Pattern.compile("([A-Z]+)\\((\\d+\\.\\d)%\\).?");

    public static void main(String[] args) throws IOException {
        List<Template> matsTemplates = TemplateMatcher.loadTemplates("Surface Mats");
        List<Template> nameTemplates = TemplateMatcher.loadTemplates("Planet Names");

        scanNewSystemMapScreenshots(matsTemplates, nameTemplates);
    }

    private static void scanNewSystemMapScreenshots(List<Template> matsTemplates, List<Template> nameTemplates) throws IOException {
        List<ScannedSurfaceMat> scannedSurfaceMats = new ArrayList<>();

        //List<Template> templates = templates.stream().filter(t -> t.getText().matches("\\w")).collect(Collectors.toList());
        List<File> screenshotFiles = getScreenshotsFromAllDir();
        for (File screenshotFile : screenshotFiles) {
            BufferedImage originalImage = ImageIO.read(screenshotFile);
            BufferedImage fourKImage = ImageUtil.toFourK(originalImage); // Scale up to 4K to make canny edge detection easier

            BufferedImage croppedImage = fourKImage.getSubimage(20, 1600, 820, 420); // Crop to planet mats
            BufferedImage cannyImage = cannyEdge(croppedImage);
            BufferedImage whiteTextImage = keepWhiteTextOnly(croppedImage);
            BufferedImage outlinedTextImage = outlineText(whiteTextImage, cannyImage);
            BufferedImage blurredTextImage = gaussian(outlinedTextImage);

            BufferedImage croppedNameImage = fourKImage.getSubimage(2100, 640, 900, 56); // Crop to planet name
            BufferedImage cannyNameImage = cannyEdge(croppedNameImage);
            BufferedImage whiteTextNameImage = keepWhiteTextOnly(croppedNameImage);
            BufferedImage outlinedTextNameImage = outlineText(whiteTextNameImage, cannyNameImage);
            BufferedImage blurredTextNameImage = gaussian(outlinedTextNameImage);

            // >>>> START DEBUG >>>>
            //            ImageIO.write(fourKImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_0fourK.png")));
            //            ImageIO.write(croppedImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_1a_cropped.png")));
            //            ImageIO.write(croppedNameImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_1b_cropped.png")));
            //            ImageIO.write(cannyImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_2a_canny.png")));
            //            ImageIO.write(cannyNameImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_2b_canny.png")));
            //            ImageIO.write(whiteTextImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_3a_whiteText.png")));
            //            ImageIO.write(whiteTextNameImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_3b_whiteText.png")));
            //            ImageIO.write(outlinedTextImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_4a_outlinedText.png")));
            //            ImageIO.write(outlinedTextNameImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_4b_outlinedText.png")));
            ImageIO.write(blurredTextImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_5a_blurredText.png")));
            ImageIO.write(blurredTextNameImage, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_5b_blurredText.png")));
            // <<<< END DEBUG <<<<

            List<TemplateMatch> matches = findTemplateMatches(blurredTextImage, matsTemplates, 50);
            if (matches.size() >= 20) {
                List<MatchGroup> matchGroups = MatchSorter.sortMatches(matches);

                String planetName = extractPlanetName(blurredTextNameImage, nameTemplates);
                ImageIO.write(blurredTextNameImage, "PNG", new File(Constants.TEMP_DIR, planetName + ".png"));

                //                // >>>> START DEBUG >>>>
                //                BufferedImage out = new BufferedImage(blurredTextImage.getWidth(), blurredTextImage.getHeight(), BufferedImage.TYPE_INT_RGB);
                //                Graphics2D g = out.createGraphics();
                //                // source image
                //                g.drawImage(blurredTextImage, 0, 0, null);
                //                // bounding boxes
                //                g.setColor(Color.GRAY);
                //                for (TemplateMatch m : matches) {
                //                    g.drawRect(m.getMatch().x, m.getMatch().y, m.getTemplate().getImage().width, m.getTemplate().getImage().height);
                //                }
                //                // individual chars
                //                g.setColor(Color.YELLOW);
                //                g.setFont(new Font("Arial", Font.PLAIN, 12));
                //                for (TemplateMatch m : matches) {
                //                    g.drawString(m.getTemplate().getText(), m.getMatch().x, m.getMatch().y);
                //                }
                //                // grouped matches
                //                g.setColor(Color.RED);
                //                g.setFont(new Font("Arial", Font.PLAIN, 14));
                //                for (MatchGroup mg : matchGroups) {
                //                    g.drawString(mg.getText(), mg.getX(), mg.getY() + 14);
                //                }
                //                ImageIO.write(out, "PNG", new File(Constants.TEMP_DIR, screenshotFile.getName().replace(".png", "_9matches.png")));
                //                g.dispose();
                //                // <<<< END DEBUG <<<<

                Iterator<MatchGroup> it = matchGroups.iterator();
                while (it.hasNext()) {
                    MatchGroup mg = it.next();
                    String text = mg.getText();
                    Matcher m = MATERIAL_PATTERN.matcher(text);
                    if (!m.matches()) {
                        if (printUnrecognizedTextWarning(text.toLowerCase())) {
                            logger.debug("Not a material text: '" + text + "' in screenshot " + screenshotFile.getName());
                        }
                    } else {
                        String elementText = m.group(1);
                        String percentText = m.group(2);

                        Item element = findBestMatchingElement(elementText);
                        if (element == null) {
                            if (elementText.length() > 1) {
                                logger.debug("Unknown element '" + elementText + "' in screenshot " + screenshotFile.getName());
                            }
                        } else {
                            BigDecimal percentage = new BigDecimal(percentText);

                            //logger.info("Found " + percentage + "% " + element.getName() + " in " + screenshotFile.getName());

                            ScannedSurfaceMat ssm = new ScannedSurfaceMat(screenshotFile, element, percentage, planetName);
                            scannedSurfaceMats.add(ssm);

                            File dir = new File(Constants.TEMP_DIR, ssm.getElement().getName());
                            File copiedFile = new File(dir, String.format(Locale.US, "%04.1f_%s", ssm.getPercentage(), ssm.getScreenshotFile().getName()));
                            // TODO FileUtils.copyFile(ssm.getScreenshotFile(), copiedFile);
                        }
                    }
                }
            }

            //            BufferedImage firstRow = fullHdInventory.getSubimage(18, 18, 520, 70);
            //            ImageIO.write(firstRow, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_1stRow.png")));
            //            BufferedImage secondRow = fullHdInventory.getSubimage(16, 90, 520, 70);
            //            ImageIO.write(secondRow, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_2ndRow.png")));
            //            BufferedImage thirdRow = fullHdInventory.getSubimage(14, 160, 520, 70);
            //            ImageIO.write(thirdRow, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_3rdRow.png")));
            //            BufferedImage fourthRow = fullHdInventory.getSubimage(10, 234, 520, 70);
            //            ImageIO.write(fourthRow, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_4thRow.png")));
            //            BufferedImage fifthRow = fullHdInventory.getSubimage(8, 304, 520, 70);
            //            ImageIO.write(fifthRow, "PNG", new File(Constants.TEMP_DIR, unscannedScreenshotFile.getName().replace(".png", "_5thRow.png")));

            //            TemplateMatch match = isInventoryScreenshot(inventoryImage, inventoryTemplates);
            //            if (match != null) {
            //                logger.info(unscannedScreenshotFile + ": epp=" + match.getErrorPerPixel() + " / q=" + match.getMatchQuality());
            //            }
        }

        // Identify highest occurences
        Map<Item, ScannedSurfaceMat> highestOccurences = new TreeMap<>();
        Map<Item, List<BigDecimal>> allOccurences = new TreeMap<>();
        int nPlanets = screenshotFiles.size(); // Each screenshot represents a planet
        for (ScannedSurfaceMat current : scannedSurfaceMats) {
            ScannedSurfaceMat bestSoFar = highestOccurences.get(current.getElement());
            if (bestSoFar == null || current.isHigher(bestSoFar)) {
                highestOccurences.put(current.getElement(), current);
            }
            List<BigDecimal> all = allOccurences.get(current.getElement());
            if (all == null) {
                all = new ArrayList<>();
                allOccurences.put(current.getElement(), all);
            }
            all.add(current.getPercentage());
        }
        System.out.println("\n>>>> >>>> >>>> >>>> RESULTS FROM " + nPlanets + " PLANETS <<<< <<<< <<<< <<<<\n");
        System.out.println(String.format(Locale.US, "%-15s %10s %10s %10s", "ELEMENT", "HIGHEST", "MEDIAN", "OCCURENCE"));
        System.out.println(String.format(Locale.US, "%-15s %10s %10s %10s", "---------------", "----------", "----------", "----------"));
        for (Item element : highestOccurences.keySet()) {
            String name = highestOccurences.get(element).getPlanetName();
            BigDecimal highest = highestOccurences.get(element).getPercentage();
            List<BigDecimal> all = allOccurences.get(element);
            Collections.sort(all);
            BigDecimal median = all.get(all.size() / 2);
            BigDecimal frequency = new BigDecimal(all.size()).multiply(new BigDecimal(100)).divide(new BigDecimal(nPlanets), 1, BigDecimal.ROUND_HALF_UP);

            System.out.println(String.format(Locale.US, "%-15s %9.1f%% %9.1f%% %9.1f%%    %s", element.getName(), highest, median, frequency, name));
        }
    }

    private static String extractPlanetName(BufferedImage blurredTextImage, List<Template> templates) {
        List<TemplateMatch> matches = findTemplateMatches(blurredTextImage, templates, 10);
        if (matches.size() >= 3) {
            List<MatchGroup> matchGroups = MatchSorter.sortMatches(matches);
            StringBuilder name = new StringBuilder();
            for (MatchGroup mg : matchGroups) {
                name.append(mg.getText()).append(" ");
            }
            //            logger.debug(name);
            //            for (MatchGroup mg : matchGroups) {
            //                for (TemplateMatch m : mg.getGroupMatches()) {
            //                    logger.debug(m.getTemplate().getText() + "    " + m.getErrorPerPixel());
            //                }
            //            }
            return name.toString().trim();
        }
        return null;
    }

    private static boolean printUnrecognizedTextWarning(String lowerCaseText) {
        if (lowerCaseText.length() <= 2) {
            return false;
        } else if (lowerCaseText.matches("[\\-\\d\\.]+.?[d°]?.?")) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "rotational") <= 2) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "period:") <= 2) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "rotational:period:") <= 4) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "(tidally") <= 2) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "locked)") <= 2) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "(tidally:locked)") <= 3) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "axial") <= 2) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "tilt:") <= 2) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "axial:tilt:") <= 3) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "arg") <= 1) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "periapsis") <= 2) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "planet") <= 2) {
            return false;
        } else if (StringUtils.getLevenshteinDistance(lowerCaseText, "materials") <= 2) {
            return false;
        } else {
            return true;
        }
    }

    private static Item findBestMatchingElement(String name) {
        Item bestItem = null;
        float bestItemError = Float.MAX_VALUE;
        for (Item item : Item.values()) {
            if (item.getType() == ItemType.ELEMENT) {
                float dist = StringUtils.getLevenshteinDistance(name.toLowerCase(), item.getName().toLowerCase());
                float len = item.getName().length();
                float err = dist / len;
                if (err <= 0.25f) {
                    if (err < bestItemError) {
                        bestItem = item;
                        bestItemError = err;
                    }
                }
            }
        }
        return bestItem;
    }

    private static BufferedImage cannyEdge(BufferedImage originalImage) {
        GrayU8 gray = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
        GrayU8 edge = gray.createSameShape();
        CannyEdge<GrayU8, GrayS16> canny = FactoryEdgeDetectors.canny(3, true, true, GrayU8.class, GrayS16.class);
        canny.process(gray, 0.1f, 0.3f, edge);
        return VisualizeBinaryData.renderBinary(edge, false, null);
    }

    private static BufferedImage keepWhiteTextOnly(BufferedImage image) {
        //float hue = 0f;
        float saturation = 0f;
        float value = 255f;

        Planar<GrayF32> input = ConvertBufferedImage.convertFromMulti(image, null, true, GrayF32.class);
        Planar<GrayF32> hsv = input.createSameShape();

        // Convert into HSV
        ColorHsv.rgbToHsv_F32(input, hsv);

        // Euclidean distance squared threshold for deciding which pixels are members of the selected set
        float maxDist2 = 0.5f * 0.5f;

        // Extract hue and saturation bands which are independent of intensity
        //GrayF32 H = hsv.getBand(0);
        GrayF32 S = hsv.getBand(1);
        GrayF32 V = hsv.getBand(2);

        // step through each pixel and mark how close it is to the selected color
        BufferedImage output = new BufferedImage(input.width, input.height, BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < hsv.height; y++) {
            for (int x = 0; x < hsv.width; x++) {
                // Hue is an angle in radians, so simple subtraction doesn't work
                //float dh = UtilAngle.dist(H.unsafe_get(x, y), hue) / (float) Math.PI;
                float ds = (S.unsafe_get(x, y) - saturation);
                float dv = (V.unsafe_get(x, y) - value) / 255f;

                // this distance measure is a bit naive, but good enough for to demonstrate the concept
                //float dist2 = dh * dh + ds * ds + dv * dv;
                float dist2 = ds * ds + dv * dv;
                if (dist2 <= maxDist2) {
                    output.setRGB(x, y, Color.WHITE.getRGB());
                }
            }
        }

        return output;
    }

    private static BufferedImage outlineText(BufferedImage whiteTextImage, BufferedImage cannyImage) {
        BufferedImage output = new BufferedImage(whiteTextImage.getWidth(), whiteTextImage.getHeight(), BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < whiteTextImage.getHeight(); y++) {
            for (int x = 0; x < whiteTextImage.getWidth(); x++) {
                int rgb = whiteTextImage.getRGB(x, y);
                if (cannyImage.getRGB(x, y) != Color.BLACK.getRGB()) {
                    rgb = Color.GRAY.getRGB();
                }
                output.setRGB(x, y, rgb);
            }
        }
        return output;
    }

    private static BufferedImage gaussian(BufferedImage originalImage) {
        GrayU8 gray = ConvertBufferedImage.convertFrom(originalImage, (GrayU8) null);
        GrayU8 gaussian = BlurImageOps.gaussian(gray, null, /* sigma = */ -1, /* radius = */ 1, null);
        return VisualizeImageData.grayMagnitude(gaussian, null, -1);
    }

    private static List<TemplateMatch> findTemplateMatches(BufferedImage outlinedTextImage, List<Template> templates, int maxMatches) {
        // Find all matches
        List<TemplateMatch> matches = new ArrayList<>();
        for (Template t : templates) {
            matches.addAll(TemplateMatcher.findTemplateMatches(outlinedTextImage, t, maxMatches));
        }

        // Sort by score
        Collections.sort(matches, new Comparator<TemplateMatch>() {
            @Override
            public int compare(TemplateMatch m1, TemplateMatch m2) {
                // Real score
                //return new Double(-1 * m1.getMatch().score).compareTo(new Double(-1 * m2.getMatch().score));

                // Unprecise score/pixel
                //return new Integer(m1.getMatchQuality()).compareTo(new Integer(m2.getMatchQuality()));

                //                // Precise score/pixel
                //                return new Double(-1 * m1.getMatch().score / (m1.getTemplate().getImage().width * m1.getTemplate().getImage().height))
                //                        .compareTo(new Double(-1 * m2.getMatch().score / (m2.getTemplate().getImage().width * m2.getTemplate().getImage().height)));

                // Precise score/width
                return new Double(m1.getErrorPerWidth()).compareTo(new Double(m2.getErrorPerWidth()));
            }
        });

        // Select all non-overlapping with a good quality
        List<TemplateMatch> nonOverlapping = new ArrayList<>();
        for (TemplateMatch m : matches) {
            double goodQuality = m.getTemplate().getText().matches("\\w+") ? 10000 : 2000; // Non-text must have a much lower error/pixel
            if (m.getErrorPerPixel() < goodQuality) {
                if (!m.overlapsWithAny(nonOverlapping)) {
                    nonOverlapping.add(m);
                }
            }
        }

        // Finished
        return nonOverlapping;
    }

    private static List<File> getScreenshotsFromAllDir() {
        File allDir = new File(Constants.SURFACE_MATS_DIR, "_ALL_");
        File[] screenshotFiles = allDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().endsWith(".png");
            }
        });
        List<File> sortableList = new ArrayList<>(Arrays.asList(screenshotFiles));
        Collections.sort(sortableList, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                return f1.getName().toLowerCase().compareTo(f2.getName().toLowerCase());
            }
        });
        return sortableList;
    }

    public static class ScannedSurfaceMat {

        private final File screenshotFile;
        private final Item element;
        private final BigDecimal percentage;
        private final String planetName;

        public ScannedSurfaceMat(File screenshotFile, Item element, BigDecimal percentage, String planetName) {
            this.screenshotFile = screenshotFile;
            this.element = element;
            this.percentage = percentage;
            this.planetName = planetName;
        }

        @Override
        public String toString() {
            String name = this.getPlanetName();
            if (name == null) {
                name = this.getScreenshotFile().getName();
            }
            return String.format(Locale.US, "%-20s\t%04.1f%% on %s", this.getElement().getName() + ":", this.getPercentage(), name);
        }

        public File getScreenshotFile() {
            return this.screenshotFile;
        }

        public Item getElement() {
            return this.element;
        }

        public BigDecimal getPercentage() {
            return this.percentage;
        }

        public String getPlanetName() {
            return this.planetName;
        }

        public boolean isHigher(ScannedSurfaceMat other) {
            return this.getPercentage().compareTo(other.getPercentage()) > 0;
        }

    }

}
