package borg.edtrading;

import borg.edtrading.bodyscanner.ScannedBodyInfo;
import borg.edtrading.data.Item;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

/**
 * BodyScannerApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyScannerApp {

    static final Logger logger = LogManager.getLogger(BodyScannerApp.class);

    public static void main(String[] args) {

    }

    static void printStats(List<ScannedBodyInfo> scannedBodyInfos) {
        Map<Item, ScannedBodyInfo> highestOccurences = new TreeMap<>();
        Map<Item, List<BigDecimal>> allOccurences = new TreeMap<>();
        int nPlanets = 0; // Each screenshot represents a planet. However we only count successfully scanned screenshots.
        for (ScannedBodyInfo bi : scannedBodyInfos) {
            if (bi.getPlanetMaterials() != null && bi.getPlanetMaterials().size() > 0) {
                nPlanets++;
                for (Item element : bi.getPlanetMaterials().keySet()) {
                    BigDecimal percentage = bi.getPlanetMaterials().get(element);

                    ScannedBodyInfo bestSoFar = highestOccurences.get(element);
                    if (bestSoFar == null || percentage.compareTo(bestSoFar.getPlanetMaterials().get(element)) > 0) {
                        highestOccurences.put(element, bi);
                    }

                    List<BigDecimal> all = allOccurences.get(element);
                    if (all == null) {
                        all = new ArrayList<>();
                        allOccurences.put(element, all);
                    }
                    all.add(percentage);
                }
            }
        }
        System.out.println("\n>>>> >>>> >>>> >>>> RESULTS FROM " + nPlanets + " PLANETS <<<< <<<< <<<< <<<<\n");
        System.out.println(String.format(Locale.US, "%-15s %10s %10s %10s", "ELEMENT", "HIGHEST", "MEDIAN", "OCCURENCE"));
        System.out.println(String.format(Locale.US, "%-15s %10s %10s %10s", "---------------", "----------", "----------", "----------"));
        for (Item element : highestOccurences.keySet()) {
            String name = highestOccurences.get(element).getBodyName();
            BigDecimal highest = highestOccurences.get(element).getPlanetMaterials().get(element);
            List<BigDecimal> all = allOccurences.get(element);
            Collections.sort(all);
            BigDecimal median = all.get(all.size() / 2);
            BigDecimal frequency = new BigDecimal(all.size()).multiply(new BigDecimal(100)).divide(new BigDecimal(nPlanets), 1, BigDecimal.ROUND_HALF_UP);

            System.out.println(String.format(Locale.US, "%-15s %9.1f%% %9.1f%% %9.1f%%    %s", element.getName(), highest, median, frequency, name));
        }
    }

}
