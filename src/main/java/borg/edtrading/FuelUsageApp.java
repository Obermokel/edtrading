package borg.edtrading;

import borg.edtrading.util.FuelAndJumpRangeLookup;
import com.google.gson.Gson;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileFilter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * FuelUsageApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FuelUsageApp {

    static final Logger logger = LogManager.getLogger(FuelUsageApp.class);

    private static final DateFormat DF = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");

    public static void main(String[] args) throws Exception {
        final Date fromDate = DF.parse("2016-11-06T21:44:46Z");
        final Date toDate = DF.parse("2016-11-07T13:25:30Z");
        final float maxFuelPerJump = 8.32f;
        final SortedMap<Float, Float> jumpRanges = new TreeMap<>();
        jumpRanges.put(0f, 0.00f);
        jumpRanges.put(8f, 51.31f);
        jumpRanges.put(8.32f, 52.0f);
        jumpRanges.put(16f, 51.45f);
        jumpRanges.put(24f, 50.83f);
        jumpRanges.put(32f, 50.22f);
        jumpRanges.put(40f, 49.63f);
        jumpRanges.put(48f, 49.05f);
        jumpRanges.put(56f, 48.49f);
        jumpRanges.put(64f, 47.93f);

        File journalDir = new File(System.getProperty("user.home"), "Google Drive/Elite Dangerous/Journal");
        File[] journalFiles = journalDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().endsWith(".log");
            }
        });
        File csvFile = new File(Constants.TEMP_DIR, "fuelByDist.csv");
        FileUtils.write(csvFile, "distPercent;fuelPercent\r\n", "ISO-8859-1", false);

        Gson gson = new Gson();
        for (File journalFile : journalFiles) {
            List<String> lines = FileUtils.readLines(journalFile, "UTF-8");
            for (String line : lines) {
                LinkedHashMap obj = gson.fromJson(line, LinkedHashMap.class);

                Date timestamp = DF.parse((String) obj.get("timestamp"));
                if (timestamp.compareTo(fromDate) >= 0 && timestamp.compareTo(toDate) <= 0) {
                    String event = (String) obj.get("event");
                    if ("FSDJump".equals(event)) {
                        float jumpDist = ((Number) obj.get("JumpDist")).floatValue();
                        float fuelUsed = ((Number) obj.get("FuelUsed")).floatValue(); // Used for this jump
                        float fuelAfter = ((Number) obj.get("FuelLevel")).floatValue(); // Have after this jump
                        float fuelBefore = fuelAfter + fuelUsed;
                        if (obj.containsKey("BoostUsed")) {
                            jumpDist /= ((Number) obj.get("BoostUsed")).floatValue();
                        }

                        float maxJumpRange = FuelAndJumpRangeLookup.estimateCurrentJumpRange(fuelBefore, jumpRanges);
                        float jumpPercent = 100f * jumpDist / maxJumpRange;
                        float fuelPercent = 100f * fuelUsed / maxFuelPerJump;

                        logger.debug(String.format(Locale.US, "dist=%.2fly (%.1f%%), used=%.2ft (%.1f%%), left=%.2ft", jumpDist, jumpPercent, fuelUsed, fuelPercent, fuelAfter));
                        FileUtils.write(csvFile, String.format(Locale.GERMANY, "%.1f;%.1f\r\n", jumpPercent, fuelPercent), "ISO-8859-1", true);
                    }
                }
            }
        }
    }

}
