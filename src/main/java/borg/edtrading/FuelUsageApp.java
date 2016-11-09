package borg.edtrading;

import borg.edtrading.journal.JournalReader;
import borg.edtrading.util.FuelAndJumpRangeLookup;
import com.google.gson.Gson;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileFilter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;

/**
 * FuelUsageApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FuelUsageApp {

    static final Logger logger = LogManager.getLogger(FuelUsageApp.class);

    private static final DateFormat DF = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");

    public static void main(String[] args) throws Exception {
        // Altair to Jaques:
        final Date fromDate = DF.parse("2016-11-06T21:44:46Z");
        final Date toDate = DF.parse("2016-11-07T22:03:52Z");
        //        // Jaques to beacon and back:
        //        final Date fromDate = DF.parse("2016-11-08T05:48:47Z");
        //        final Date toDate = DF.parse("2016-11-08T22:58:12Z");
        final int maxFuelTons = 88;
        final float maxFuelPerJump = 8.32f;
        final float jumpRangeFuelFull = 48.30f;
        final float jumpRangeFuelOpt = 54.53f;

        FuelAndJumpRangeLookup lut = new FuelAndJumpRangeLookup(maxFuelTons, maxFuelPerJump, jumpRangeFuelFull, jumpRangeFuelOpt);
        lut.writeFuelUsageCsv(new File(Constants.TEMP_DIR, "fuelUsage.csv"));
        lut.writeJumpRangeCsv(new File(Constants.TEMP_DIR, "jumpRange.csv"));

        File[] journalFiles = Constants.JOURNAL_DIR.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                return file.getName().endsWith(".log");
            }
        });
        Arrays.sort(journalFiles, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                return new Long(f1.lastModified()).compareTo(new Long(f2.lastModified()));
            }
        });
        File fuelUsageCsvFile = new File(Constants.TEMP_DIR, "fuelByDist.csv");
        FileUtils.write(fuelUsageCsvFile, "distPercent;fuelPercent\r\n", "ISO-8859-1", false);
        File jumpTimeCsvFile = new File(Constants.TEMP_DIR, "jumpTimes.csv");
        FileUtils.write(jumpTimeCsvFile, "n;seconds\r\n", "ISO-8859-1", false);

        Gson gson = new Gson();
        Date prevTimestamp = null;
        List<Long> jumpTimes = new ArrayList<>();
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

                        float maxJumpRange = FuelAndJumpRangeLookup.estimateCurrentJumpRange(fuelBefore, maxFuelTons, maxFuelPerJump, jumpRangeFuelFull, jumpRangeFuelOpt);
                        float jumpPercent = 100f * jumpDist / maxJumpRange;
                        float fuelPercent = 100f * fuelUsed / maxFuelPerJump;

                        logger.debug(String.format(Locale.US, "dist=%.2fly (%.1f%%), used=%.2ft (%.1f%%), left=%.2ft", jumpDist, jumpPercent, fuelUsed, fuelPercent, fuelAfter));
                        FileUtils.write(fuelUsageCsvFile, String.format(Locale.GERMANY, "%.1f;%.1f\r\n", jumpPercent, fuelPercent), "ISO-8859-1", true);

                        if (prevTimestamp != null) {
                            jumpTimes.add(timestamp.getTime() - prevTimestamp.getTime());
                            FileUtils.write(jumpTimeCsvFile, String.format(Locale.GERMANY, "%d;%d\r\n", jumpTimes.size(), (timestamp.getTime() - prevTimestamp.getTime()) / 1000L), "ISO-8859-1", true);
                        }
                        prevTimestamp = timestamp;
                    }
                }
            }
        }
        Collections.sort(jumpTimes);
        logger.debug(String.format(Locale.US, "%d jumps, median = %d seconds", jumpTimes.size(), jumpTimes.get((jumpTimes.size() * 5) / 10) / 1000L));

        JournalReader.readEntireJournal(Constants.JOURNAL_DIR);
    }

}
