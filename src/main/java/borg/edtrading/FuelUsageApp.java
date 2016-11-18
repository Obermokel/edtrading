package borg.edtrading;

import borg.edtrading.data.Coord;
import borg.edtrading.journal.Event;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalReader;
import borg.edtrading.journal.entries.location.FSDJumpEntry;
import borg.edtrading.util.FuelAndJumpRangeLookup;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
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
        //        // Altair to Jaques:
        //        final Date fromDate = DF.parse("2016-11-06T21:44:46Z");
        //        final Date toDate = DF.parse("2016-11-07T22:03:52Z");
        //        // Jaques to beacon and back:
        //        final Date fromDate = DF.parse("2016-11-08T05:48:47Z");
        //        final Date toDate = DF.parse("2016-11-08T22:58:12Z");
        // Jaques to Game Crash:
        final Date fromDate = DF.parse("2016-11-09T19:03:11Z");
        final Date toDate = DF.parse("2016-11-09T23:36:14Z");
        final int maxFuelTons = 88;
        final float maxFuelPerJump = 8.32f;
        final float jumpRangeFuelFull = 48.30f;
        final float jumpRangeFuelOpt = 54.53f;

        FuelAndJumpRangeLookup lut = new FuelAndJumpRangeLookup(maxFuelTons, maxFuelPerJump, jumpRangeFuelFull, jumpRangeFuelOpt);
        lut.writeFuelUsageCsv(new File(Constants.TEMP_DIR, "fuelUsage.csv"));
        lut.writeJumpRangeCsv(new File(Constants.TEMP_DIR, "jumpRange.csv"));

        Journal journal = new Journal(new JournalReader().readEntireJournal(Constants.JOURNAL_DIR));
        List<FSDJumpEntry> fsdJumpEntries = MiscUtil.unsafeCast(journal.getEntries(fromDate, toDate, Event.FSDJump));

        int jumpNo = 0;
        String fromName = "Colonia";
        Coord fromCoord = new Coord(-9530.500f, -910.281f, 19808.125f); // Colonia
        float trackedFuelLevelBeforeJumping = maxFuelTons;
        for (FSDJumpEntry entry : fsdJumpEntries) {
            jumpNo++;
            String jumpSymbol = MiscUtil.getAsFloat(entry.getBoostUsed(), 1f) > 1f ? "*→" : "→";
            String toName = entry.getStarSystem();
            Coord toCoord = entry.getStarPos();
            logger.debug(String.format(Locale.US, "#%d %s %s %s (%.2f Ly)", jumpNo, fromName, jumpSymbol, toName, entry.getJumpDist()));

            // Compute distance and check for error
            float computedJumpDistance = fromCoord.distanceTo(toCoord);
            float actualJumpDistance = entry.getJumpDist();
            float errorJumpDistance = computedJumpDistance - actualJumpDistance;
            if (Math.abs(errorJumpDistance) >= 0.01f) {
                logger.warn(String.format(Locale.US, "Jump distance error of %.4f Ly: est: %.2f Ly; act: %.2f Ly", errorJumpDistance, computedJumpDistance, actualJumpDistance));
            }

            // Compute max jump range + normalize jump range + reset fuel when coming from non-neutron star
            float computedCurrentMaxJumpRange = FuelAndJumpRangeLookup.estimateCurrentJumpRange(trackedFuelLevelBeforeJumping, maxFuelTons, maxFuelPerJump, jumpRangeFuelFull, jumpRangeFuelOpt);
            float boostUsed = MiscUtil.getAsFloat(entry.getBoostUsed(), 1f);
            float computedNormalizedJumpRange = computedJumpDistance / boostUsed;
            if (boostUsed == 1f) {
                trackedFuelLevelBeforeJumping = maxFuelTons;
            }

            // Compute fuel usage and check for error
            float computedFuelUsage = FuelAndJumpRangeLookup.estimateFuelUsage(computedNormalizedJumpRange, computedCurrentMaxJumpRange, maxFuelPerJump);
            float actualFuelUsage = entry.getFuelUsed();
            float errorFuelUsage = computedFuelUsage - actualFuelUsage;
            if (Math.abs(errorFuelUsage) >= 0.1f) {
                logger.warn(String.format(Locale.US, "Fuel usage error of %.2ft: est: %.2ft; act: %.2ft", errorFuelUsage, computedFuelUsage, actualFuelUsage));
            }

            // Remove fuel and check for error
            trackedFuelLevelBeforeJumping -= computedFuelUsage;
            float actualFuelLevel = entry.getFuelLevel();
            float errorFuelLevel = trackedFuelLevelBeforeJumping - actualFuelLevel;
            if (Math.abs(errorFuelLevel) >= 0.1f) {
                logger.warn(String.format(Locale.US, "Fuel level error of %.2ft: est: %.2ft; act: %.2ft", errorFuelLevel, trackedFuelLevelBeforeJumping, actualFuelLevel));
            }

            logger.debug(String.format(Locale.US, "...now have %.2ft fuel and %.2f Ly base jump range", trackedFuelLevelBeforeJumping,
                    FuelAndJumpRangeLookup.estimateCurrentJumpRange(trackedFuelLevelBeforeJumping, maxFuelTons, maxFuelPerJump, jumpRangeFuelFull, jumpRangeFuelOpt)));

            // Continue
            fromName = toName;
            fromCoord = toCoord;
        }
    }

}
