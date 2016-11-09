package borg.edtrading.util;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.SortedMap;
import java.util.TreeMap;

/**
 * FuelAndJumpRangeLookup
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FuelAndJumpRangeLookup {

    static final Logger logger = LogManager.getLogger(FuelAndJumpRangeLookup.class);

    private final float maxFuelTons;
    private final int maxFuelKg;
    private final float maxFuelPerJump;
    private final SortedMap<Integer, Float> fuelUsageByJumpPercent;
    private final SortedMap<Integer, Float> jumpRangeByFuelKg;
    private final float absoluteMinJumpRange;
    private final float absoluteMaxJumpRange;

    public FuelAndJumpRangeLookup(int maxFuelTons, float maxFuelPerJump, SortedMap<Float, Float> knownJumpRangesByFuelLevel) {
        this.maxFuelTons = maxFuelTons;
        this.maxFuelKg = maxFuelTons * 1000;
        this.maxFuelPerJump = maxFuelPerJump;
        this.fuelUsageByJumpPercent = buildFuelUsageLUT(maxFuelPerJump);
        this.jumpRangeByFuelKg = buildJumpRangeLUT(maxFuelTons, knownJumpRangesByFuelLevel);
        this.absoluteMinJumpRange = this.lookupMaxJumpRange(maxFuelTons);
        this.absoluteMaxJumpRange = this.lookupMaxJumpRange(maxFuelPerJump);
        //        for (float jumpRange : knownJumpRangesByFuelLevel.values()) {
        //            if (jumpRange > 0) {
        //                this.absoluteMinJumpRange = Math.min(this.absoluteMinJumpRange, jumpRange);
        //                this.absoluteMaxJumpRange = Math.max(this.absoluteMaxJumpRange, jumpRange);
        //            }
        //        }
    }

    public float getMaxFuelTons() {
        return this.maxFuelTons;
    }

    int getMaxFuelKg() {
        return this.maxFuelKg;
    }

    public float getMaxFuelPerJump() {
        return this.maxFuelPerJump;
    }

    public float getAbsoluteMinJumpRange() {
        return this.absoluteMinJumpRange;
    }

    public float getAbsoluteMaxJumpRange() {
        return this.absoluteMaxJumpRange;
    }

    private static SortedMap<Integer, Float> buildFuelUsageLUT(float maxFuelPerJump) {
        SortedMap<Integer, Float> result = new TreeMap<>();
        for (int percent = 0; percent <= 1000; percent += 1) {
            result.put(percent, maxFuelPerJump * (float) Math.pow(percent / 1000.0, 2.5));
        }
        return result;
    }

    private static SortedMap<Integer, Float> buildJumpRangeLUT(int maxFuelTons, SortedMap<Float, Float> knownJumpRangesByFuelLevel) {
        SortedMap<Integer, Float> result = new TreeMap<>();
        for (int kg = 0; kg <= (maxFuelTons * 1000); kg += 100) {
            result.put(kg, estimateCurrentJumpRange(kg / 1000f, knownJumpRangesByFuelLevel));
        }
        return result;
    }

    public static float estimateCurrentJumpRange(float currentFuelLevel, SortedMap<Float, Float> jumpRanges) {
        if (jumpRanges.containsKey(currentFuelLevel)) {
            return jumpRanges.get(currentFuelLevel);
        } else {
            Float nextLevelLessThanCurrent = null;
            Float nextLevelMoreThanCurrent = null;
            for (float fuelLevel : jumpRanges.keySet()) {
                if (fuelLevel <= currentFuelLevel && (nextLevelLessThanCurrent == null || fuelLevel > nextLevelLessThanCurrent)) {
                    nextLevelLessThanCurrent = fuelLevel;
                }
                if (fuelLevel >= currentFuelLevel && (nextLevelMoreThanCurrent == null || fuelLevel < nextLevelMoreThanCurrent)) {
                    nextLevelMoreThanCurrent = fuelLevel;
                }
            }
            float heavyJumpRange = jumpRanges.get(nextLevelMoreThanCurrent);
            float lightJumpRange = jumpRanges.get(nextLevelLessThanCurrent);
            float possibleGain = lightJumpRange - heavyJumpRange;
            float percent = 1f - ((currentFuelLevel - nextLevelLessThanCurrent) / (nextLevelMoreThanCurrent - nextLevelLessThanCurrent));
            return heavyJumpRange + percent * possibleGain;
        }
    }

    public float lookupMaxJumpRange(float fuelLevel) {
        int kgRounded = ((int) (fuelLevel * 10f)) * 100;
        if (kgRounded < 0) {
            return 0f;
        } else if (kgRounded > this.maxFuelKg) {
            return this.jumpRangeByFuelKg.get(this.maxFuelKg);
        } else {
            return this.jumpRangeByFuelKg.get(kgRounded);
        }
    }

    /**
     * Convenience method which lookups the max jump range by the given fuel level, then simply
     * calls {@link #lookupFuelUsageWithKnownMaxJumpRange(float, float)}. If you already know your
     * current max jump range use the other method directly.
     */
    public float lookupFuelUsage(float jumpDistance, float fuelLevel) {
        return this.lookupFuelUsageWithKnownMaxJumpRange(jumpDistance, this.lookupMaxJumpRange(fuelLevel));
    }

    /**
     * @param jumpDistance
     *      The distance you want to jump in Ly (0 &lt; jumpDistance &lt;= currentMaxJumpRange)
     * @param currentMaxJumpRange
     *      The maximum jump range you currently have in Ly
     * @return
     *      Fuel usage in tons
     */
    public float lookupFuelUsageWithKnownMaxJumpRange(float jumpDistance, float currentMaxJumpRange) {
        int jumpPercent = ((int) (jumpDistance / currentMaxJumpRange * 1000f));
        if (jumpPercent < 0) {
            return 0f;
        } else if (jumpPercent > 1000) {
            return this.fuelUsageByJumpPercent.get(1000);
        } else {
            return this.fuelUsageByJumpPercent.get(jumpPercent);
        }
    }

}
