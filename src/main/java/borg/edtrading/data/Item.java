package borg.edtrading.data;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Item
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public enum Item {

    //@formatter:off
    ABBERANT_SHIELD_PATTERN_ANALYSIS("ABBERANT SHIELD PATTERN ANALYSIS", ItemType.DATA, 0),
    ABNORMAL_COMPACT_EMISSIONS_DATA("ABNORMAL COMPACT EMISSIONS DATA", ItemType.DATA, 0),
    ANOMALOUS_BULK_SCAN_DATA("ANOMALOUS BULK SCAN DATA", ItemType.DATA, 0),
    ANOMALOUS_FSD_TELEMETRY("ANOMALOUS FSD TELEMETRY", ItemType.DATA, 0),
    ANTIMONY("ANTIMONY", ItemType.ELEMENT, 0),
    ARSENIC("ARSENIC", ItemType.ELEMENT, 0),
    ATYPICAL_DISRUPTED_WAKE_ECHOES("ATYPICAL DISRUPTED WAKE ECHOES", ItemType.DATA, 0),
    ATYPICAL_ENCRYPTION_ARCHIVES("ATYPICAL ENCRYPTION ARCHIVES", ItemType.DATA, 0),
    BASIC_CONDUCTORS("BASIC CONDUCTORS", ItemType.MATERIAL, 0),
    CADMIUM("CADMIUM", ItemType.ELEMENT, 0),
    CLASSIFIED_SCAN_FRAGMENT("CLASSIFIED SCAN FRAGMENT", ItemType.DATA, 0),
    CARBON("CARBON", ItemType.ELEMENT, 0),
    CHEMICAL_DISTILLERY("CHEMICAL DISTILLERY", ItemType.MATERIAL, 0),
    CHEMICAL_MANIPULATORS("CHEMICAL MANIPULATORS", ItemType.MATERIAL, 0),
    CHEMICAL_PROCESSORS("CHEMICAL PROCESSORS", ItemType.MATERIAL, 0),
    CHROMIUM("CHROMIUM", ItemType.ELEMENT, 0),
    CLASSIFIED_SCAN_DATABANKS("CLASSIFIED SCAN DATABANKS", ItemType.DATA, 0),
    COMPOUND_SHIELDING("COMPOUND SHIELDING", ItemType.MATERIAL, 0),
    CONDUCTIVE_CERAMICS("CONDUCTIVE CERAMICS", ItemType.MATERIAL, 0),
    CONDUCTIVE_COMPONENTS("CONDUCTIVE COMPONENTS", ItemType.MATERIAL, 0),
    CONDUCTIVE_POLYMERS("CONDUCTIVE POLYMERS", ItemType.MATERIAL, 0),
    CONFIGURABLE_COMPONENTS("CONFIGURABLE COMPONENTS", ItemType.MATERIAL, 0),
    CORE_DYNAMICS_COMPOSITES("CORE DYNAMICS COMPOSITES", ItemType.MATERIAL, 0),
    CRACKED_INDUSTRIAL_FIRMWARE("CRACKED INDUSTRIAL FIRMWARE", ItemType.DATA, 0),
    DATAMINED_WAKE_EXCEPTIONS("DATAMINED WAKE EXCEPTIONS", ItemType.DATA, 0),
    DISTORED_SHIELD_CYCLE_RECORDINGS("DISTORED SHIELD CYCLE RECORDINGS", ItemType.DATA, 0),
    DIVERGENT_SCAN_DATA("DIVERGENT SCAN DATA", ItemType.DATA, 0),
    ECCENTRIC_HYPERSPACE_TRAJECTORIES("ECCENTRIC HYPERSPACE TRAJECTORIES", ItemType.DATA, 0),
    ELECTROCHEMICAL_ARRAYS("ELECTROCHEMICAL ARRAYS", ItemType.MATERIAL, 0),
    EXCEPTIONAL_SCRAMBLED_EMISSION_DATA("EXCEPTIONAL SCRAMBLED EMISSION DATA", ItemType.DATA, 0),
    EXQUISITE_FOCUS_CRYSTALS("EXQUISITE FOCUS CRYSTALS", ItemType.MATERIAL, 0),
    FLAWED_FOCUS_CRYSTALS("FLAWED FOCUS CRYSTALS", ItemType.MATERIAL, 0),
    FOCUS_CRYSTALS("FOCUS CRYSTALS", ItemType.MATERIAL, 0),
    GALVANISING_ALLOYS("GALVANISING ALLOYS", ItemType.MATERIAL, 0),
    GERMANIUM("GERMANIUM", ItemType.ELEMENT, 0),
    GRID_RESISTORS("GRID RESISTORS", ItemType.MATERIAL, 0),
    HEAT_DISPERSION_PLATE("HEAT DISPERSION PLATE", ItemType.MATERIAL, 0),
    HEAT_EXCHANGERS("HEAT EXCHANGERS", ItemType.MATERIAL, 0),
    HEAT_VANES("HEAT VANES", ItemType.MATERIAL, 0),
    HIGH_DENSITY_COMPOSITES("HIGH DENSITY COMPOSITES", ItemType.MATERIAL, 0),
    HYBRID_CAPACITORS("HYBRID CAPACITORS", ItemType.MATERIAL, 0),
    IMPERIAL_SHIELDING("IMPERIAL SHIELDING", ItemType.MATERIAL, 0),
    INCONSISTENT_SHIELD_SOAK_ANALYSIS("INCONSISTENT SHIELD SOAK ANALYSIS", ItemType.DATA, 0),
    IRON("IRON", ItemType.ELEMENT, 0),
    IRREGULAR_EMISSION_DATA("IRREGULAR EMISSION DATA", ItemType.DATA, 0),
    MANGANESE("MANGANESE", ItemType.ELEMENT, 0),
    MECHANICAL_COMPONENTS("MECHANICAL COMPONENTS", ItemType.MATERIAL, 0),
    MECHANICAL_EQUIPMENT("MECHANICAL EQUIPMENT", ItemType.MATERIAL, 0),
    MECHANICAL_SCRAP("MECHANICAL SCRAP", ItemType.MATERIAL, 0),
    MERCURY("MERCURY", ItemType.ELEMENT, 0),
    MILITARY_GRADE_ALLOYS("MILITARY GRADE ALLOYS", ItemType.MATERIAL, 0),
    MODIFIED_CONSUMER_FIRMWARE("MODIFIED CONSUMER FIRMWARE", ItemType.DATA, 0),
    MODIFIED_EMBEDDED_FIRMWARE("MODIFIED EMBEDDED FIRMWARE", ItemType.DATA, 0),
    MOLYBDENUM("MOLYBDENUM", ItemType.ELEMENT, 0),
    NICKEL("NICKEL", ItemType.ELEMENT, 0),
    NIOBIUM("NIOBIUM", ItemType.ELEMENT, 0),
    OPEN_SYMMETRIC_KEYS("OPEN SYMMETRIC KEYS", ItemType.DATA, 0),
    PECULIAR_SHIELD_FREQUENCY_DATA("PECULIAR SHIELD FREQUENCY DATA", ItemType.DATA, 0),
    PHARMACEUTICAL_ISOLATORS("PHARMACEUTICAL ISOLATORS", ItemType.MATERIAL, 0),
    PHASE_ALLOYS("PHASE ALLOYS", ItemType.MATERIAL, 0),
    PHOSPHORUS("PHOSPHORUS", ItemType.ELEMENT, 0),
    POLONIUM("POLONIUM", ItemType.ELEMENT, 0),
    POLYMER_CAPACITORS("POLYMER CAPACITORS", ItemType.MATERIAL, 0),
    PRECIPITATED_ALLOYS("PRECIPITATED ALLOYS", ItemType.MATERIAL, 0),
    PROPRIETARY_COMPOSITES("PROPRIETARY COMPOSITES", ItemType.MATERIAL, 0),
    PROTO_HEAT_RADIATORS("PROTO HEAT RADIATORS", ItemType.MATERIAL, 0),
    PROTO_LIGHT_ALLOYS("PROTO LIGHT ALLOYS", ItemType.MATERIAL, 0),
    PROTO_RADIOLIC_ALLOYS("PROTO RADIOLIC ALLOYS", ItemType.MATERIAL, 0),
    REFINED_FOCUS_CRYSTALS("REFINED FOCUS CRYSTALS", ItemType.MATERIAL, 0),
    RUTHENIUM("RUTHENIUM", ItemType.ELEMENT, 0),
    SALVAGED_ALLOYS("SALVAGED ALLOYS", ItemType.MATERIAL, 0),
    SECURITY_FIRMWARE_PATCH("SECURITY FIRMWARE PATCH", ItemType.DATA, 0),
    SELENIUM("SELENIUM", ItemType.ELEMENT, 0),
    SHIELD_EMITTERS("SHIELD EMITTERS", ItemType.MATERIAL, 0),
    SHIELDING_SENSORS("SHIELDING SENSORS", ItemType.MATERIAL, 0),
    SPECIALISED_LEGACY_FIRMWARE("SPECIALISED LEGACY FIRMWARE", ItemType.DATA, 0),
    STRANGE_WAKE_SOLUTIONS("STRANGE WAKE SOLUTIONS", ItemType.DATA, 0),
    SULPHUR("SULPHUR", ItemType.ELEMENT, 0),
    TAGGED_ENCRYPTION_CODES("TAGGED ENCRYPTION CODES", ItemType.DATA, 0),
    TECHNETIUM("TECHNETIUM", ItemType.ELEMENT, 0),
    TELLURIUM("TELLURIUM", ItemType.ELEMENT, 0),
    THERMIC_ALLOYS("THERMIC ALLOYS", ItemType.MATERIAL, 0),
    TIN("TIN", ItemType.ELEMENT, 0),
    TUNGSTEN("TUNGSTEN", ItemType.ELEMENT, 0),
    UNEXPECTED_EMISSION_DATA("UNEXPECTED EMISSION DATA", ItemType.DATA, 0),
    UNIDENTIFIED_SCAN_ARCHIVES("UNIDENTIFIED SCAN ARCHIVES", ItemType.DATA, 0),
    UNTYPICAL_SHIELD_SCANS("UNTYPICAL SHIELD SCANS", ItemType.DATA, 0),
    UNUSUAL_ENCRYPTED_FILES("UNUSUAL ENCRYPTED FILES", ItemType.DATA, 0),
    VANADIUM("VANADIUM", ItemType.ELEMENT, 0),
    WORN_SHIELD_EMITTERS("WORN SHIELD EMITTERS", ItemType.MATERIAL, 0),
    YTTRIUM("YTTRIUM", ItemType.ELEMENT, 0),
    ZINC("ZINC", ItemType.ELEMENT, 0),
    ZIRCONIUM("ZIRCONIUM", ItemType.ELEMENT, 0);
    //@formatter:on

    private final String name;
    private final ItemType type;
    private final int grade;

    private Item(String name, ItemType type, int grade) {
        this.name = name;
        this.type = type;
        this.grade = grade;
    }

    public static List<Item> byType(ItemType type) {
        List<Item> result = new ArrayList<>();
        for (Item item : Item.values()) {
            if (item.getType() == type) {
                result.add(item);
            }
        }
        Collections.sort(result, new Comparator<Item>() {
            @Override
            public int compare(Item i1, Item i2) {
                return -1 * new Integer(i1.getName().length()).compareTo(new Integer(i2.getName().length()));
            }
        });
        return result;
    }

    public static Item findBestMatching(String name, ItemType type) {
        Item bestItem = null;
        float bestItemError = Float.MAX_VALUE;
        for (Item item : Item.values()) {
            if (type == null || item.getType() == type) {
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

    public String getName() {
        return this.name;
    }

    public ItemType getType() {
        return this.type;
    }

    public int getGrade() {
        return this.grade;
    }

    public static enum ItemType {
        COMMODITY, ELEMENT, MATERIAL, DATA;
    }

}
