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
    ADAPTIVE_ENCRYPTIONS_CAPTURE("ADAPTIVE ENCRYPTIONS CAPTURE", ItemType.DATA, 0),
    ANOMALOUS_BULK_SCAN_DATA("ANOMALOUS BULK SCAN DATA", ItemType.DATA, 0),
    ANOMALOUS_FSD_TELEMETRY("ANOMALOUS FSD TELEMETRY", ItemType.DATA, 0),
    ANTIMONY("ANTIMONY", ItemType.ELEMENT, 0),
    ARSENIC("ARSENIC", ItemType.ELEMENT, 0),
    ATYPICAL_DISRUPTED_WAKE_ECHOES("ATYPICAL DISRUPTED WAKE ECHOES", ItemType.DATA, 0),
    ATYPICAL_ENCRYPTION_ARCHIVES("ATYPICAL ENCRYPTION ARCHIVES", ItemType.DATA, 0),
    BASIC_CONDUCTORS("BASIC CONDUCTORS", ItemType.MANUFACTURED, 0),
    BIOTECH_CONDUCTORS("BIOTECH CONDUCTORS", ItemType.MANUFACTURED, 0),
    CADMIUM("CADMIUM", ItemType.ELEMENT, 0),
    CLASSIFIED_SCAN_FRAGMENT("CLASSIFIED SCAN FRAGMENT", ItemType.DATA, 0),
    CARBON("CARBON", ItemType.ELEMENT, 0),
    CHEMICAL_DISTILLERY("CHEMICAL DISTILLERY", ItemType.MANUFACTURED, 0),
    CHEMICAL_MANIPULATORS("CHEMICAL MANIPULATORS", ItemType.MANUFACTURED, 0),
    CHEMICAL_PROCESSORS("CHEMICAL PROCESSORS", ItemType.MANUFACTURED, 0),
    CHROMIUM("CHROMIUM", ItemType.ELEMENT, 0),
    CLASSIFIED_SCAN_DATABANKS("CLASSIFIED SCAN DATABANKS", ItemType.DATA, 0),
    COMPOUND_SHIELDING("COMPOUND SHIELDING", ItemType.MANUFACTURED, 0),
    CONDUCTIVE_CERAMICS("CONDUCTIVE CERAMICS", ItemType.MANUFACTURED, 0),
    CONDUCTIVE_COMPONENTS("CONDUCTIVE COMPONENTS", ItemType.MANUFACTURED, 0),
    CONDUCTIVE_POLYMERS("CONDUCTIVE POLYMERS", ItemType.MANUFACTURED, 0),
    CONFIGURABLE_COMPONENTS("CONFIGURABLE COMPONENTS", ItemType.MANUFACTURED, 0),
    CORE_DYNAMICS_COMPOSITES("CORE DYNAMICS COMPOSITES", ItemType.MANUFACTURED, 0),
    CRACKED_INDUSTRIAL_FIRMWARE("CRACKED INDUSTRIAL FIRMWARE", ItemType.DATA, 0),
    DATAMINED_WAKE_EXCEPTIONS("DATAMINED WAKE EXCEPTIONS", ItemType.DATA, 0),
    DECODED_EMISSION_DATA("DECODED EMISSION DATA", ItemType.DATA, 0),
    DISTORTED_SHIELD_CYCLE_RECORDINGS("DISTORTED SHIELD CYCLE RECORDINGS", ItemType.DATA, 0),
    DIVERGENT_SCAN_DATA("DIVERGENT SCAN DATA", ItemType.DATA, 0),
    ECCENTRIC_HYPERSPACE_TRAJECTORIES("ECCENTRIC HYPERSPACE TRAJECTORIES", ItemType.DATA, 0),
    ELECTROCHEMICAL_ARRAYS("ELECTROCHEMICAL ARRAYS", ItemType.MANUFACTURED, 0),
    EXCEPTIONAL_SCRAMBLED_EMISSION_DATA("EXCEPTIONAL SCRAMBLED EMISSION DATA", ItemType.DATA, 0),
    EXQUISITE_FOCUS_CRYSTALS("EXQUISITE FOCUS CRYSTALS", ItemType.MANUFACTURED, 0),
    FLAWED_FOCUS_CRYSTALS("FLAWED FOCUS CRYSTALS", ItemType.MANUFACTURED, 0),
    FOCUS_CRYSTALS("FOCUS CRYSTALS", ItemType.MANUFACTURED, 0),
    GALVANISING_ALLOYS("GALVANISING ALLOYS", ItemType.MANUFACTURED, 0),
    GERMANIUM("GERMANIUM", ItemType.ELEMENT, 0),
    GRID_RESISTORS("GRID RESISTORS", ItemType.MANUFACTURED, 0),
    HEAT_CONDUCTION_WIRING("HEAT CONDUCTION WIRING", ItemType.MANUFACTURED, 0),
    HEAT_DISPERSION_PLATE("HEAT DISPERSION PLATE", ItemType.MANUFACTURED, 0),
    HEAT_EXCHANGERS("HEAT EXCHANGERS", ItemType.MANUFACTURED, 0),
    HEAT_VANES("HEAT VANES", ItemType.MANUFACTURED, 0),
    HIGH_DENSITY_COMPOSITES("HIGH DENSITY COMPOSITES", ItemType.MANUFACTURED, 0),
    HYBRID_CAPACITORS("HYBRID CAPACITORS", ItemType.MANUFACTURED, 0),
    IMPERIAL_SHIELDING("IMPERIAL SHIELDING", ItemType.MANUFACTURED, 0),
    INCONSISTENT_SHIELD_SOAK_ANALYSIS("INCONSISTENT SHIELD SOAK ANALYSIS", ItemType.DATA, 0),
    IRON("IRON", ItemType.ELEMENT, 0),
    IRREGULAR_EMISSION_DATA("IRREGULAR EMISSION DATA", ItemType.DATA, 0),
    MANGANESE("MANGANESE", ItemType.ELEMENT, 0),
    MECHANICAL_COMPONENTS("MECHANICAL COMPONENTS", ItemType.MANUFACTURED, 0),
    MECHANICAL_EQUIPMENT("MECHANICAL EQUIPMENT", ItemType.MANUFACTURED, 0),
    MECHANICAL_SCRAP("MECHANICAL SCRAP", ItemType.MANUFACTURED, 0),
    MERCURY("MERCURY", ItemType.ELEMENT, 0),
    MILITARY_GRADE_ALLOYS("MILITARY GRADE ALLOYS", ItemType.MANUFACTURED, 0),
    MILITARY_SUPERCAPACITORS("MILITARY SUPERCAPACITORS", ItemType.MANUFACTURED, 0),
    MODIFIED_CONSUMER_FIRMWARE("MODIFIED CONSUMER FIRMWARE", ItemType.DATA, 0),
    MODIFIED_EMBEDDED_FIRMWARE("MODIFIED EMBEDDED FIRMWARE", ItemType.DATA, 0),
    MOLYBDENUM("MOLYBDENUM", ItemType.ELEMENT, 0),
    NICKEL("NICKEL", ItemType.ELEMENT, 0),
    NIOBIUM("NIOBIUM", ItemType.ELEMENT, 0),//pattern alpha obelisk data
    OPEN_SYMMETRIC_KEYS("OPEN SYMMETRIC KEYS", ItemType.DATA, 0),
    PATTERN_ALPHA_OBELISK_DATA("PATTERN ALPHA OBELISK DATA", ItemType.DATA, 0),
    PATTERN_BETA_OBELISK_DATA("PATTERN BETA OBELISK DATA", ItemType.DATA, 0),
    PATTERN_GAMMA_OBELISK_DATA("PATTERN GAMMA OBELISK DATA", ItemType.DATA, 0),
    PATTERN_DELTA_OBELISK_DATA("PATTERN DELTA OBELISK DATA", ItemType.DATA, 0),
    PATTERN_EPSILON_OBELISK_DATA("PATTERN EPSILON OBELISK DATA", ItemType.DATA, 0),
    PECULIAR_SHIELD_FREQUENCY_DATA("PECULIAR SHIELD FREQUENCY DATA", ItemType.DATA, 0),
    PHARMACEUTICAL_ISOLATORS("PHARMACEUTICAL ISOLATORS", ItemType.MANUFACTURED, 0),
    PHASE_ALLOYS("PHASE ALLOYS", ItemType.MANUFACTURED, 0),
    PHOSPHORUS("PHOSPHORUS", ItemType.ELEMENT, 0),
    POLONIUM("POLONIUM", ItemType.ELEMENT, 0),
    POLYMER_CAPACITORS("POLYMER CAPACITORS", ItemType.MANUFACTURED, 0),
    PRECIPITATED_ALLOYS("PRECIPITATED ALLOYS", ItemType.MANUFACTURED, 0),
    PROPRIETARY_COMPOSITES("PROPRIETARY COMPOSITES", ItemType.MANUFACTURED, 0),
    PROTO_HEAT_RADIATORS("PROTO HEAT RADIATORS", ItemType.MANUFACTURED, 0),
    PROTO_LIGHT_ALLOYS("PROTO LIGHT ALLOYS", ItemType.MANUFACTURED, 0),
    PROTO_RADIOLIC_ALLOYS("PROTO RADIOLIC ALLOYS", ItemType.MANUFACTURED, 0),
    REFINED_FOCUS_CRYSTALS("REFINED FOCUS CRYSTALS", ItemType.MANUFACTURED, 0),
    RUTHENIUM("RUTHENIUM", ItemType.ELEMENT, 0),
    SALVAGED_ALLOYS("SALVAGED ALLOYS", ItemType.MANUFACTURED, 0),
    SECURITY_FIRMWARE_PATCH("SECURITY FIRMWARE PATCH", ItemType.DATA, 0),
    SELENIUM("SELENIUM", ItemType.ELEMENT, 0),
    SHIELD_EMITTERS("SHIELD EMITTERS", ItemType.MANUFACTURED, 0),
    SHIELDING_SENSORS("SHIELDING SENSORS", ItemType.MANUFACTURED, 0),
    SPECIALISED_LEGACY_FIRMWARE("SPECIALISED LEGACY FIRMWARE", ItemType.DATA, 0),
    STRANGE_WAKE_SOLUTIONS("STRANGE WAKE SOLUTIONS", ItemType.DATA, 0),
    SULPHUR("SULPHUR", ItemType.ELEMENT, 0),
    TAGGED_ENCRYPTION_CODES("TAGGED ENCRYPTION CODES", ItemType.DATA, 0),
    TECHNETIUM("TECHNETIUM", ItemType.ELEMENT, 0),
    TELLURIUM("TELLURIUM", ItemType.ELEMENT, 0),
    THERMIC_ALLOYS("THERMIC ALLOYS", ItemType.MANUFACTURED, 0),
    TIN("TIN", ItemType.ELEMENT, 0),
    TUNGSTEN("TUNGSTEN", ItemType.ELEMENT, 0),
    UNEXPECTED_EMISSION_DATA("UNEXPECTED EMISSION DATA", ItemType.DATA, 0),
    UNIDENTIFIED_SCAN_ARCHIVES("UNIDENTIFIED SCAN ARCHIVES", ItemType.DATA, 0),
    UNKNOWN_FRAGMENT("UNKNOWN FRAGMENT", ItemType.MANUFACTURED, 0),
    UNTYPICAL_SHIELD_SCANS("UNTYPICAL SHIELD SCANS", ItemType.DATA, 0),
    UNUSUAL_ENCRYPTED_FILES("UNUSUAL ENCRYPTED FILES", ItemType.DATA, 0),
    VANADIUM("VANADIUM", ItemType.ELEMENT, 0),
    WORN_SHIELD_EMITTERS("WORN SHIELD EMITTERS", ItemType.MANUFACTURED, 0),
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

    public static Item byJournalName(String journalName) {
        if ("arsenic".equals(journalName)) {
            return Item.ARSENIC;
        } else if ("biotechconductors".equals(journalName)) {
            return Item.BIOTECH_CONDUCTORS;
        } else if ("cadmium".equals(journalName)) {
            return Item.CADMIUM;
        } else if ("carbon".equals(journalName)) {
            return Item.CARBON;
        } else if ("chemicaldistillery".equals(journalName)) {
            return Item.CHEMICAL_DISTILLERY;
        } else if ("chemicalmanipulators".equals(journalName)) {
            return Item.CHEMICAL_MANIPULATORS;
        } else if ("chemicalprocessors".equals(journalName)) {
            return Item.CHEMICAL_PROCESSORS;
        } else if ("chromium".equals(journalName)) {
            return Item.CHROMIUM;
        } else if ("compoundshielding".equals(journalName)) {
            return Item.COMPOUND_SHIELDING;
        } else if ("conductiveceramics".equals(journalName)) {
            return Item.CONDUCTIVE_CERAMICS;
        } else if ("conductivecomponents".equals(journalName)) {
            return Item.CONDUCTIVE_COMPONENTS;
        } else if ("conductivepolymers".equals(journalName)) {
            return Item.CONDUCTIVE_POLYMERS;
        } else if ("configurablecomponents".equals(journalName)) {
            return Item.CONFIGURABLE_COMPONENTS;
        } else if ("electrochemicalarrays".equals(journalName)) {
            return Item.ELECTROCHEMICAL_ARRAYS;
        } else if ("exquisitefocuscrystals".equals(journalName)) {
            return Item.EXQUISITE_FOCUS_CRYSTALS;
        } else if ("focuscrystals".equals(journalName)) {
            return Item.FOCUS_CRYSTALS;
        } else if ("galvanisingalloys".equals(journalName)) {
            return Item.GALVANISING_ALLOYS;
        } else if ("germanium".equals(journalName)) {
            return Item.GERMANIUM;
        } else if ("heatconductionwiring".equals(journalName)) {
            return Item.HEAT_CONDUCTION_WIRING;
        } else if ("heatdispersionplate".equals(journalName)) {
            return Item.HEAT_DISPERSION_PLATE;
        } else if ("heatvanes".equals(journalName)) {
            return Item.HEAT_VANES;
        } else if ("imperialshielding".equals(journalName)) {
            return Item.IMPERIAL_SHIELDING;
        } else if ("iron".equals(journalName)) {
            return Item.IRON;
        } else if ("manganese".equals(journalName)) {
            return Item.MANGANESE;
        } else if ("mechanicalcomponents".equals(journalName)) {
            return Item.MECHANICAL_COMPONENTS;
        } else if ("mechanicalequipment".equals(journalName)) {
            return Item.MECHANICAL_EQUIPMENT;
        } else if ("mechanicalscrap".equals(journalName)) {
            return Item.MECHANICAL_SCRAP;
        } else if ("molybdenum".equals(journalName)) {
            return Item.MOLYBDENUM;
        } else if ("nickel".equals(journalName)) {
            return Item.NICKEL;
        } else if ("niobium".equals(journalName)) {
            return Item.NIOBIUM;
        } else if ("pharmaceuticalisolators".equals(journalName)) {
            return Item.PHARMACEUTICAL_ISOLATORS;
        } else if ("phasealloys".equals(journalName)) {
            return Item.PHASE_ALLOYS;
        } else if ("phosphorus".equals(journalName)) {
            return Item.PHOSPHORUS;
        } else if ("protolightalloys".equals(journalName)) {
            return Item.PROTO_LIGHT_ALLOYS;
        } else if ("refinedfocuscrystals".equals(journalName)) {
            return Item.REFINED_FOCUS_CRYSTALS;
        } else if ("salvagedalloys".equals(journalName)) {
            return Item.SALVAGED_ALLOYS;
        } else if ("selenium".equals(journalName)) {
            return Item.SELENIUM;
        } else if ("shieldemitters".equals(journalName)) {
            return Item.SHIELD_EMITTERS;
        } else if ("sulphur".equals(journalName)) {
            return Item.SULPHUR;
        } else if ("thermicalloys".equals(journalName)) {
            return Item.THERMIC_ALLOYS;
        } else if ("vanadium".equals(journalName)) {
            return Item.VANADIUM;
        } else if ("wornshieldemitters".equals(journalName)) {
            return Item.WORN_SHIELD_EMITTERS;
        } else if ("zinc".equals(journalName)) {
            return Item.ZINC;
        } else if ("zirconium".equals(journalName)) {
            return Item.ZIRCONIUM;
        }

        if ("bulkscandata".equals(journalName)) {
            return Item.ANOMALOUS_BULK_SCAN_DATA;
        } else if ("consumerfirmware".equals(journalName)) {
            return Item.MODIFIED_CONSUMER_FIRMWARE;
        } else if ("dataminedwake".equals(journalName)) {
            return Item.DATAMINED_WAKE_EXCEPTIONS;
        } else if ("decodedemissiondata".equals(journalName)) {
            return Item.DECODED_EMISSION_DATA;
        } else if ("emissiondata".equals(journalName)) {
            return null; // TODO
        } else if ("encodedscandata".equals(journalName)) {
            return null; // TODO
        } else if ("fsdtelemetry".equals(journalName)) {
            return Item.ANOMALOUS_FSD_TELEMETRY;
        } else if ("industrialfirmware".equals(journalName)) {
            return Item.CRACKED_INDUSTRIAL_FIRMWARE;
        } else if ("legacyfirmware".equals(journalName)) {
            return Item.SPECIALISED_LEGACY_FIRMWARE;
        } else if ("scanarchives".equals(journalName)) {
            return Item.UNIDENTIFIED_SCAN_ARCHIVES;
        } else if ("scandatabanks".equals(journalName)) {
            return Item.CLASSIFIED_SCAN_DATABANKS;
        } else if ("shieldcyclerecordings".equals(journalName)) {
            return Item.DISTORTED_SHIELD_CYCLE_RECORDINGS;
        } else if ("shielddensityreports".equals(journalName)) {
            return null; // TODO
        } else if ("shieldpatternanalysis".equals(journalName)) {
            return Item.ABBERANT_SHIELD_PATTERN_ANALYSIS;
        } else if ("shieldsoakanalysis".equals(journalName)) {
            return Item.INCONSISTENT_SHIELD_SOAK_ANALYSIS;
        }

        return null;
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
        ELEMENT, MANUFACTURED, DATA, COMMODITY;
    }

}
