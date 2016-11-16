package borg.edtrading.data;

import org.apache.commons.lang3.StringUtils;
import org.jcodec.common.logging.Logger;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Item
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public enum Item {

    //@formatter:off
    ANTIMONY("ANTIMONY", "antimony", ItemType.ELEMENT, 0),
    ARSENIC("ARSENIC", "arsenic", ItemType.ELEMENT, 0),
    CADMIUM("CADMIUM", "cadmium", ItemType.ELEMENT, 0),
    CARBON("CARBON", "carbon", ItemType.ELEMENT, 0),
    CHROMIUM("CHROMIUM", "chromium", ItemType.ELEMENT, 0),
    GERMANIUM("GERMANIUM", "germanium", ItemType.ELEMENT, 0),
    IRON("IRON", "iron", ItemType.ELEMENT, 0),
    MANGANESE("MANGANESE", "manganese", ItemType.ELEMENT, 0),
    MERCURY("MERCURY", "mercury", ItemType.ELEMENT, 0),
    MOLYBDENUM("MOLYBDENUM", "molybdenum", ItemType.ELEMENT, 0),
    NICKEL("NICKEL", "nickel", ItemType.ELEMENT, 0),
    NIOBIUM("NIOBIUM", "niobium", ItemType.ELEMENT, 0),
    PHOSPHORUS("PHOSPHORUS", "phosphorus", ItemType.ELEMENT, 0),
    POLONIUM("POLONIUM", "polonium", ItemType.ELEMENT, 0),
    RUTHENIUM("RUTHENIUM", "ruthenium", ItemType.ELEMENT, 0),
    SELENIUM("SELENIUM", "selenium", ItemType.ELEMENT, 0),
    SULPHUR("SULPHUR", "sulphur", ItemType.ELEMENT, 0),
    TECHNETIUM("TECHNETIUM", "technetium", ItemType.ELEMENT, 0),
    TELLURIUM("TELLURIUM", "tellurium", ItemType.ELEMENT, 0),
    TIN("TIN", "tin", ItemType.ELEMENT, 0),
    TUNGSTEN("TUNGSTEN", "tungsten", ItemType.ELEMENT, 0),
    VANADIUM("VANADIUM", "vanadium", ItemType.ELEMENT, 0),
    YTTRIUM("YTTRIUM", "yttrium", ItemType.ELEMENT, 0),
    ZINC("ZINC", "zinc", ItemType.ELEMENT, 0),
    ZIRCONIUM("ZIRCONIUM", "zirconium", ItemType.ELEMENT, 0),

    BASIC_CONDUCTORS("BASIC CONDUCTORS",null, ItemType.MANUFACTURED, 0),
    BIOTECH_CONDUCTORS("BIOTECH CONDUCTORS", "biotechconductors", ItemType.MANUFACTURED, 0),
    CHEMICAL_DISTILLERY("CHEMICAL DISTILLERY", "chemicaldistillery", ItemType.MANUFACTURED, 0),
    CHEMICAL_MANIPULATORS("CHEMICAL MANIPULATORS", "chemicalmanipulators", ItemType.MANUFACTURED, 0),
    CHEMICAL_PROCESSORS("CHEMICAL PROCESSORS", "chemicalprocessors", ItemType.MANUFACTURED, 0),
    COMPOUND_SHIELDING("COMPOUND SHIELDING", "compoundshielding", ItemType.MANUFACTURED, 0),
    CONDUCTIVE_CERAMICS("CONDUCTIVE CERAMICS", "conductiveceramics", ItemType.MANUFACTURED, 0),
    CONDUCTIVE_COMPONENTS("CONDUCTIVE COMPONENTS", "conductivecomponents", ItemType.MANUFACTURED, 0),
    CONDUCTIVE_POLYMERS("CONDUCTIVE POLYMERS", "conductivepolymers", ItemType.MANUFACTURED, 0),
    CONFIGURABLE_COMPONENTS("CONFIGURABLE COMPONENTS", "configurablecomponents", ItemType.MANUFACTURED, 0),
    CORE_DYNAMICS_COMPOSITES("CORE DYNAMICS COMPOSITES",null, ItemType.MANUFACTURED, 0),
    ELECTROCHEMICAL_ARRAYS("ELECTROCHEMICAL ARRAYS", "electrochemicalarrays", ItemType.MANUFACTURED, 0),
    EXQUISITE_FOCUS_CRYSTALS("EXQUISITE FOCUS CRYSTALS", "exquisitefocuscrystals", ItemType.MANUFACTURED, 0),
    FLAWED_FOCUS_CRYSTALS("FLAWED FOCUS CRYSTALS",null, ItemType.MANUFACTURED, 0),
    FOCUS_CRYSTALS("FOCUS CRYSTALS", "focuscrystals", ItemType.MANUFACTURED, 0),
    GALVANISING_ALLOYS("GALVANISING ALLOYS", "galvanisingalloys", ItemType.MANUFACTURED, 0),
    GRID_RESISTORS("GRID RESISTORS",null, ItemType.MANUFACTURED, 0),
    HEAT_CONDUCTION_WIRING("HEAT CONDUCTION WIRING", "heatconductionwiring", ItemType.MANUFACTURED, 0),
    HEAT_DISPERSION_PLATE("HEAT DISPERSION PLATE", "heatdispersionplate", ItemType.MANUFACTURED, 0),
    HEAT_EXCHANGERS("HEAT EXCHANGERS",null, ItemType.MANUFACTURED, 0),
    HEAT_VANES("HEAT VANES", "heatvanes", ItemType.MANUFACTURED, 0),
    HIGH_DENSITY_COMPOSITES("HIGH DENSITY COMPOSITES",null, ItemType.MANUFACTURED, 0),
    HYBRID_CAPACITORS("HYBRID CAPACITORS",null, ItemType.MANUFACTURED, 0),
    IMPERIAL_SHIELDING("IMPERIAL SHIELDING", "imperialshielding", ItemType.MANUFACTURED, 0),
    MECHANICAL_COMPONENTS("MECHANICAL COMPONENTS", "mechanicalcomponents", ItemType.MANUFACTURED, 0),
    MECHANICAL_EQUIPMENT("MECHANICAL EQUIPMENT", "mechanicalequipment", ItemType.MANUFACTURED, 0),
    MECHANICAL_SCRAP("MECHANICAL SCRAP", "mechanicalscrap", ItemType.MANUFACTURED, 0),
    MILITARY_GRADE_ALLOYS("MILITARY GRADE ALLOYS",null, ItemType.MANUFACTURED, 0),
    MILITARY_SUPERCAPACITORS("MILITARY SUPERCAPACITORS",null, ItemType.MANUFACTURED, 0),
    PHARMACEUTICAL_ISOLATORS("PHARMACEUTICAL ISOLATORS", "pharmaceuticalisolators", ItemType.MANUFACTURED, 0),
    PHASE_ALLOYS("PHASE ALLOYS", "phasealloys", ItemType.MANUFACTURED, 0),
    POLYMER_CAPACITORS("POLYMER CAPACITORS",null, ItemType.MANUFACTURED, 0),
    PRECIPITATED_ALLOYS("PRECIPITATED ALLOYS",null, ItemType.MANUFACTURED, 0),
    PROPRIETARY_COMPOSITES("PROPRIETARY COMPOSITES",null, ItemType.MANUFACTURED, 0),
    PROTO_HEAT_RADIATORS("PROTO HEAT RADIATORS",null, ItemType.MANUFACTURED, 0),
    PROTO_LIGHT_ALLOYS("PROTO LIGHT ALLOYS", "protolightalloys", ItemType.MANUFACTURED, 0),
    PROTO_RADIOLIC_ALLOYS("PROTO RADIOLIC ALLOYS",null, ItemType.MANUFACTURED, 0),
    REFINED_FOCUS_CRYSTALS("REFINED FOCUS CRYSTALS", "refinedfocuscrystals", ItemType.MANUFACTURED, 0),
    SALVAGED_ALLOYS("SALVAGED ALLOYS", "salvagedalloys", ItemType.MANUFACTURED, 0),
    SHIELD_EMITTERS("SHIELD EMITTERS", "shieldemitters", ItemType.MANUFACTURED, 0),
    SHIELDING_SENSORS("SHIELDING SENSORS",null, ItemType.MANUFACTURED, 0),
    THERMIC_ALLOYS("THERMIC ALLOYS", "thermicalloys", ItemType.MANUFACTURED, 0),
    UNKNOWN_FRAGMENT("UNKNOWN FRAGMENT",null, ItemType.MANUFACTURED, 0),
    WORN_SHIELD_EMITTERS("WORN SHIELD EMITTERS", "wornshieldemitters", ItemType.MANUFACTURED, 0),

    ABBERANT_SHIELD_PATTERN_ANALYSIS("ABBERANT SHIELD PATTERN ANALYSIS","shieldpatternanalysis", ItemType.DATA, 0),
    ABNORMAL_COMPACT_EMISSIONS_DATA("ABNORMAL COMPACT EMISSIONS DATA",null, ItemType.DATA, 0),
    ADAPTIVE_ENCRYPTIONS_CAPTURE("ADAPTIVE ENCRYPTIONS CAPTURE",null, ItemType.DATA, 0),
    ANOMALOUS_BULK_SCAN_DATA("ANOMALOUS BULK SCAN DATA", "bulkscandata", ItemType.DATA, 0),
    ANOMALOUS_FSD_TELEMETRY("ANOMALOUS FSD TELEMETRY","fsdtelemetry", ItemType.DATA, 0),
    ATYPICAL_DISRUPTED_WAKE_ECHOES("ATYPICAL DISRUPTED WAKE ECHOES",null, ItemType.DATA, 0),
    ATYPICAL_ENCRYPTION_ARCHIVES("ATYPICAL ENCRYPTION ARCHIVES",null, ItemType.DATA, 0),
    CLASSIFIED_SCAN_FRAGMENT("CLASSIFIED SCAN FRAGMENT",null, ItemType.DATA, 0),
    CLASSIFIED_SCAN_DATABANKS("CLASSIFIED SCAN DATABANKS","scandatabanks", ItemType.DATA, 0),
    CRACKED_INDUSTRIAL_FIRMWARE("CRACKED INDUSTRIAL FIRMWARE","industrialfirmware", ItemType.DATA, 0),
    DATAMINED_WAKE_EXCEPTIONS("DATAMINED WAKE EXCEPTIONS","dataminedwake", ItemType.DATA, 0),
    DECODED_EMISSION_DATA("DECODED EMISSION DATA","decodedemissiondata", ItemType.DATA, 0),
    DISTORTED_SHIELD_CYCLE_RECORDINGS("DISTORTED SHIELD CYCLE RECORDINGS","shieldcyclerecordings", ItemType.DATA, 0),
    DIVERGENT_SCAN_DATA("DIVERGENT SCAN DATA",null, ItemType.DATA, 0),
    ECCENTRIC_HYPERSPACE_TRAJECTORIES("ECCENTRIC HYPERSPACE TRAJECTORIES",null, ItemType.DATA, 0),
    EXCEPTIONAL_SCRAMBLED_EMISSION_DATA("EXCEPTIONAL SCRAMBLED EMISSION DATA",null, ItemType.DATA, 0),
    INCONSISTENT_SHIELD_SOAK_ANALYSIS("INCONSISTENT SHIELD SOAK ANALYSIS","shieldsoakanalysis", ItemType.DATA, 0),
    IRREGULAR_EMISSION_DATA("IRREGULAR EMISSION DATA","emissiondata", ItemType.DATA, 0),
    MODIFIED_CONSUMER_FIRMWARE("MODIFIED CONSUMER FIRMWARE", "consumerfirmware", ItemType.DATA, 0),
    MODIFIED_EMBEDDED_FIRMWARE("MODIFIED EMBEDDED FIRMWARE",null, ItemType.DATA, 0),
    OPEN_SYMMETRIC_KEYS("OPEN SYMMETRIC KEYS",null, ItemType.DATA, 0),
    PATTERN_ALPHA_OBELISK_DATA("PATTERN ALPHA OBELISK DATA",null, ItemType.DATA, 0),
    PATTERN_BETA_OBELISK_DATA("PATTERN BETA OBELISK DATA",null, ItemType.DATA, 0),
    PATTERN_GAMMA_OBELISK_DATA("PATTERN GAMMA OBELISK DATA",null, ItemType.DATA, 0),
    PATTERN_DELTA_OBELISK_DATA("PATTERN DELTA OBELISK DATA",null, ItemType.DATA, 0),
    PATTERN_EPSILON_OBELISK_DATA("PATTERN EPSILON OBELISK DATA",null, ItemType.DATA, 0),
    PECULIAR_SHIELD_FREQUENCY_DATA("PECULIAR SHIELD FREQUENCY DATA",null, ItemType.DATA, 0),
    SECURITY_FIRMWARE_PATCH("SECURITY FIRMWARE PATCH",null, ItemType.DATA, 0),
    SPECIALISED_LEGACY_FIRMWARE("SPECIALISED LEGACY FIRMWARE","legacyfirmware", ItemType.DATA, 0),
    STRANGE_WAKE_SOLUTIONS("STRANGE WAKE SOLUTIONS",null, ItemType.DATA, 0),
    TAGGED_ENCRYPTION_CODES("TAGGED ENCRYPTION CODES",null, ItemType.DATA, 0),
    UNEXPECTED_EMISSION_DATA("UNEXPECTED EMISSION DATA",null, ItemType.DATA, 0),
    UNIDENTIFIED_SCAN_ARCHIVES("UNIDENTIFIED SCAN ARCHIVES","scanarchives", ItemType.DATA, 0),
    UNTYPICAL_SHIELD_SCANS("UNTYPICAL SHIELD SCANS",null, ItemType.DATA, 0),
    UNUSUAL_ENCRYPTED_FILES("UNUSUAL ENCRYPTED FILES",null, ItemType.DATA, 0),

    // Engineer commodities
    ARTICULATION_MOTORS("ARTICULATION MOTORS","articulationmotors",ItemType.COMMODITY,0),
    BROMELLITE("BROMELLITE","bromellite",ItemType.COMMODITY,0),
    CMM_COMPOSITE("CMM COMPOSITE","cmmcomposite",ItemType.COMMODITY,0),
    EMERGENCY_POWER_CELLS("EMERGENCY POWER CELLS",null,ItemType.COMMODITY,0),
    ENERGY_GRID_ASSEMBLY("ENERGY GRID ASSEMBLY",null,ItemType.COMMODITY,0),
    EXHAUST_MANIFOLD("EXHAUST MANIFOLD","exhaustmanifold",ItemType.COMMODITY,0),
    HARDWARE_DIAGNOSTIC_SENSOR("HARDWARE DIAGNOSTIC SENSOR",null,ItemType.COMMODITY,0),
    HEATSINK_INTERLINK("HEATSINK INTERLINK",null,ItemType.COMMODITY,0),
    HN_SHOCK_MOUNT("HN SHOCK MOUNT",null,ItemType.COMMODITY,0),
    INSULATING_MEMBRANE("INSULATING MEMBRANE",null,ItemType.COMMODITY,0),
    ION_DISTRIBUTOR("ION DISTRIBUTOR","iondistributor",ItemType.COMMODITY,0),
    MAGNETIC_EMITTER_COIL("MAGNETIC EMITTER COIL","magneticemittercoil",ItemType.COMMODITY,0),
    MICRO_CONTROLLERS("MICRO CONTROLLERS",null,ItemType.COMMODITY,0),
    MICRO_WEAVE_COOLING_HOSES("MICRO-WEAVE COOLING HOSES","coolinghoses",ItemType.COMMODITY,0),
    MODULAR_TERMINALS("MODULAR TERMINALS",null,ItemType.COMMODITY,0),
    NANOBREAKERS("NANOBREAKERS",null,ItemType.COMMODITY,0),
    NEOFABRIC_INSULATION("NEOFABRIC INSULATION","neofabricinsulation",ItemType.COMMODITY,0),
    OSMIUM("OSMIUM","osmium",ItemType.COMMODITY,0),
    PLATINUM("PLATINUM","platinum",ItemType.COMMODITY,0),
    POWER_CONVERTER("POWER CONVERTER",null,ItemType.COMMODITY,0),
    POWER_TRANSFER_BUS("POWER TRANSFER BUS",null,ItemType.COMMODITY,0),
    PRASEODYMIUM("PRASEODYMIUM","praseodymium",ItemType.COMMODITY,0),
    RADIATION_BAFFLE("RADIATION BAFFLE","radiationbaffle",ItemType.COMMODITY,0),
    REINFORCED_MOUNTING_PLATE("REINFORCED MOUNTING PLATE",null,ItemType.COMMODITY,0),
    SAMARIUM("SAMARIUM","samarium",ItemType.COMMODITY,0),
    TELEMETRY_SUITE("TELEMETRY SUITE","telemetrysuite",ItemType.COMMODITY,0),

    // Rare commodities
    ALTAIRIAN_SKIN("ALTAIRIAN SKIN", "altairianskin", ItemType.COMMODITY, 0),
    FUJIN_TEA("FUJIN TEA", "fujintea", ItemType.COMMODITY, 0),
    KONGGA_ALE("KONGGA ALE", "konggaale", ItemType.COMMODITY, 0),
    META_ALLOYS("META ALLOYS", "metaalloys", ItemType.COMMODITY, 0),
    TOXANDJI_VIROCIDE("TOXANDJI VIROCIDE", "toxandjivirocide", ItemType.COMMODITY, 0),

    // Other commodities
    BIOWASTE("BIOWASTE", "biowaste", ItemType.COMMODITY, 0),
    CLOTHING("CLOTHING", "clothing", ItemType.COMMODITY, 0),
    EXPLOSIVES("EXPLOSIVES", "explosives", ItemType.COMMODITY, 0),
    FISH("FISH", "fish", ItemType.COMMODITY, 0),
    HYDROGEN_FUEL("HYDROGEN FUEL", "hydrogenfuel", ItemType.COMMODITY, 0),
    HYDROGEN_PEROXIDE("HYDROGEN PEROXIDE", "hydrogenperoxide", ItemType.COMMODITY, 0),
    LIQUID_OXYGEN("LIQUID OXYGEN", "liquidoxygen", ItemType.COMMODITY, 0),
    LITHIUM_HYDROXIDE("LITHIUM HYDROXIDE", "lithiumhydroxide", ItemType.COMMODITY, 0),
    METHANE_CLATHRATE("METHANE CLATHRATE", "methaneclathrate", ItemType.COMMODITY, 0),
    METHANOL_MONOHYDRATE_CRYSTALS("METHANOL MONOHYDRATE CRYSTALS", "methanolmonohydratecrystals", ItemType.COMMODITY, 0),
    PESTICIDES("PESTICIDES", "pesticides", ItemType.COMMODITY, 0),
    SYNTHETIC_MEAT("SYNTHETIC MEAT", "syntheticmeat", ItemType.COMMODITY, 0),
    TANTALUM("TANTALUM", "tantalum", ItemType.COMMODITY, 0),
    TITANIUM("TITANIUM", "titanium", ItemType.COMMODITY, 0),
    WATER("WATER", "water", ItemType.COMMODITY, 0),

    DRONES("DRONES", "drones", ItemType.COMMODITY, 0);
    //@formatter:on

    private static final SortedMap<ItemType, List<Item>> ITEMS_BY_TYPE = new TreeMap<>();
    private static final SortedMap<String, Item> ITEM_BY_NAME = new TreeMap<>();
    private static final SortedMap<String, Item> ITEM_BY_JOURNALNAME = new TreeMap<>();

    static {
        for (Item item : Item.values()) {
            List<Item> typeList = ITEMS_BY_TYPE.getOrDefault(item.getType(), new ArrayList<>());
            typeList.add(item);
            ITEMS_BY_TYPE.put(item.getType(), typeList);

            ITEM_BY_NAME.put(item.getName(), item);

            if (item.getJournalName() != null) {
                ITEM_BY_JOURNALNAME.put(item.getJournalName(), item);
            } else {
                Logger.warn("Unknown journal name: " + item);
            }
        }
    }

    private final String name;
    private final String journalName;
    private final ItemType type;
    private final int grade;

    private Item(String name, String journalName, ItemType type, int grade) {
        this.name = name;
        this.journalName = journalName;
        this.type = type;
        this.grade = grade;
    }

    public static List<Item> byType(ItemType type) {
        List<Item> result = new ArrayList<>(ITEMS_BY_TYPE.get(type));
        Collections.sort(result, new Comparator<Item>() {
            @Override
            public int compare(Item i1, Item i2) {
                return -1 * new Integer(i1.getName().length()).compareTo(new Integer(i2.getName().length()));
            }
        });
        return result;
    }

    public static Item byName(String name) {
        return ITEM_BY_NAME.get(name);
    }

    public static Item byJournalName(String journalName) {
        return ITEM_BY_JOURNALNAME.get(journalName);
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

    public String getJournalName() {
        return this.journalName;
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
