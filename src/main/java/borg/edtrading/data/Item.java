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
    // ELEMENT
    ANTIMONY                                          ("Antimony"                                        , "antimony"                                        , ItemType.ELEMENT         , 2),
    ARSENIC                                           ("Arsenic"                                         , "arsenic"                                         , ItemType.ELEMENT         , 0),
    CADMIUM                                           ("Cadmium"                                         , "cadmium"                                         , ItemType.ELEMENT         , 1),
    CARBON                                            ("Carbon"                                          , "carbon"                                          , ItemType.ELEMENT         , -2),
    CHROMIUM                                          ("Chromium"                                        , "chromium"                                        , ItemType.ELEMENT         , -1),
    GERMANIUM                                         ("Germanium"                                       , "germanium"                                       , ItemType.ELEMENT         , -1),
    IRON                                              ("Iron"                                            , "iron"                                            , ItemType.ELEMENT         , -2),
    MANGANESE                                         ("Manganese"                                       , "manganese"                                       , ItemType.ELEMENT         , -1),
    MERCURY                                           ("Mercury"                                         , "mercury"                                         , ItemType.ELEMENT         , 1),
    MOLYBDENUM                                        ("Molybdenum"                                      , "molybdenum"                                      , ItemType.ELEMENT         , 1),
    NICKEL                                            ("Nickel"                                          , "nickel"                                          , ItemType.ELEMENT         , -2),
    NIOBIUM                                           ("Niobium"                                         , "niobium"                                         , ItemType.ELEMENT         , 0),
    PHOSPHORUS                                        ("Phosphorus"                                      , "phosphorus"                                      , ItemType.ELEMENT         , -2),
    POLONIUM                                          ("Polonium"                                        , "polonium"                                        , ItemType.ELEMENT         , 2),
    RUTHENIUM                                         ("Ruthenium"                                       , "ruthenium"                                       , ItemType.ELEMENT         , 2),
    SELENIUM                                          ("Selenium"                                        , "selenium"                                        , ItemType.ELEMENT         , 0),
    SULPHUR                                           ("Sulphur"                                         , "sulphur"                                         , ItemType.ELEMENT         , -2),
    TECHNETIUM                                        ("Technetium"                                      , "technetium"                                      , ItemType.ELEMENT         , 2),
    TELLURIUM                                         ("Tellurium"                                       , "tellurium"                                       , ItemType.ELEMENT         , 2),
    TIN                                               ("Tin"                                             , "tin"                                             , ItemType.ELEMENT         , 1),
    TUNGSTEN                                          ("Tungsten"                                        , "tungsten"                                        , ItemType.ELEMENT         , 0),
    VANADIUM                                          ("Vanadium"                                        , "vanadium"                                        , ItemType.ELEMENT         , -1),
    YTTRIUM                                           ("Yttrium"                                         , "yttrium"                                         , ItemType.ELEMENT         , 1),
    ZINC                                              ("Zinc"                                            , "zinc"                                            , ItemType.ELEMENT         , -1),
    ZIRCONIUM                                         ("Zirconium"                                       , "zirconium"                                       , ItemType.ELEMENT         , 0),

    // MANUFACTURED
    BASIC_CONDUCTORS                                  ("Basic Conductors"                                , "basicconductors"                                 , ItemType.MANUFACTURED    , -2),
    BIOTECH_CONDUCTORS                                ("Biotech Conductors"                              , "biotechconductors"                               , ItemType.MANUFACTURED    , 2),
    CHEMICAL_DISTILLERY                               ("Chemical Distillery"                             , "chemicaldistillery"                              , ItemType.MANUFACTURED    , 0),
    CHEMICAL_MANIPULATORS                             ("Chemical Manipulators"                           , "chemicalmanipulators"                            , ItemType.MANUFACTURED    , 1),
    CHEMICAL_PROCESSORS                               ("Chemical Processors"                             , "chemicalprocessors"                              , ItemType.MANUFACTURED    , -1),
    CHEMICAL_STORAGE_UNITS                            ("Chemical Storage Units"                          , null                                              , ItemType.MANUFACTURED    , -2),
    COMPACT_COMPOSITES                                ("Compact Composites"                              , null                                              , ItemType.MANUFACTURED    , -2),
    COMPOUND_SHIELDING                                ("Compound Shielding"                              , "compoundshielding"                               , ItemType.MANUFACTURED    , 1),
    CONDUCTIVE_CERAMICS                               ("Conductive Ceramics"                             , "conductiveceramics"                              , ItemType.MANUFACTURED    , 0),
    CONDUCTIVE_COMPONENTS                             ("Conductive Components"                           , "conductivecomponents"                            , ItemType.MANUFACTURED    , -1),
    CONDUCTIVE_POLYMERS                               ("Conductive Polymers"                             , "conductivepolymers"                              , ItemType.MANUFACTURED    , 1),
    CONFIGURABLE_COMPONENTS                           ("Configurable Components"                         , "configurablecomponents"                          , ItemType.MANUFACTURED    , 1),
    CORE_DYNAMICS_COMPOSITES                          ("Core Dynamics Composites"                        , "fedcorecomposites"                               , ItemType.MANUFACTURED    , 2),
    CRYSTAL_SHARDS                                    ("Crystal Shards"                                  , null                                              , ItemType.MANUFACTURED    , -2),
    ELECTROCHEMICAL_ARRAYS                            ("Electrochemical Arrays"                          , "electrochemicalarrays"                           , ItemType.MANUFACTURED    , 0),
    EXQUISITE_FOCUS_CRYSTALS                          ("Exquisite Focus Crystals"                        , "exquisitefocuscrystals"                          , ItemType.MANUFACTURED    , 2),
    FILAMENT_COMPOSITES                               ("Filament Composites"                             , null                                              , ItemType.MANUFACTURED    , -1),
    FLAWED_FOCUS_CRYSTALS                             ("Flawed Focus Crystals"                           , "uncutfocuscrystals"                              , ItemType.MANUFACTURED    , -1),
    FOCUS_CRYSTALS                                    ("Focus Crystals"                                  , "focuscrystals"                                   , ItemType.MANUFACTURED    , 0),
    GALVANISING_ALLOYS                                ("Galvanising Alloys"                              , "galvanisingalloys"                               , ItemType.MANUFACTURED    , -1),
    GRID_RESISTORS                                    ("Grid Resistors"                                  , "gridresistors"                                   , ItemType.MANUFACTURED    , -2),
    HEAT_CONDUCTION_WIRING                            ("Heat Conduction Wiring"                          , "heatconductionwiring"                            , ItemType.MANUFACTURED    , -2),
    HEAT_DISPERSION_PLATE                             ("Heat Dispersion Plate"                           , "heatdispersionplate"                             , ItemType.MANUFACTURED    , -1),
    HEAT_EXCHANGERS                                   ("Heat Exchangers"                                 , "heatexchangers"                                  , ItemType.MANUFACTURED    , 0),
    HEAT_RESISTANT_CERAMICS                           ("Heat Resistant Ceramics"                         , null                                              , ItemType.MANUFACTURED    , -1),
    HEAT_VANES                                        ("Heat Vanes"                                      , "heatvanes"                                       , ItemType.MANUFACTURED    , 1),
    HIGH_DENSITY_COMPOSITES                           ("High Density Composites"                         , "highdensitycomposites"                           , ItemType.MANUFACTURED    , 0),
    HYBRID_CAPACITORS                                 ("Hybrid Capacitors"                               , "hybridcapacitors"                                , ItemType.MANUFACTURED    , -1),
    IMPERIAL_SHIELDING                                ("Imperial Shielding"                              , "imperialshielding"                               , ItemType.MANUFACTURED    , 2),
    IMPROVISED_COMPONENTS                             ("Improvised Components"                           , "improvisedcomponents"                            , ItemType.MANUFACTURED    , 2),
    MECHANICAL_COMPONENTS                             ("Mechanical Components"                           , "mechanicalcomponents"                            , ItemType.MANUFACTURED    , 0),
    MECHANICAL_EQUIPMENT                              ("Mechanical Equipment"                            , "mechanicalequipment"                             , ItemType.MANUFACTURED    , -1),
    MECHANICAL_SCRAP                                  ("Mechanical Scrap"                                , "mechanicalscrap"                                 , ItemType.MANUFACTURED    , -2),
    MILITARY_GRADE_ALLOYS                             ("Military Grade Alloys"                           , "militarygradealloys"                             , ItemType.MANUFACTURED    , 2),
    MILITARY_SUPERCAPACITORS                          ("Military Supercapacitors"                        , "militarysupercapacitors"                         , ItemType.MANUFACTURED    , 2),
    PHARMACEUTICAL_ISOLATORS                          ("Pharmaceutical Isolators"                        , "pharmaceuticalisolators"                         , ItemType.MANUFACTURED    , 2),
    PHASE_ALLOYS                                      ("Phase Alloys"                                    , "phasealloys"                                     , ItemType.MANUFACTURED    , 0),
    POLYMER_CAPACITORS                                ("Polymer Capacitors"                              , "polymercapacitors"                               , ItemType.MANUFACTURED    , 1),
    PRECIPITATED_ALLOYS                               ("Precipitated Alloys"                             , "precipitatedalloys"                              , ItemType.MANUFACTURED    , 0),
    PROPRIETARY_COMPOSITES                            ("Proprietary Composites"                          , "fedproprietarycomposites"                        , ItemType.MANUFACTURED    , 1),
    PROTO_HEAT_RADIATORS                              ("Proto Heat Radiators"                            , "protoheatradiators"                              , ItemType.MANUFACTURED    , 2),
    PROTO_LIGHT_ALLOYS                                ("Proto Light Alloys"                              , "protolightalloys"                                , ItemType.MANUFACTURED    , 1),
    PROTO_RADIOLIC_ALLOYS                             ("Proto Radiolic Alloys"                           , "protoradiolicalloys"                             , ItemType.MANUFACTURED    , 2),
    REFINED_FOCUS_CRYSTALS                            ("Refined Focus Crystals"                          , "refinedfocuscrystals"                            , ItemType.MANUFACTURED    , 1),
    SALVAGED_ALLOYS                                   ("Salvaged Alloys"                                 , "salvagedalloys"                                  , ItemType.MANUFACTURED    , -2),
    SHIELD_EMITTERS                                   ("Shield Emitters"                                 , "shieldemitters"                                  , ItemType.MANUFACTURED    , -1),
    SHIELDING_SENSORS                                 ("Shielding Sensors"                               , "shieldingsensors"                                , ItemType.MANUFACTURED    , 0),
    TEMPERED_ALLOYS                                   ("Tempered Alloys"                                 , null                                              , ItemType.MANUFACTURED    , -2),
    THERMIC_ALLOYS                                    ("Thermic Alloys"                                  , "thermicalloys"                                   , ItemType.MANUFACTURED    , 1),
    UNKNOWN_FRAGMENT                                  ("Unknown Fragment"                                , "unknownenergysource"                             , ItemType.MANUFACTURED    , 2),
    WORN_SHIELD_EMITTERS                              ("Worn Shield Emitters"                            , "wornshieldemitters"                              , ItemType.MANUFACTURED    , -2),

    // DATA
    ABERRANT_SHIELD_PATTERN_ANALYSIS                  ("Aberrant Shield Pattern Analysis"                , "shieldpatternanalysis"                           , ItemType.DATA            , 1),
    ABNORMAL_COMPACT_EMISSION_DATA                    ("Abnormal Compact Emissions Data"                 , "compactemissionsdata"                            , ItemType.DATA            , 2),
    ADAPTIVE_ENCRYPTORS_CAPTURE                       ("Adaptive Encryptors Capture"                     , "adaptiveencryptors"                              , ItemType.DATA            , 2),
    ANOMALOUS_BULK_SCAN_DATA                          ("Anomalous Bulk Scan Data"                        , "bulkscandata"                                    , ItemType.DATA            , -2),
    ANOMALOUS_FSD_TELEMETRY                           ("Anomalous FSD Telemetry"                         , "fsdtelemetry"                                    , ItemType.DATA            , -1),
    ATYPICAL_DISRUPTED_WAKE_ECHOES                    ("Atypical Disrupted Wake Echoes"                  , "disruptedwakeechoes"                             , ItemType.DATA            , -2),
    ATYPICAL_ENCRYPTION_ARCHIVES                      ("Atypical Encryption Archives"                    , "encryptionarchives"                              , ItemType.DATA            , 1),
    CLASSIFIED_SCAN_DATABANKS                         ("Classified Scan Databanks"                       , "scandatabanks"                                   , ItemType.DATA            , 0),
    CLASSIFIED_SCAN_FRAGMENT                          ("Classified Scan Fragment"                        , "classifiedscandata"                              , ItemType.DATA            , 2),
    CRACKED_INDUSTRIAL_FIRMWARE                       ("Cracked Industrial Firmware"                     , "industrialfirmware"                              , ItemType.DATA            , 0),
    DATAMINED_WAKE_EXCEPTIONS                         ("Datamined Wake Exceptions"                       , "dataminedwake"                                   , ItemType.DATA            , 2),
    DECODED_EMISSION_DATA                             ("Decoded Emission Data"                           , "decodedemissiondata"                             , ItemType.DATA            , 1),
    DISTORTED_SHIELD_CYCLE_RECORDINGS                 ("Distorted Shield Cycle Recordings"               , "shieldcyclerecordings"                           , ItemType.DATA            , -2),
    DIVERGENT_SCAN_DATA                               ("Divergent Scan Data"                             , "encodedscandata"                                 , ItemType.DATA            , 1),
    ECCENTRIC_HYPERSPACE_TRAJECTORIES                 ("Eccentric Hyperspace Trajectories"               , "hyperspacetrajectories"                          , ItemType.DATA            , 1),
    EXCEPTIONAL_SCRAMBLED_EMISSION_DATA               ("Exceptional Scrambled Emission Data"             , "scrambledemissiondata"                           , ItemType.DATA            , -2),
    INCONSISTENT_SHIELD_SOAK_ANALYSIS                 ("Inconsistent Shield Soak Analysis"               , "shieldsoakanalysis"                              , ItemType.DATA            , -1),
    IRREGULAR_EMISSION_DATA                           ("Irregular Emission Data"                         , "archivedemissiondata"                            , ItemType.DATA            , -1),
    MODIFIED_CONSUMER_FIRMWARE                        ("Modified Consumer Firmware"                      , "consumerfirmware"                                , ItemType.DATA            , -1),
    MODIFIED_EMBEDDED_FIRMWARE                        ("Modified Embedded Firmware"                      , "embeddedfirmware"                                , ItemType.DATA            , 2),
    OPEN_SYMMETRIC_KEYS                               ("Open Symmetric Keys"                             , "symmetrickeys"                                   , ItemType.DATA            , 0),
    PECULIAR_SHIELD_FREQUENCY_DATA                    ("Peculiar Shield Frequency Data"                  , "shieldfrequencydata"                             , ItemType.DATA            , 2),
    SECURITY_FIRMWARE_PATCH                           ("Security Firmware Patch"                         , "securityfirmware"                                , ItemType.DATA            , 1),
    SPECIALISED_LEGACY_FIRMWARE                       ("Specialised Legacy Firmware"                     , "legacyfirmware"                                  , ItemType.DATA            , -2),
    STRANGE_WAKE_SOLUTIONS                            ("Strange Wake Solutions"                          , "wakesolutions"                                   , ItemType.DATA            , 0),
    TAGGED_ENCRYPTION_CODES                           ("Tagged Encryption Codes"                         , "encryptioncodes"                                 , ItemType.DATA            , -1),
    UNEXPECTED_EMISSION_DATA                          ("Unexpected Emission Data"                        , "emissiondata"                                    , ItemType.DATA            , 0),
    UNIDENTIFIED_SCAN_ARCHIVES                        ("Unidentified Scan Archives"                      , "scanarchives"                                    , ItemType.DATA            , -1),
    UNTYPICAL_SHIELD_SCANS                            ("Untypical Shield Scans"                          , "shielddensityreports"                            , ItemType.DATA            , 0),
    UNUSUAL_ENCRYPTED_FILES                           ("Unusual Encrypted Files"                         , "encryptedfiles"                                  , ItemType.DATA            , -2),

    // ENGINEER COMMODITIES
    ARTICULATION_MOTORS                               ("Articulation Motors"                             , "articulationmotors"                              , ItemType.COMMODITY       , 0),
    BROMELLITE                                        ("Bromellite"                                      , "bromellite"                                      , ItemType.COMMODITY       , 0),
    CMM_COMPOSITE                                     ("CMM Composite"                                   , "cmmcomposite"                                    , ItemType.COMMODITY       , 0),
    EMERGENCY_POWER_CELLS                             ("Emergency Power Cells"                           , "emergencypowercells"                             , ItemType.COMMODITY       , 0),
    ENERGY_GRID_ASSEMBLY                              ("Energy Grid Assembly"                            , null                                              , ItemType.COMMODITY       , 0),
    EXHAUST_MANIFOLD                                  ("Exhaust Manifold"                                , "exhaustmanifold"                                 , ItemType.COMMODITY       , 0),
    HN_SHOCK_MOUNT                                    ("HN Shock Mount"                                  , null                                              , ItemType.COMMODITY       , 0),
    HARDWARE_DIAGNOSTIC_SENSOR                        ("Hardware Diagnostic Sensor"                      , null                                              , ItemType.COMMODITY       , 0),
    HEATSINK_INTERLINK                                ("Heatsink Interlink"                              , "heatsinkinterlink"                               , ItemType.COMMODITY       , 0),
    INSULATING_MEMBRANE                               ("Insulating Membrane"                             , null                                              , ItemType.COMMODITY       , 0),
    ION_DISTRIBUTOR                                   ("Ion Distributor"                                 , "iondistributor"                                  , ItemType.COMMODITY       , 0),
    MAGNETIC_EMITTER_COIL                             ("Magnetic Emitter Coil"                           , "magneticemittercoil"                             , ItemType.COMMODITY       , 0),
    MICRO_CONTROLLERS                                 ("Micro Controllers"                               , null                                              , ItemType.COMMODITY       , 0),
    MICROWEAVE_COOLING_HOSES                          ("Micro-Weave Cooling Hoses"                       , "coolinghoses"                                    , ItemType.COMMODITY       , 0),
    MODULAR_TERMINALS                                 ("Modular Terminals"                               , "modularterminals"                                , ItemType.COMMODITY       , 0),
    NANOBREAKERS                                      ("Nanobreakers"                                    , "nanobreakers"                                    , ItemType.COMMODITY       , 0),
    NEOFABRIC_INSULATION                              ("Neofabric Insulation"                            , "neofabricinsulation"                             , ItemType.COMMODITY       , 0),
    OSMIUM                                            ("Osmium"                                          , "osmium"                                          , ItemType.COMMODITY       , 0),
    PLATINUM                                          ("Platinum"                                        , "platinum"                                        , ItemType.COMMODITY       , 0),
    POWER_CONVERTER                                   ("Power Converter"                                 , null                                              , ItemType.COMMODITY       , 0),
    POWER_TRANSFER_BUS                                ("Power Transfer Bus"                              , null                                              , ItemType.COMMODITY       , 0),
    PRASEODYMIUM                                      ("Praseodymium"                                    , "praseodymium"                                    , ItemType.COMMODITY       , 0),
    RADIATION_BAFFLE                                  ("Radiation Baffle"                                , "radiationbaffle"                                 , ItemType.COMMODITY       , 0),
    REINFORCED_MOUNTING_PLATE                         ("Reinforced Mounting Plate"                       , null                                              , ItemType.COMMODITY       , 0),
    SAMARIUM                                          ("Samarium"                                        , "samarium"                                        , ItemType.COMMODITY       , 0),
    TELEMETRY_SUITE                                   ("Telemetry Suite"                                 , "telemetrysuite"                                  , ItemType.COMMODITY       , 0),

    // Rare commodities
    ALTAIRIAN_SKIN("ALTAIRIAN SKIN", "altairianskin", ItemType.COMMODITY, 0),
    FUJIN_TEA("FUJIN TEA", "fujintea", ItemType.COMMODITY, 0),
    KONGGA_ALE("KONGGA ALE", "konggaale", ItemType.COMMODITY, 0),
    META_ALLOYS("META ALLOYS", "metaalloys", ItemType.COMMODITY, 0),
    TOXANDJI_VIROCIDE("TOXANDJI VIROCIDE", "toxandjivirocide", ItemType.COMMODITY, 0),

    // Other commodities
    AUTO_FABRICATORS("AUTO FABRICATORS", "autofabricators", ItemType.COMMODITY, 0),
    BASIC_MEDICINES("BASIC MEDICINES", "basicmedicines", ItemType.COMMODITY, 0),
    BERTRANDITE("BERTRANDITE", "bertrandite", ItemType.COMMODITY, 0),
    BIOWASTE("BIOWASTE", "biowaste", ItemType.COMMODITY, 0),
    CLOTHING("CLOTHING", "clothing", ItemType.COMMODITY, 0),
    CONSUMER_TECHNOLOGY("CONSUMER TECHNOLOGY", "consumertechnology", ItemType.COMMODITY, 0),
    DOMESTIC_APPLIANCES("DOMESTIC APPLIANCES", "domesticappliances", ItemType.COMMODITY, 0),
    EXPLOSIVES("EXPLOSIVES", "explosives", ItemType.COMMODITY, 0),
    FISH("FISH", "fish", ItemType.COMMODITY, 0),
    GALLITE("GALLITE", "gallite", ItemType.COMMODITY, 0),
    GOLD("GOLD", "gold", ItemType.COMMODITY, 0),
    HYDROGEN_FUEL("HYDROGEN FUEL", "hydrogenfuel", ItemType.COMMODITY, 0),
    HYDROGEN_PEROXIDE("HYDROGEN PEROXIDE", "hydrogenperoxide", ItemType.COMMODITY, 0),
    INDITE("INDITE", "indite", ItemType.COMMODITY, 0),
    LIQUID_OXYGEN("LIQUID OXYGEN", "liquidoxygen", ItemType.COMMODITY, 0),
    LITHIUM_HYDROXIDE("LITHIUM HYDROXIDE", "lithiumhydroxide", ItemType.COMMODITY, 0),
    MARINE_EQUIPMENT("MARINE EQUIPMENT", "marinesupplies", ItemType.COMMODITY, 0),
    METHANE_CLATHRATE("METHANE CLATHRATE", "methaneclathrate", ItemType.COMMODITY, 0),
    METHANOL_MONOHYDRATE_CRYSTALS("METHANOL MONOHYDRATE CRYSTALS", "methanolmonohydratecrystals", ItemType.COMMODITY, 0),
    PALLADIUM("PALLADIUM", "palladium", ItemType.COMMODITY, 0),
    PAINITE("PAINITE", "painite", ItemType.COMMODITY, 0),
    PESTICIDES("PESTICIDES", "pesticides", ItemType.COMMODITY, 0),
    SILVER("SILVER", "silver", ItemType.COMMODITY, 0),
    SUPERCONDUCTORS("SUPERCONDUCTORS", "superconductors", ItemType.COMMODITY, 0),
    SYNTHETIC_MEAT("SYNTHETIC MEAT", "syntheticmeat", ItemType.COMMODITY, 0),
    TANTALUM("TANTALUM", "tantalum", ItemType.COMMODITY, 0),
    TITANIUM("TITANIUM", "titanium", ItemType.COMMODITY, 0),
    WATER("WATER", "water", ItemType.COMMODITY, 0),

    DRONES("DRONES", "drones", ItemType.DRONES, 0);
    //@formatter:on

    private static final SortedMap<ItemType, List<Item>> ITEMS_BY_TYPE = new TreeMap<>();
    private static final SortedMap<String, Item> ITEM_BY_NAME = new TreeMap<>();
    private static final SortedMap<String, Item> ITEM_BY_JOURNALNAME = new TreeMap<>();

    static {
        for (Item item : Item.values()) {
            List<Item> typeList = ITEMS_BY_TYPE.getOrDefault(item.getType(), new ArrayList<>());
            typeList.add(item);
            ITEMS_BY_TYPE.put(item.getType(), typeList);

            ITEM_BY_NAME.put(item.getName().toUpperCase(), item);

            if (item.getJournalName() != null) {
                ITEM_BY_JOURNALNAME.put(item.getJournalName().toLowerCase(), item);
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
        return ITEM_BY_NAME.get(name.toUpperCase().replace("ABBERANT", "ABERRANT"));
    }

    public static Item byJournalName(String journalName) {
        return ITEM_BY_JOURNALNAME.get(journalName.toLowerCase());
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
        ELEMENT, MANUFACTURED, DATA, COMMODITY, DRONES;
    }

}
