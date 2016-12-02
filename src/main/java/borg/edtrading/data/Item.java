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
    CHEMICAL_STORAGE_UNITS                            ("Chemical Storage Units"                          , "chemicalstorageunits"                            , ItemType.MANUFACTURED    , -2),
    COMPACT_COMPOSITES                                ("Compact Composites"                              , "compactcomposites"                               , ItemType.MANUFACTURED    , -2),
    COMPOUND_SHIELDING                                ("Compound Shielding"                              , "compoundshielding"                               , ItemType.MANUFACTURED    , 1),
    CONDUCTIVE_CERAMICS                               ("Conductive Ceramics"                             , "conductiveceramics"                              , ItemType.MANUFACTURED    , 0),
    CONDUCTIVE_COMPONENTS                             ("Conductive Components"                           , "conductivecomponents"                            , ItemType.MANUFACTURED    , -1),
    CONDUCTIVE_POLYMERS                               ("Conductive Polymers"                             , "conductivepolymers"                              , ItemType.MANUFACTURED    , 1),
    CONFIGURABLE_COMPONENTS                           ("Configurable Components"                         , "configurablecomponents"                          , ItemType.MANUFACTURED    , 1),
    CORE_DYNAMICS_COMPOSITES                          ("Core Dynamics Composites"                        , "fedcorecomposites"                               , ItemType.MANUFACTURED    , 2),
    CRYSTAL_SHARDS                                    ("Crystal Shards"                                  , "crystalshards"                                   , ItemType.MANUFACTURED    , -2),
    ELECTROCHEMICAL_ARRAYS                            ("Electrochemical Arrays"                          , "electrochemicalarrays"                           , ItemType.MANUFACTURED    , 0),
    EXQUISITE_FOCUS_CRYSTALS                          ("Exquisite Focus Crystals"                        , "exquisitefocuscrystals"                          , ItemType.MANUFACTURED    , 2),
    FILAMENT_COMPOSITES                               ("Filament Composites"                             , "filamentcomposites"                              , ItemType.MANUFACTURED    , -1),
    FLAWED_FOCUS_CRYSTALS                             ("Flawed Focus Crystals"                           , "uncutfocuscrystals"                              , ItemType.MANUFACTURED    , -1),
    FOCUS_CRYSTALS                                    ("Focus Crystals"                                  , "focuscrystals"                                   , ItemType.MANUFACTURED    , 0),
    GALVANISING_ALLOYS                                ("Galvanising Alloys"                              , "galvanisingalloys"                               , ItemType.MANUFACTURED    , -1),
    GRID_RESISTORS                                    ("Grid Resistors"                                  , "gridresistors"                                   , ItemType.MANUFACTURED    , -2),
    HEAT_CONDUCTION_WIRING                            ("Heat Conduction Wiring"                          , "heatconductionwiring"                            , ItemType.MANUFACTURED    , -2),
    HEAT_DISPERSION_PLATE                             ("Heat Dispersion Plate"                           , "heatdispersionplate"                             , ItemType.MANUFACTURED    , -1),
    HEAT_EXCHANGERS                                   ("Heat Exchangers"                                 , "heatexchangers"                                  , ItemType.MANUFACTURED    , 0),
    HEAT_RESISTANT_CERAMICS                           ("Heat Resistant Ceramics"                         , "heatresistantceramics"                           , ItemType.MANUFACTURED    , -1),
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
    TEMPERED_ALLOYS                                   ("Tempered Alloys"                                 , "temperedalloys"                                  , ItemType.MANUFACTURED    , -2),
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

    // COMMODITIES
    EXPLOSIVES                                        ("Explosives"                                      , "explosives"                                      , ItemType.COMMODITY       , "Chemicals"              , 0),
    HIP_ORGANOPHOSPHATES                              ("HIP Organophosphates"                            , "hiporganophosphates"                             , ItemType.COMMODITY       , "Chemicals"              , 2),
    HYDROGEN_FUEL                                     ("Hydrogen Fuel"                                   , "hydrogenfuel"                                    , ItemType.COMMODITY       , "Chemicals"              , 0),
    HYDROGEN_PEROXIDE                                 ("Hydrogen Peroxide"                               , "hydrogenperoxide"                                , ItemType.COMMODITY       , "Chemicals"              , 0),
    KORO_KUNG_PELLETS                                 ("Koro Kung Pellets"                               , "korokungpellets"                                 , ItemType.COMMODITY       , "Chemicals"              , 2),
    LIQUID_OXYGEN                                     ("Liquid Oxygen"                                   , "liquidoxygen"                                    , ItemType.COMMODITY       , "Chemicals"              , 0),
    MINERAL_OIL                                       ("Mineral Oil"                                     , "mineraloil"                                      , ItemType.COMMODITY       , "Chemicals"              , 0),
    NERVE_AGENTS                                      ("Nerve Agents"                                    , "nerveagents"                                     , ItemType.COMMODITY       , "Chemicals"              , 0),
    PESTICIDES                                        ("Pesticides"                                      , "pesticides"                                      , ItemType.COMMODITY       , "Chemicals"              , 0),
    SURFACE_STABILISERS                               ("Surface Stabilisers"                             , "surfacestabilisers"                              , ItemType.COMMODITY       , "Chemicals"              , 0),
    SYNTHETIC_REAGENTS                                ("Synthetic Reagents"                              , "syntheticreagents"                               , ItemType.COMMODITY       , "Chemicals"              , 0),
    TOXANDJI_VIROCIDE                                 ("Toxandji Virocide"                               , "toxandjivirocide"                                , ItemType.COMMODITY       , "Chemicals"              , 2),
    WATER                                             ("Water"                                           , "water"                                           , ItemType.COMMODITY       , "Chemicals"              , 0),
    ALACARAKMO_SKIN_ART                               ("Alacarakmo Skin Art"                             , "alacarakmoskinart"                               , ItemType.COMMODITY       , "Consumer Items"         , 2),
    ALTAIRIAN_SKIN                                    ("Altairian Skin"                                  , "altairianskin"                                   , ItemType.COMMODITY       , "Consumer Items"         , 2),
    CLOTHING                                          ("Clothing"                                        , "clothing"                                        , ItemType.COMMODITY       , "Consumer Items"         , 0),
    CONSUMER_TECHNOLOGY                               ("Consumer Technology"                             , "consumertechnology"                              , ItemType.COMMODITY       , "Consumer Items"         , 0),
    CRYSTALLINE_SPHERES                               ("Crystalline Spheres"                             , "crystallinespheres"                              , ItemType.COMMODITY       , "Consumer Items"         , 2),
    DOMESTIC_APPLIANCES                               ("Domestic Appliances"                             , "domesticappliances"                              , ItemType.COMMODITY       , "Consumer Items"         , 0),
    ELEU_THERMALS                                     ("Eleu Thermals"                                   , "eleuthermals"                                    , ItemType.COMMODITY       , "Consumer Items"         , 2),
    ESHU_UMBRELLAS                                    ("Eshu Umbrellas"                                  , "eshuumbrellas"                                   , ItemType.COMMODITY       , "Consumer Items"         , 2),
    EVACUATION_SHELTER                                ("Evacuation Shelter"                              , "evacuationshelter"                               , ItemType.COMMODITY       , "Consumer Items"         , 0),
    HAVASUPAI_DREAM_CATCHER                           ("Havasupai Dream Catcher"                         , "havasupaidreamcatcher"                           , ItemType.COMMODITY       , "Consumer Items"         , 2),
    JAQUES_QUINENTIAN_STILL                           ("Jaques Quinentian Still"                         , "jaquesquinentianstill"                           , ItemType.COMMODITY       , "Consumer Items"         , 2),
    JARADHARRE_PUZZLE_BOX                             ("Jaradharre Puzzle Box"                           , "jaradharrepuzzlebox"                             , ItemType.COMMODITY       , "Consumer Items"         , 2),
    JOTUN_MOOKAH                                      ("Jotun Mookah"                                    , "jotunmookah"                                     , ItemType.COMMODITY       , "Consumer Items"         , 2),
    KARETII_COUTURE                                   ("Karetii Couture"                                 , "karetiicouture"                                  , ItemType.COMMODITY       , "Consumer Items"         , 2),
    KINAGO_VIOLINS                                    ("Kinago Violins"                                  , "kinagoviolins"                                   , ItemType.COMMODITY       , "Consumer Items"         , 2),
    LEATHERY_EGGS                                     ("Leathery Eggs"                                   , "leatheryeggs"                                    , ItemType.COMMODITY       , "Consumer Items"         , 2),
    MOMUS_BOG_SPANIEL                                 ("Momus Bog Spaniel"                               , "momusbogspaniel"                                 , ItemType.COMMODITY       , "Consumer Items"         , 2),
    NGUNA_MODERN_ANTIQUES                             ("Nguna Modern Antiques"                           , "ngunamodernantiques"                             , ItemType.COMMODITY       , "Consumer Items"         , 2),
    NJANGARI_SADDLES                                  ("Njangari Saddles"                                , "njangarisaddles"                                 , ItemType.COMMODITY       , "Consumer Items"         , 2),
    OPHIUCH_EXINO_ARTEFACTS                           ("Ophiuch Exino Artefacts"                         , "ophiuchexinoartefacts"                           , ItemType.COMMODITY       , "Consumer Items"         , 2),
    RAJUKRU_MULTISTOVES                               ("Rajukru Multi-Stoves"                            , "rajukrumultistoves"                              , ItemType.COMMODITY       , "Consumer Items"         , 2),
    SOONTILL_RELICS                                   ("Soontill Relics"                                 , "soontillrelics"                                  , ItemType.COMMODITY       , "Consumer Items"         , 2),
    SURVIVAL_EQUIPMENT                                ("Survival Equipment"                              , "survivalequipment"                               , ItemType.COMMODITY       , "Consumer Items"         , 0),
    THE_HUTTON_MUG                                    ("The Hutton Mug"                                  , "thehuttonmug"                                    , ItemType.COMMODITY       , "Consumer Items"         , 2),
    TIOLCE_WASTE2PASTE_UNITS                          ("Tiolce Waste2Paste Units"                        , "tiolcewaste2pasteunits"                          , ItemType.COMMODITY       , "Consumer Items"         , 2),
    UZUMOKU_LOWG_WINGS                                ("Uzumoku Low-G Wings"                             , "uzumokulowgwings"                                , ItemType.COMMODITY       , "Consumer Items"         , 2),
    VIDAVANTIAN_LACE                                  ("Vidavantian Lace"                                , "vidavantianlace"                                 , ItemType.COMMODITY       , "Consumer Items"         , 2),
    ZEESSZE_ANT_GRUB_GLUE                             ("Zeessze Ant Grub Glue"                           , "zeesszeantgrubglue"                              , ItemType.COMMODITY       , "Consumer Items"         , 2),
    AEPYORNIS_EGG                                     ("Aepyornis Egg"                                   , "aepyornisegg"                                    , ItemType.COMMODITY       , "Foods"                  , 2),
    ALBINO_QUECHUA_MAMMOTH                            ("Albino Quechua Mammoth"                          , "albinoquechuamammoth"                            , ItemType.COMMODITY       , "Foods"                  , 2),
    ALGAE                                             ("Algae"                                           , "algae"                                           , ItemType.COMMODITY       , "Foods"                  , 0),
    ANIMAL_MEAT                                       ("Animal Meat"                                     , "animalmeat"                                      , ItemType.COMMODITY       , "Foods"                  , 0),
    ANY_NA_COFFEE                                     ("Any Na Coffee"                                   , "anynacoffee"                                     , ItemType.COMMODITY       , "Foods"                  , 2),
    AROUCA_CONVENTUAL_SWEETS                          ("Arouca Conventual Sweets"                        , "aroucaconventualsweets"                          , ItemType.COMMODITY       , "Foods"                  , 2),
    BALTAH_SINE_VACUUM_KRILL                          ("Baltah Sine Vacuum Krill"                        , "baltahsinevacuumkrill"                           , ItemType.COMMODITY       , "Foods"                  , 2),
    CD75_KITTEN_BRAND_COFFEE                          ("CD-75 Kitten Brand Coffee"                       , "cd75kittenbrandcoffee"                           , ItemType.COMMODITY       , "Foods"                  , 2),
    CEREMONIAL_HEIKE_TEA                              ("Ceremonial Heike Tea"                            , "ceremonialheiketea"                              , ItemType.COMMODITY       , "Foods"                  , 2),
    CETI_RABBITS                                      ("Ceti Rabbits"                                    , "cetirabbits"                                     , ItemType.COMMODITY       , "Foods"                  , 2),
    CHI_ERIDANI_MARINE_PASTE                          ("Chi Eridani Marine Paste"                        , "chieridanimarinepaste"                           , ItemType.COMMODITY       , "Foods"                  , 2),
    COFFEE                                            ("Coffee"                                          , "coffee"                                          , ItemType.COMMODITY       , "Foods"                  , 0),
    COQUIM_SPONGIFORM_VICTUALS                        ("Coquim Spongiform Victuals"                      , "coquimspongiformvictuals"                        , ItemType.COMMODITY       , "Foods"                  , 2),
    DEURINGAS_TRUFFLES                                ("Deuringas Truffles"                              , "deuringastruffles"                               , ItemType.COMMODITY       , "Foods"                  , 2),
    DISO_MA_CORN                                      ("Diso Ma Corn"                                    , "disomacorn"                                      , ItemType.COMMODITY       , "Foods"                  , 2),
    EDEN_APPLES_OF_AERIAL                             ("Eden Apples Of Aerial"                           , "edenapplesofaerial"                              , ItemType.COMMODITY       , "Foods"                  , 2),
    ESUSEKU_CAVIAR                                    ("Esuseku Caviar"                                  , "esusekucaviar"                                   , ItemType.COMMODITY       , "Foods"                  , 2),
    ETHGREZE_TEA_BUDS                                 ("Ethgreze Tea Buds"                               , "ethgrezeteabuds"                                 , ItemType.COMMODITY       , "Foods"                  , 2),
    FISH                                              ("Fish"                                            , "fish"                                            , ItemType.COMMODITY       , "Foods"                  , 0),
    FOOD_CARTRIDGES                                   ("Food Cartridges"                                 , "foodcartridges"                                  , ItemType.COMMODITY       , "Foods"                  , 0),
    FRUIT_AND_VEGETABLES                              ("Fruit and Vegetables"                            , "fruitandvegetables"                              , ItemType.COMMODITY       , "Foods"                  , 0),
    GIANT_IRUKAMA_SNAILS                              ("Giant Irukama Snails"                            , "giantirukamasnails"                              , ItemType.COMMODITY       , "Foods"                  , 2),
    GOMAN_YAUPON_COFFEE                               ("Goman Yaupon Coffee"                             , "gomanyauponcoffee"                               , ItemType.COMMODITY       , "Foods"                  , 2),
    GRAIN                                             ("Grain"                                           , "grain"                                           , ItemType.COMMODITY       , "Foods"                  , 0),
    HAIDNE_BLACK_BREW                                 ("Haidne Black Brew"                               , "haidneblackbrew"                                 , ItemType.COMMODITY       , "Foods"                  , 2),
    HELVETITJ_PEARLS                                  ("Helvetitj Pearls"                                , "helvetitjpearls"                                 , ItemType.COMMODITY       , "Foods"                  , 2),
    HIP_10175_BUSH_MEAT                               ("HIP 10175 Bush Meat"                             , "hip10175bushmeat"                                , ItemType.COMMODITY       , "Foods"                  , 2),
    HIP_PROTOSQUID                                    ("HIP Proto-Squid"                                 , "hipprotosquid"                                   , ItemType.COMMODITY       , "Foods"                  , 2),
    HR_7221_WHEAT                                     ("HR 7221 Wheat"                                   , "hr7221wheat"                                     , ItemType.COMMODITY       , "Foods"                  , 2),
    JAROUA_RICE                                       ("Jaroua Rice"                                     , "jarouarice"                                      , ItemType.COMMODITY       , "Foods"                  , 2),
    KARSUKI_LOCUSTS                                   ("Karsuki Locusts"                                 , "karsukilocusts"                                  , ItemType.COMMODITY       , "Foods"                  , 2),
    LIVE_HECATE_SEA_WORMS                             ("Live Hecate Sea Worms"                           , "livehecateseaworms"                              , ItemType.COMMODITY       , "Foods"                  , 2),
    LTT_HYPERSWEET                                    ("LTT Hypersweet"                                  , "ltthypersweet"                                   , ItemType.COMMODITY       , "Foods"                  , 2),
    MECHUCOS_HIGH_TEA                                 ("Mechucos High Tea"                               , "mechucoshightea"                                 , ItemType.COMMODITY       , "Foods"                  , 2),
    MOKOJING_BEAST_FEAST                              ("Mokojing Beast Feast"                            , "mokojingbeastfeast"                              , ItemType.COMMODITY       , "Foods"                  , 2),
    MUKUSUBII_CHITINOS                                ("Mukusubii Chitin-Os"                             , "mukusubiichitinos"                               , ItemType.COMMODITY       , "Foods"                  , 2),
    MULACHI_GIANT_FUNGUS                              ("Mulachi Giant Fungus"                            , "mulachigiantfungus"                              , ItemType.COMMODITY       , "Foods"                  , 2),
    NERITUS_BERRIES                                   ("Neritus Berries"                                 , "neritusberries"                                  , ItemType.COMMODITY       , "Foods"                  , 2),
    OCHOENG_CHILLIES                                  ("Ochoeng Chillies"                                , "ochoengchillies"                                 , ItemType.COMMODITY       , "Foods"                  , 2),
    ORRERIAN_VICIOUS_BREW                             ("Orrerian Vicious Brew"                           , "orrerianviciousbrew"                             , ItemType.COMMODITY       , "Foods"                  , 2),
    SANUMA_DECORATIVE_MEAT                            ("Sanuma Decorative Meat"                          , "sanumadecorativemeat"                            , ItemType.COMMODITY       , "Foods"                  , 2),
    SYNTHETIC_MEAT                                    ("Synthetic Meat"                                  , "syntheticmeat"                                   , ItemType.COMMODITY       , "Foods"                  , 0),
    TANMARK_TRANQUIL_TEA                              ("Tanmark Tranquil Tea"                            , "tanmarktranquiltea"                              , ItemType.COMMODITY       , "Foods"                  , 2),
    TEA                                               ("Tea"                                             , "tea"                                             , ItemType.COMMODITY       , "Foods"                  , 0),
    USZAIAN_TREE_GRUB                                 ("Uszaian Tree Grub"                               , "uszaiantreegrub"                                 , ItemType.COMMODITY       , "Foods"                  , 2),
    UTGAROAR_MILLENNIAL_EGGS                          ("Utgaroar Millennial Eggs"                        , "utgaroarmillennialeggs"                          , ItemType.COMMODITY       , "Foods"                  , 2),
    VOID_EXTRACT_COFFEE                               ("Void Extract Coffee"                             , "voidextractcoffee"                               , ItemType.COMMODITY       , "Foods"                  , 2),
    WHEEMETE_WHEAT_CAKES                              ("Wheemete Wheat Cakes"                            , "wheemetewheatcakes"                              , ItemType.COMMODITY       , "Foods"                  , 2),
    WITCHHAUL_KOBE_BEEF                               ("Witchhaul Kobe Beef"                             , "witchhaulkobebeef"                               , ItemType.COMMODITY       , "Foods"                  , 2),
    CERAMIC_COMPOSITES                                ("Ceramic Composites"                              , "ceramiccomposites"                               , ItemType.COMMODITY       , "Industrial Materials"   , 0),
    CMM_COMPOSITE                                     ("CMM Composite"                                   , "cmmcomposite"                                    , ItemType.COMMODITY       , "Industrial Materials"   , 1),
    INSULATING_MEMBRANE                               ("Insulating Membrane"                             , "insulatingmembrane"                              , ItemType.COMMODITY       , "Industrial Materials"   , 1),
    MEDB_STARLUBE                                     ("Medb Starlube"                                   , "medbstarlube"                                    , ItemType.COMMODITY       , "Industrial Materials"   , 2),
    METAALLOYS                                        ("Meta-Alloys"                                     , "metaalloys"                                      , ItemType.COMMODITY       , "Industrial Materials"   , 0),
    MICROWEAVE_COOLING_HOSES                          ("Micro-Weave Cooling Hoses"                       , "microweavecoolinghoses"                          , ItemType.COMMODITY       , "Industrial Materials"   , 1),
    NEOFABRIC_INSULATION                              ("Neofabric Insulation"                            , "neofabricinsulation"                             , ItemType.COMMODITY       , "Industrial Materials"   , 1),
    POLYMERS                                          ("Polymers"                                        , "polymers"                                        , ItemType.COMMODITY       , "Industrial Materials"   , 0),
    SEMICONDUCTORS                                    ("Semiconductors"                                  , "semiconductors"                                  , ItemType.COMMODITY       , "Industrial Materials"   , 0),
    SUPERCONDUCTORS                                   ("Superconductors"                                 , "superconductors"                                 , ItemType.COMMODITY       , "Industrial Materials"   , 0),
    AZURE_MILK                                        ("Azure Milk"                                      , "azuremilk"                                       , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    BAST_SNAKE_GIN                                    ("Bast Snake Gin"                                  , "bastsnakegin"                                    , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    BEER                                              ("Beer"                                            , "beer"                                            , ItemType.COMMODITY       , "Legal Drugs"            , 0),
    BOOTLEG_LIQUOR                                    ("Bootleg Liquor"                                  , "bootlegliquor"                                   , ItemType.COMMODITY       , "Legal Drugs"            , 0),
    BURNHAM_BILE_DISTILLATE                           ("Burnham Bile Distillate"                         , "burnhambiledistillate"                           , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    CENTAURI_MEGA_GIN                                 ("Centauri Mega Gin"                               , "centaurimegagin"                                 , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    CHATEAU_DE_AEGAEON                                ("Chateau De Aegaeon"                              , "chateaudeaegaeon"                                , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    ERANIN_PEARL_WHISKEY                              ("Eranin Pearl Whiskey"                            , "eraninpearlwhiskey"                              , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    GEAWEN_DANCE_DUST                                 ("Geawen Dance Dust"                               , "geawendancedust"                                 , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    GERASIAN_GUEUZE_BEER                              ("Gerasian Gueuze Beer"                            , "gerasiangueuzebeer"                              , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    INDI_BOURBON                                      ("Indi Bourbon"                                    , "indibourbon"                                     , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    KAMITRA_CIGARS                                    ("Kamitra Cigars"                                  , "kamitracigars"                                   , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    KONGGA_ALE                                        ("Kongga Ale"                                      , "konggaale"                                       , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    LAVIAN_BRANDY                                     ("Lavian Brandy"                                   , "lavianbrandy"                                    , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    LEESTIAN_EVIL_JUICE                               ("Leestian Evil Juice"                             , "leestianeviljuice"                               , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    LIQUOR                                            ("Liquor"                                          , "liquor"                                          , ItemType.COMMODITY       , "Legal Drugs"            , 0),
    LUCAN_ONION_HEAD                                  ("Lucan Onion Head"                                , "lucanonionhead"                                  , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    LYRAE_WEED                                        ("Lyrae Weed"                                      , "lyraeweed"                                       , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    MOTRONA_EXPERIENCE_JELLY                          ("Motrona Experience Jelly"                        , "motronaexperiencejelly"                          , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    NARCOTICS                                         ("Narcotics"                                       , "narcotics"                                       , ItemType.COMMODITY       , "Legal Drugs"            , 0),
    ONION_HEAD                                        ("Onion Head"                                      , "onionhead"                                       , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    ONIONHEAD_ALPHA_STRAIN                            ("Onionhead Alpha Strain"                          , "onionheadalphastrain"                            , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    ONIONHEAD_BETA_STRAIN                             ("Onionhead Beta Strain"                           , "onionheadbetastrain"                             , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    PAVONIS_EAR_GRUBS                                 ("Pavonis Ear Grubs"                               , "pavoniseargrubs"                                 , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    RUSANI_OLD_SMOKEY                                 ("Rusani Old Smokey"                               , "rusanioldsmokey"                                 , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    SAXON_WINE                                        ("Saxon Wine"                                      , "saxonwine"                                       , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    TARACH_SPICE                                      ("Tarach Spice"                                    , "tarachspice"                                     , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    THRUTIS_CREAM                                     ("Thrutis Cream"                                   , "thrutiscream"                                    , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    TOBACCO                                           ("Tobacco"                                         , "tobacco"                                         , ItemType.COMMODITY       , "Legal Drugs"            , 0),
    WINE                                              ("Wine"                                            , "wine"                                            , ItemType.COMMODITY       , "Legal Drugs"            , 0),
    WOLF_FESH                                         ("Wolf Fesh"                                       , "wolffesh"                                        , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    WUTHIELO_KU_FROTH                                 ("Wuthielo Ku Froth"                               , "wuthielokufroth"                                 , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    YASO_KONDI_LEAF                                   ("Yaso Kondi Leaf"                                 , "yasokondileaf"                                   , ItemType.COMMODITY       , "Legal Drugs"            , 2),
    ARTICULATION_MOTORS                               ("Articulation Motors"                             , "articulationmotors"                              , ItemType.COMMODITY       , "Machinery"              , 1),
    ATMOSPHERIC_PROCESSORS                            ("Atmospheric Processors"                          , "atmosphericprocessors"                           , ItemType.COMMODITY       , "Machinery"              , 0),
    BUILDING_FABRICATORS                              ("Building Fabricators"                            , "buildingfabricators"                             , ItemType.COMMODITY       , "Machinery"              , 0),
    CROP_HARVESTERS                                   ("Crop Harvesters"                                 , "cropharvesters"                                  , ItemType.COMMODITY       , "Machinery"              , 0),
    EMERGENCY_POWER_CELLS                             ("Emergency Power Cells"                           , "emergencypowercells"                             , ItemType.COMMODITY       , "Machinery"              , 1),
    ENERGY_GRID_ASSEMBLY                              ("Energy Grid Assembly"                            , "energygridassembly"                              , ItemType.COMMODITY       , "Machinery"              , 1),
    EXHAUST_MANIFOLD                                  ("Exhaust Manifold"                                , "exhaustmanifold"                                 , ItemType.COMMODITY       , "Machinery"              , 1),
    GEOLOGICAL_EQUIPMENT                              ("Geological Equipment"                            , "geologicalequipment"                             , ItemType.COMMODITY       , "Machinery"              , 0),
    GIANT_VERRIX                                      ("Giant Verrix"                                    , "giantverrix"                                     , ItemType.COMMODITY       , "Machinery"              , 2),
    HEATSINK_INTERLINK                                ("Heatsink Interlink"                              , "heatsinkinterlink"                               , ItemType.COMMODITY       , "Machinery"              , 1),
    HN_SHOCK_MOUNT                                    ("HN Shock Mount"                                  , "hnshockmount"                                    , ItemType.COMMODITY       , "Machinery"              , 1),
    ION_DISTRIBUTOR                                   ("Ion Distributor"                                 , "iondistributor"                                  , ItemType.COMMODITY       , "Machinery"              , 1),
    MAGNETIC_EMITTER_COIL                             ("Magnetic Emitter Coil"                           , "magneticemittercoil"                             , ItemType.COMMODITY       , "Machinery"              , 1),
    MARINE_EQUIPMENT                                  ("Marine Equipment"                                , "marineequipment"                                 , ItemType.COMMODITY       , "Machinery"              , 0),
    MICROBIAL_FURNACES                                ("Microbial Furnaces"                              , "microbialfurnaces"                               , ItemType.COMMODITY       , "Machinery"              , 0),
    MINERAL_EXTRACTORS                                ("Mineral Extractors"                              , "mineralextractors"                               , ItemType.COMMODITY       , "Machinery"              , 0),
    MODULAR_TERMINALS                                 ("Modular Terminals"                               , "modularterminals"                                , ItemType.COMMODITY       , "Machinery"              , 1),
    NON_EUCLIDIAN_EXOTANKS                            ("Non Euclidian Exotanks"                          , "noneuclidianexotanks"                            , ItemType.COMMODITY       , "Machinery"              , 2),
    POWER_CONVERTER                                   ("Power Converter"                                 , "powerconverter"                                  , ItemType.COMMODITY       , "Machinery"              , 1),
    POWER_GENERATORS                                  ("Power Generators"                                , "powergenerators"                                 , ItemType.COMMODITY       , "Machinery"              , 0),
    POWER_TRANSFER_BUS                                ("Power Transfer Bus"                              , "powertransferbus"                                , ItemType.COMMODITY       , "Machinery"              , 1),
    RADIATION_BAFFLE                                  ("Radiation Baffle"                                , "radiationbaffle"                                 , ItemType.COMMODITY       , "Machinery"              , 1),
    REINFORCED_MOUNTING_PLATE                         ("Reinforced Mounting Plate"                       , "reinforcedmountingplate"                         , ItemType.COMMODITY       , "Machinery"              , 1),
    SKIMMER_COMPONENTS                                ("Skimmer Components"                              , "skimmercomponents"                               , ItemType.COMMODITY       , "Machinery"              , 0),
    THERMAL_COOLING_UNITS                             ("Thermal Cooling Units"                           , "thermalcoolingunits"                             , ItemType.COMMODITY       , "Machinery"              , 0),
    VOLKHAB_BEE_DRONES                                ("Volkhab Bee Drones"                              , "volkhabbeedrones"                                , ItemType.COMMODITY       , "Machinery"              , 2),
    WATER_PURIFIERS                                   ("Water Purifiers"                                 , "waterpurifiers"                                  , ItemType.COMMODITY       , "Machinery"              , 0),
    WULPA_HYPERBORE_SYSTEMS                           ("Wulpa Hyperbore Systems"                         , "wulpahyperboresystems"                           , ItemType.COMMODITY       , "Machinery"              , 2),
    ADVANCED_MEDICINES                                ("Advanced Medicines"                              , "advancedmedicines"                               , ItemType.COMMODITY       , "Medicines"              , 0),
    AGANIPPE_RUSH                                     ("Aganippe Rush"                                   , "aganipperush"                                    , ItemType.COMMODITY       , "Medicines"              , 2),
    AGRIMEDICINES                                     ("Agri-Medicines"                                  , "agrimedicines"                                   , ItemType.COMMODITY       , "Medicines"              , 0),
    ALYA_BODY_SOAP                                    ("Alya Body Soap"                                  , "alyabodysoap"                                    , ItemType.COMMODITY       , "Medicines"              , 2),
    BASIC_MEDICINES                                   ("Basic Medicines"                                 , "basicmedicines"                                  , ItemType.COMMODITY       , "Medicines"              , 0),
    COMBAT_STABILISERS                                ("Combat Stabilisers"                              , "combatstabilisers"                               , ItemType.COMMODITY       , "Medicines"              , 0),
    FUJIN_TEA                                         ("Fujin Tea"                                       , "fujintea"                                        , ItemType.COMMODITY       , "Medicines"              , 2),
    HONESTY_PILLS                                     ("Honesty Pills"                                   , "honestypills"                                    , ItemType.COMMODITY       , "Medicines"              , 2),
    KACHIRIGIN_FILTER_LEECHES                         ("Kachirigin Filter Leeches"                       , "kachiriginfilterleeches"                         , ItemType.COMMODITY       , "Medicines"              , 2),
    PANTAA_PRAYER_STICKS                              ("Pantaa Prayer Sticks"                            , "pantaaprayersticks"                              , ItemType.COMMODITY       , "Medicines"              , 2),
    PERFORMANCE_ENHANCERS                             ("Performance Enhancers"                           , "performanceenhancers"                            , ItemType.COMMODITY       , "Medicines"              , 0),
    PROGENITOR_CELLS                                  ("Progenitor Cells"                                , "progenitorcells"                                 , ItemType.COMMODITY       , "Medicines"              , 0),
    TAURI_CHIMES                                      ("Tauri Chimes"                                    , "taurichimes"                                     , ItemType.COMMODITY       , "Medicines"              , 2),
    TERRA_MATER_BLOOD_BORES                           ("Terra Mater Blood Bores"                         , "terramaterbloodbores"                            , ItemType.COMMODITY       , "Medicines"              , 2),
    V_HERCULIS_BODY_RUB                               ("V Herculis Body Rub"                             , "vherculisbodyrub"                                , ItemType.COMMODITY       , "Medicines"              , 2),
    VEGA_SLIMWEED                                     ("Vega Slimweed"                                   , "vegaslimweed"                                    , ItemType.COMMODITY       , "Medicines"              , 2),
    WATERS_OF_SHINTARA                                ("Waters Of Shintara"                              , "watersofshintara"                                , ItemType.COMMODITY       , "Medicines"              , 2),
    ALUMINIUM                                         ("Aluminium"                                       , "aluminium"                                       , ItemType.COMMODITY       , "Metals"                 , 0),
    BERYLLIUM                                         ("Beryllium"                                       , "beryllium"                                       , ItemType.COMMODITY       , "Metals"                 , 0),
    BISMUTH                                           ("Bismuth"                                         , "bismuth"                                         , ItemType.COMMODITY       , "Metals"                 , 0),
    COBALT                                            ("Cobalt"                                          , "cobalt"                                          , ItemType.COMMODITY       , "Metals"                 , 0),
    COPPER                                            ("Copper"                                          , "copper"                                          , ItemType.COMMODITY       , "Metals"                 , 0),
    GALLIUM                                           ("Gallium"                                         , "gallium"                                         , ItemType.COMMODITY       , "Metals"                 , 0),
    GOLD                                              ("Gold"                                            , "gold"                                            , ItemType.COMMODITY       , "Metals"                 , 0),
    HAFNIUM_178                                       ("Hafnium 178"                                     , "hafnium178"                                      , ItemType.COMMODITY       , "Metals"                 , 0),
    INDIUM                                            ("Indium"                                          , "indium"                                          , ItemType.COMMODITY       , "Metals"                 , 0),
    LANTHANUM                                         ("Lanthanum"                                       , "lanthanum"                                       , ItemType.COMMODITY       , "Metals"                 , 0),
    LITHIUM                                           ("Lithium"                                         , "lithium"                                         , ItemType.COMMODITY       , "Metals"                 , 0),
    OSMIUM                                            ("Osmium"                                          , "osmium"                                          , ItemType.COMMODITY       , "Metals"                 , 1),
    PALLADIUM                                         ("Palladium"                                       , "palladium"                                       , ItemType.COMMODITY       , "Metals"                 , 0),
    PLATINUM                                          ("Platinum"                                        , "platinum"                                        , ItemType.COMMODITY       , "Metals"                 , 1),
    PRASEODYMIUM                                      ("Praseodymium"                                    , "praseodymium"                                    , ItemType.COMMODITY       , "Metals"                 , 1),
    SAMARIUM                                          ("Samarium"                                        , "samarium"                                        , ItemType.COMMODITY       , "Metals"                 , 1),
    SILVER                                            ("Silver"                                          , "silver"                                          , ItemType.COMMODITY       , "Metals"                 , 0),
    SOTHIS_CRYSTALLINE_GOLD                           ("Sothis Crystalline Gold"                         , "sothiscrystallinegold"                           , ItemType.COMMODITY       , "Metals"                 , 2),
    TANTALUM                                          ("Tantalum"                                        , "tantalum"                                        , ItemType.COMMODITY       , "Metals"                 , 0),
    THALLIUM                                          ("Thallium"                                        , "thallium"                                        , ItemType.COMMODITY       , "Metals"                 , 0),
    THORIUM                                           ("Thorium"                                         , "thorium"                                         , ItemType.COMMODITY       , "Metals"                 , 0),
    TITANIUM                                          ("Titanium"                                        , "titanium"                                        , ItemType.COMMODITY       , "Metals"                 , 0),
    URANIUM                                           ("Uranium"                                         , "uranium"                                         , ItemType.COMMODITY       , "Metals"                 , 0),
    BAUXITE                                           ("Bauxite"                                         , "bauxite"                                         , ItemType.COMMODITY       , "Minerals"               , 0),
    BERTRANDITE                                       ("Bertrandite"                                     , "bertrandite"                                     , ItemType.COMMODITY       , "Minerals"               , 0),
    BROMELLITE                                        ("Bromellite"                                      , "bromellite"                                      , ItemType.COMMODITY       , "Minerals"               , 1),
    CHERBONES_BLOOD_CRYSTALS                          ("Cherbones Blood Crystals"                        , "cherbonesbloodcrystals"                          , ItemType.COMMODITY       , "Minerals"               , 2),
    COLTAN                                            ("Coltan"                                          , "coltan"                                          , ItemType.COMMODITY       , "Minerals"               , 0),
    CRYOLITE                                          ("Cryolite"                                        , "cryolite"                                        , ItemType.COMMODITY       , "Minerals"               , 0),
    GALLITE                                           ("Gallite"                                         , "gallite"                                         , ItemType.COMMODITY       , "Minerals"               , 0),
    GOSLARITE                                         ("Goslarite"                                       , "goslarite"                                       , ItemType.COMMODITY       , "Minerals"               , 0),
    INDITE                                            ("Indite"                                          , "indite"                                          , ItemType.COMMODITY       , "Minerals"               , 0),
    JADEITE                                           ("Jadeite"                                         , "jadeite"                                         , ItemType.COMMODITY       , "Minerals"               , 0),
    LEPIDOLITE                                        ("Lepidolite"                                      , "lepidolite"                                      , ItemType.COMMODITY       , "Minerals"               , 0),
    LITHIUM_HYDROXIDE                                 ("Lithium Hydroxide"                               , "lithiumhydroxide"                                , ItemType.COMMODITY       , "Minerals"               , 0),
    LOW_TEMPERATURE_DIAMONDS                          ("Low Temperature Diamonds"                        , "lowtemperaturediamonds"                          , ItemType.COMMODITY       , "Minerals"               , 0),
    METHANE_CLATHRATE                                 ("Methane Clathrate"                               , "methaneclathrate"                                , ItemType.COMMODITY       , "Minerals"               , 0),
    METHANOL_MONOHYDRATE                              ("Methanol Monohydrate"                            , "methanolmonohydrate"                             , ItemType.COMMODITY       , "Minerals"               , 0),
    MOISSANITE                                        ("Moissanite"                                      , "moissanite"                                      , ItemType.COMMODITY       , "Minerals"               , 0),
    NGADANDARI_FIRE_OPALS                             ("Ngadandari Fire Opals"                           , "ngadandarifireopals"                             , ItemType.COMMODITY       , "Minerals"               , 2),
    PAINITE                                           ("Painite"                                         , "painite"                                         , ItemType.COMMODITY       , "Minerals"               , 0),
    PYROPHYLLITE                                      ("Pyrophyllite"                                    , "pyrophyllite"                                    , ItemType.COMMODITY       , "Minerals"               , 0),
    RUTILE                                            ("Rutile"                                          , "rutile"                                          , ItemType.COMMODITY       , "Minerals"               , 0),
    TAAFFEITE                                         ("Taaffeite"                                       , "taaffeite"                                       , ItemType.COMMODITY       , "Minerals"               , 0),
    URANINITE                                         ("Uraninite"                                       , "uraninite"                                       , ItemType.COMMODITY       , "Minerals"               , 0),
    AI_RELICS                                         ("Ai Relics"                                       , "airelics"                                        , ItemType.COMMODITY       , "Salvage"                , 0),
    ANCIENT_ARTEFACT                                  ("Ancient Artefact"                                , "ancientartefact"                                 , ItemType.COMMODITY       , "Salvage"                , 0),
    ANTIMATTER_CONTAINMENT_UNIT                       ("Antimatter Containment Unit"                     , "antimattercontainmentunit"                       , ItemType.COMMODITY       , "Salvage"                , 0),
    ANTIQUITIES                                       ("Antiquities"                                     , "antiquities"                                     , ItemType.COMMODITY       , "Salvage"                , 0),
    ASSAULT_PLANS                                     ("Assault Plans"                                   , "assaultplans"                                    , ItemType.COMMODITY       , "Salvage"                , 0),
    BLACK_BOX                                         ("Black Box"                                       , "blackbox"                                        , ItemType.COMMODITY       , "Salvage"                , 0),
    COMMERCIAL_SAMPLES                                ("Commercial Samples"                              , "commercialsamples"                               , ItemType.COMMODITY       , "Salvage"                , 0),
    DATA_CORE                                         ("Data Core"                                       , "datacore"                                        , ItemType.COMMODITY       , "Salvage"                , 0),
    DIPLOMATIC_BAG                                    ("Diplomatic Bag"                                  , "diplomaticbag"                                   , ItemType.COMMODITY       , "Salvage"                , 0),
    ENCRYPTED_CORRESPONDENCE                          ("Encrypted Correspondence"                        , "encryptedcorrespondence"                         , ItemType.COMMODITY       , "Salvage"                , 0),
    ENCRYPTED_DATA_STORAGE                            ("Encrypted Data Storage"                          , "encrypteddatastorage"                            , ItemType.COMMODITY       , "Salvage"                , 0),
    EXPERIMENTAL_CHEMICALS                            ("Experimental Chemicals"                          , "experimentalchemicals"                           , ItemType.COMMODITY       , "Salvage"                , 0),
    FOSSIL_REMNANTS                                   ("Fossil Remnants"                                 , "fossilremnants"                                  , ItemType.COMMODITY       , "Salvage"                , 0),
    GALACTIC_TRAVEL_GUIDE                             ("Galactic Travel Guide"                           , "galactictravelguide"                             , ItemType.COMMODITY       , "Salvage"                , 0),
    GEOLOGICAL_SAMPLES                                ("Geological Samples"                              , "geologicalsamples"                               , ItemType.COMMODITY       , "Salvage"                , 0),
    HOSTAGE                                           ("Hostage"                                         , "hostage"                                         , ItemType.COMMODITY       , "Salvage"                , 0),
    MILITARY_INTELLIGENCE                             ("Military Intelligence"                           , "militaryintelligence"                            , ItemType.COMMODITY       , "Salvage"                , 0),
    MILITARY_PLANS                                    ("Military Plans"                                  , "militaryplans"                                   , ItemType.COMMODITY       , "Salvage"                , 0),
    MYSTERIOUS_IDOL                                   ("Mysterious Idol"                                 , "mysteriousidol"                                  , ItemType.COMMODITY       , "Salvage"                , 0),
    OCCUPIED_CRYOPOD                                  ("Occupied CryoPod"                                , "occupiedcryopod"                                 , ItemType.COMMODITY       , "Salvage"                , 0),
    OCCUPIED_ESCAPE_POD                               ("Occupied Escape Pod"                             , "occupiedescapepod"                               , ItemType.COMMODITY       , "Salvage"                , 0),
    PERSONAL_EFFECTS                                  ("Personal Effects"                                , "personaleffects"                                 , ItemType.COMMODITY       , "Salvage"                , 0),
    POLITICAL_PRISONER                                ("Political Prisoner"                              , "politicalprisoner"                               , ItemType.COMMODITY       , "Salvage"                , 0),
    PRECIOUS_GEMS                                     ("Precious Gems"                                   , "preciousgems"                                    , ItemType.COMMODITY       , "Salvage"                , 0),
    PROHIBITED_RESEARCH_MATERIALS                     ("Prohibited Research Materials"                   , "prohibitedresearchmaterials"                     , ItemType.COMMODITY       , "Salvage"                , 0),
    PROTOTYPE_TECH                                    ("Prototype Tech"                                  , "prototypetech"                                   , ItemType.COMMODITY       , "Salvage"                , 0),
    RARE_ARTWORK                                      ("Rare Artwork"                                    , "rareartwork"                                     , ItemType.COMMODITY       , "Salvage"                , 0),
    REBEL_TRANSMISSIONS                               ("Rebel Transmissions"                             , "rebeltransmissions"                              , ItemType.COMMODITY       , "Salvage"                , 0),
    SALVAGEABLE_WRECKAGE                              ("Salvageable Wreckage"                            , "salvageablewreckage"                             , ItemType.COMMODITY       , "Salvage"                , 0),
    SAP_8_CORE_CONTAINER                              ("Sap 8 Core Container"                            , "sap8corecontainer"                               , ItemType.COMMODITY       , "Salvage"                , 0),
    SCIENTIFIC_RESEARCH                               ("Scientific Research"                             , "scientificresearch"                              , ItemType.COMMODITY       , "Salvage"                , 0),
    SCIENTIFIC_SAMPLES                                ("Scientific Samples"                              , "scientificsamples"                               , ItemType.COMMODITY       , "Salvage"                , 0),
    SPACE_PIONEER_RELICS                              ("Space Pioneer Relics"                            , "spacepioneerrelics"                              , ItemType.COMMODITY       , "Salvage"                , 0),
    TACTICAL_DATA                                     ("Tactical Data"                                   , "tacticaldata"                                    , ItemType.COMMODITY       , "Salvage"                , 0),
    TECHNICAL_BLUEPRINTS                              ("Technical Blueprints"                            , "technicalblueprints"                             , ItemType.COMMODITY       , "Salvage"                , 0),
    TRADE_DATA                                        ("Trade Data"                                      , "tradedata"                                       , ItemType.COMMODITY       , "Salvage"                , 0),
    TRINKETS_OF_HIDDEN_FORTUNE                        ("Trinkets Of Hidden Fortune"                      , "trinketsofhiddenfortune"                         , ItemType.COMMODITY       , "Salvage"                , 0),
    UNKNOWN_ARTEFACT                                  ("Unknown Artefact"                                , "unknownartefact"                                 , ItemType.COMMODITY       , "Salvage"                , 0),
    UNKNOWN_PROBE                                     ("Unknown Probe"                                   , "unknownprobe"                                    , ItemType.COMMODITY       , "Salvage"                , 0),
    UNSTABLE_DATA_CORE                                ("Unstable Data Core"                              , "unstabledatacore"                                , ItemType.COMMODITY       , "Salvage"                , 0),
    IMPERIAL_SLAVES                                   ("Imperial Slaves"                                 , "imperialslaves"                                  , ItemType.COMMODITY       , "Slavery"                , 0),
    MASTER_CHEFS                                      ("Master Chefs"                                    , "masterchefs"                                     , ItemType.COMMODITY       , "Slavery"                , 2),
    SLAVES                                            ("Slaves"                                          , "slaves"                                          , ItemType.COMMODITY       , "Slavery"                , 0),
    ADVANCED_CATALYSERS                               ("Advanced Catalysers"                             , "advancedcatalysers"                              , ItemType.COMMODITY       , "Technology"             , 0),
    ANIMAL_MONITORS                                   ("Animal Monitors"                                 , "animalmonitors"                                  , ItemType.COMMODITY       , "Technology"             , 0),
    AQUAPONIC_SYSTEMS                                 ("Aquaponic Systems"                               , "aquaponicsystems"                                , ItemType.COMMODITY       , "Technology"             , 0),
    AUTOFABRICATORS                                   ("Auto-Fabricators"                                , "autofabricators"                                 , ItemType.COMMODITY       , "Technology"             , 0),
    AZ_CANCRI_FORMULA_42                              ("Az Cancri Formula 42"                            , "azcancriformula42"                               , ItemType.COMMODITY       , "Technology"             , 2),
    BIOREDUCING_LICHEN                                ("Bioreducing Lichen"                              , "bioreducinglichen"                               , ItemType.COMMODITY       , "Technology"             , 0),
    COMPUTER_COMPONENTS                               ("Computer Components"                             , "computercomponents"                              , ItemType.COMMODITY       , "Technology"             , 0),
    HE_SUITS                                          ("H.E. Suits"                                      , "hesuits"                                         , ItemType.COMMODITY       , "Technology"             , 0),
    HARDWARE_DIAGNOSTIC_SENSOR                        ("Hardware Diagnostic Sensor"                      , "hardwarediagnosticsensor"                        , ItemType.COMMODITY       , "Technology"             , 1),
    LAND_ENRICHMENT_SYSTEMS                           ("Land Enrichment Systems"                         , "landenrichmentsystems"                           , ItemType.COMMODITY       , "Technology"             , 0),
    MEDICAL_DIAGNOSTIC_EQUIPMENT                      ("Medical Diagnostic Equipment"                    , "medicaldiagnosticequipment"                      , ItemType.COMMODITY       , "Technology"             , 0),
    MICRO_CONTROLLERS                                 ("Micro Controllers"                               , "microcontrollers"                                , ItemType.COMMODITY       , "Technology"             , 1),
    MUON_IMAGER                                       ("Muon Imager"                                     , "muonimager"                                      , ItemType.COMMODITY       , "Technology"             , 0),
    NANOBREAKERS                                      ("Nanobreakers"                                    , "nanobreakers"                                    , ItemType.COMMODITY       , "Technology"             , 1),
    RESONATING_SEPARATORS                             ("Resonating Separators"                           , "resonatingseparators"                            , ItemType.COMMODITY       , "Technology"             , 0),
    ROBOTICS                                          ("Robotics"                                        , "robotics"                                        , ItemType.COMMODITY       , "Technology"             , 0),
    STRUCTURAL_REGULATORS                             ("Structural Regulators"                           , "structuralregulators"                            , ItemType.COMMODITY       , "Technology"             , 0),
    TELEMETRY_SUITE                                   ("Telemetry Suite"                                 , "telemetrysuite"                                  , ItemType.COMMODITY       , "Technology"             , 1),
    XIHE_BIOMORPHIC_COMPANIONS                        ("Xihe Biomorphic Companions"                      , "xihebiomorphiccompanions"                        , ItemType.COMMODITY       , "Technology"             , 2),
    BANKI_AMPHIBIOUS_LEATHER                          ("Banki Amphibious Leather"                        , "bankiamphibiousleather"                          , ItemType.COMMODITY       , "Textiles"               , 2),
    BELALANS_RAY_LEATHER                              ("Belalans Ray Leather"                            , "belalansrayleather"                              , ItemType.COMMODITY       , "Textiles"               , 2),
    CHAMELEON_CLOTH                                   ("Chameleon Cloth"                                 , "chameleoncloth"                                  , ItemType.COMMODITY       , "Textiles"               , 2),
    CONDUCTIVE_FABRICS                                ("Conductive Fabrics"                              , "conductivefabrics"                               , ItemType.COMMODITY       , "Textiles"               , 0),
    DAMNA_CARAPACES                                   ("Damna Carapaces"                                 , "damnacarapaces"                                  , ItemType.COMMODITY       , "Textiles"               , 2),
    LEATHER                                           ("Leather"                                         , "leather"                                         , ItemType.COMMODITY       , "Textiles"               , 0),
    MILITARY_GRADE_FABRICS                            ("Military Grade Fabrics"                          , "militarygradefabrics"                            , ItemType.COMMODITY       , "Textiles"               , 0),
    NATURAL_FABRICS                                   ("Natural Fabrics"                                 , "naturalfabrics"                                  , ItemType.COMMODITY       , "Textiles"               , 0),
    RAPA_BAO_SNAKE_SKINS                              ("Rapa Bao Snake Skins"                            , "rapabaosnakeskins"                               , ItemType.COMMODITY       , "Textiles"               , 2),
    SYNTHETIC_FABRICS                                 ("Synthetic Fabrics"                               , "syntheticfabrics"                                , ItemType.COMMODITY       , "Textiles"               , 0),
    TIEGFRIES_SYNTH_SILK                              ("Tiegfries Synth Silk"                            , "tiegfriessynthsilk"                              , ItemType.COMMODITY       , "Textiles"               , 2),
    VANAYEQUI_CERATOMORPHA_FUR                        ("Vanayequi Ceratomorpha Fur"                      , "vanayequiceratomorphafur"                        , ItemType.COMMODITY       , "Textiles"               , 2),
    LIMPET                                            ("Limpet"                                          , "limpet"                                          , ItemType.COMMODITY       , "Unknown"                , 0),
    BIOWASTE                                          ("Biowaste"                                        , "biowaste"                                        , ItemType.COMMODITY       , "Waste"                  , 0),
    CHEMICAL_WASTE                                    ("Chemical Waste"                                  , "chemicalwaste"                                   , ItemType.COMMODITY       , "Waste"                  , 0),
    SCRAP                                             ("Scrap"                                           , "scrap"                                           , ItemType.COMMODITY       , "Waste"                  , 0),
    TOXIC_WASTE                                       ("Toxic Waste"                                     , "toxicwaste"                                      , ItemType.COMMODITY       , "Waste"                  , 0),
    BATTLE_WEAPONS                                    ("Battle Weapons"                                  , "battleweapons"                                   , ItemType.COMMODITY       , "Weapons"                , 0),
    BORASETANI_PATHOGENETICS                          ("Borasetani Pathogenetics"                        , "borasetanipathogenetics"                         , ItemType.COMMODITY       , "Weapons"                , 2),
    GILYA_SIGNATURE_WEAPONS                           ("Gilya Signature Weapons"                         , "gilyasignatureweapons"                           , ItemType.COMMODITY       , "Weapons"                , 2),
    HIP_118311_SWARM                                  ("HIP 118311 Swarm"                                , "hip118311swarm"                                  , ItemType.COMMODITY       , "Weapons"                , 2),
    HOLVA_DUELLING_BLADES                             ("Holva Duelling Blades"                           , "holvaduellingblades"                             , ItemType.COMMODITY       , "Weapons"                , 2),
    KAMORIN_HISTORIC_WEAPONS                          ("Kamorin Historic Weapons"                        , "kamorinhistoricweapons"                          , ItemType.COMMODITY       , "Weapons"                , 2),
    LANDMINES                                         ("Landmines"                                       , "landmines"                                       , ItemType.COMMODITY       , "Weapons"                , 0),
    NONLETHAL_WEAPONS                                 ("Non-lethal Weapons"                              , "nonlethalweapons"                                , ItemType.COMMODITY       , "Weapons"                , 0),
    PERSONAL_WEAPONS                                  ("Personal Weapons"                                , "personalweapons"                                 , ItemType.COMMODITY       , "Weapons"                , 0),
    REACTIVE_ARMOUR                                   ("Reactive Armour"                                 , "reactivearmour"                                  , ItemType.COMMODITY       , "Weapons"                , 0),

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
    private final String categoryName;
    private final int grade; // -2=very common, -1=common, 0=standard, +1=rare, +2=very rare --OR-- 0=normal commodity, 1=engineer commodity, 2=rare commodity

    private Item(String name, String journalName, ItemType type, int grade) {
        this.name = name;
        this.journalName = journalName;
        this.type = type;
        this.categoryName = null;
        this.grade = grade;
    }

    private Item(String name, String journalName, ItemType type, String categoryName, int grade) {
        this.name = name;
        this.journalName = journalName;
        this.type = type;
        this.categoryName = categoryName;
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

    public String getCategoryName() {
        return this.categoryName;
    }

    public static enum ItemType {
        ELEMENT, MANUFACTURED, DATA, COMMODITY, DRONES;
    }

}
