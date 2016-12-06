package borg.edtrading;

import borg.edtrading.imagetransformation.simple.RgbToGrayF32Transformation;
import borg.edtrading.ocr.CharacterLocator;
import borg.edtrading.ocr.OcrExecutor;
import borg.edtrading.ocr.OcrResult;
import borg.edtrading.ocr.OcrTask;
import borg.edtrading.ocr.TextLine;
import borg.edtrading.screenshots.Region;
import borg.edtrading.screenshots.Screenshot;
import borg.edtrading.templatematching.Match;
import borg.edtrading.templatematching.Template;
import borg.edtrading.templatematching.TemplateMatcher;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * FactionScannerApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FactionScannerApp {

    static final Logger logger = LogManager.getLogger(FactionScannerApp.class);

    static SortedSet<String> unknownFactions = new TreeSet<>();

    public static void main(String[] args) throws IOException {
        List<File> screenshotFiles = FactionScannerApp.selectAllScreenshots();

        CharacterLocator characterLocator = new CharacterLocator(2, 40, 16, 40, 1); // min 2x16, max 40x40, 1px border
        List<Template> templates = Template.fromFolder("BodyScanner");
        List<Template> systemIdentifierTemplates = Template.fromFolder("SystemIdentifiers");

        SortedMap<String, SystemFactions> systemsByName = new TreeMap<>();
        for (File screenshotFile : screenshotFiles) {
            String systemName = guessSystemName(screenshotFile, systemIdentifierTemplates).toUpperCase();
            logger.debug("I guess " + screenshotFile.getName() + " is " + systemName);
            SystemFactions systemFactions = systemsByName.get(systemName);
            if (systemFactions == null) {
                systemFactions = new SystemFactions(systemName);
                systemsByName.put(systemName, systemFactions);
            }

            Screenshot screenshot = Screenshot.loadFromFile(screenshotFile, 3840, 2160, null);
            Region region = screenshot.getAsRegion();
            //x=0,y=350,w=840,h=1620

            OcrTask ocrTask = new OcrTask(region, characterLocator, templates);
            ocrTask.setDebugAllTextLines(true);
            OcrResult ocrResult = new OcrExecutor().executeOcr(ocrTask);
            ocrResult.writeDebugImages();
            updateSystemFactions(systemFactions, ocrResult);
        }

        for (SystemFactions systemFactions : systemsByName.values()) {
            System.out.println("==== " + systemFactions.getSystemName() + " ====");
            System.out.println("---- " + systemFactions.getControllingFaction() + " ----");
            for (KnownFaction faction : systemFactions.getFactions().keySet()) {
                System.out.println("* " + faction);
                SystemFaction systemFaction = systemFactions.getFactions().get(faction);
                System.out.println("Government:    " + systemFaction.getGovernment());
                System.out.println("Allegiance:    " + systemFaction.getAllegiance());
                System.out.println("Influence:     " + systemFaction.getInfluence());
                System.out.println("State:         " + systemFaction.getState());
                System.out.println("Relationship:  " + systemFaction.getRelationship());
            }
        }

        for (String unknownFaction : unknownFactions) {
            System.out.println(unknownFaction);
        }
    }

    private static void updateSystemFactions(SystemFactions systemFactions, OcrResult ocrResult) {
        KnownFaction currentFaction = null;
        KnownLabel currentLabel = null;
        String currentValue = null;
        for (TextLine tl : ocrResult.getTextLines()) {
            String text = tl.toText().replace("→", " ").trim();
            if (text.contains(":")) {
                // New label starts here. Current value is finished.
                if (currentValue != null) {
                    if (currentLabel == KnownLabel.CONTROLLING_FACTION) {
                        systemFactions.setControllingFaction(KnownFaction.findBestMatching(currentValue));
                    } else if (currentLabel == KnownLabel.FACTION) {
                        currentFaction = KnownFaction.findBestMatching(currentValue);
                        if (currentFaction == null) {
                            unknownFactions.add(currentValue);
                            logger.warn(currentValue);
                        }
                    } else if (currentLabel != null && currentFaction != null) {
                        SystemFaction systemFaction = systemFactions.getFactions().get(currentFaction);
                        if (systemFaction == null) {
                            systemFaction = new SystemFaction(currentFaction);
                            systemFactions.getFactions().put(currentFaction, systemFaction);
                        }
                        if (currentLabel == KnownLabel.GOVERNMENT) {
                            systemFaction.setGovernment(Government.findBestMatching(currentValue));
                        } else if (currentLabel == KnownLabel.ALLEGIANCE) {
                            systemFaction.setAllegiance(Allegiance.findBestMatching(currentValue));
                        } else if (currentLabel == KnownLabel.INFLUENCE) {
                            String fixedValue = currentValue.toUpperCase().replace(" ", "").replace("%", "").replace(",", ".");
                            fixedValue = fixedValue.replace("O", "0").replace("D", "0").replace("I", "1").replace("S", "5").replace("B", "8");
                            systemFaction.setInfluence(new BigDecimal(fixedValue.trim()));
                        } else if (currentLabel == KnownLabel.STATE) {
                            systemFaction.setState(State.findBestMatching(currentValue));
                        } else if (currentLabel == KnownLabel.RELATIONSHIP) {
                            systemFaction.setRelationship(Relationship.findBestMatching(currentValue));
                        }
                    }
                }
                currentLabel = KnownLabel.findBestMatching(text.substring(0, text.indexOf(":")).trim());
                currentValue = null;
                text = text.substring(text.indexOf(":") + 1).trim();
            }
            if (currentValue == null) {
                currentValue = text;
            } else {
                currentValue = currentValue + " " + text;
            }
        }
    }

    private static String guessSystemName(File screenshotFile, List<Template> systemIdentifierTemplates) throws IOException {
        Screenshot screenshot = Screenshot.loadFromFile(screenshotFile, 384, 216, null);
        Region region = screenshot.getAsRegion();
        region.applyTransformation("F32", new RgbToGrayF32Transformation());

        String best = null;
        float bestErr = Float.MAX_VALUE;
        //        LinkedHashMap<String, Float> guesses = new LinkedHashMap<>();
        for (Template template : systemIdentifierTemplates) {
            Match bestMatchingLocation = new TemplateMatcher().bestMatchingLocation(region, template);
            if (bestMatchingLocation != null) {
                float err = bestMatchingLocation.getErrorPerPixel();
                //                guesses.put(bestMatchingLocation.getTemplate().getText(), err);
                if (err < bestErr) {
                    bestErr = err;
                    best = bestMatchingLocation.getTemplate().getText();
                }
            }
        }
        //        MiscUtil.sortMapByValue(guesses);
        //        for (String guess : guesses.keySet()) {
        //            System.out.println(String.format(Locale.US, "%6.4f err/pixel: %s", guesses.get(guess), guess));
        //        }

        return best;
    }

    static List<File> selectSpecificScreenshot(String filename) {
        return Arrays.asList(new File(Constants.FACTION_SCREENSHOTS_DIR, filename));
    }

    static List<File> selectRandomScreenshot() {
        return selectAllScreenshots().subList(0, 1);
    }

    static List<File> selectAllScreenshots() {
        File[] fileArray = Constants.FACTION_SCREENSHOTS_DIR.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().endsWith(".png");
            }
        });
        List<File> fileList = new ArrayList<>(fileArray.length);
        for (File f : fileArray) {
            fileList.add(f);
        }
        Collections.shuffle(fileList);
        return fileList;
    }

    public static class SystemFactions {

        private String systemName = null;
        private KnownFaction controllingFaction = null;
        private SortedMap<KnownFaction, SystemFaction> factions = null;

        public SystemFactions(String systemName) {
            this.setSystemName(systemName);
            this.setFactions(new TreeMap<>());
        }

        public String getSystemName() {
            return this.systemName;
        }

        public void setSystemName(String systemName) {
            this.systemName = systemName;
        }

        public KnownFaction getControllingFaction() {
            return this.controllingFaction;
        }

        public void setControllingFaction(KnownFaction controllingFaction) {
            this.controllingFaction = controllingFaction;
        }

        public SortedMap<KnownFaction, SystemFaction> getFactions() {
            return this.factions;
        }

        public void setFactions(SortedMap<KnownFaction, SystemFaction> factions) {
            this.factions = factions;
        }

    }

    public static class SystemFaction {

        private KnownFaction faction = null;
        private Government government = null;
        private Allegiance allegiance = null;
        private BigDecimal influence = null;
        private State state = null;
        private Relationship relationship = null;

        public SystemFaction(KnownFaction faction) {
            this.setFaction(faction);
        }

        public KnownFaction getFaction() {
            return this.faction;
        }

        public void setFaction(KnownFaction faction) {
            this.faction = faction;
        }

        public Government getGovernment() {
            return this.government;
        }

        public void setGovernment(Government government) {
            this.government = government;
        }

        public Allegiance getAllegiance() {
            return this.allegiance;
        }

        public void setAllegiance(Allegiance allegiance) {
            this.allegiance = allegiance;
        }

        public BigDecimal getInfluence() {
            return this.influence;
        }

        public void setInfluence(BigDecimal influence) {
            this.influence = influence;
        }

        public State getState() {
            return this.state;
        }

        public void setState(State state) {
            this.state = state;
        }

        public Relationship getRelationship() {
            return this.relationship;
        }

        public void setRelationship(Relationship relationship) {
            this.relationship = relationship;
        }

    }

    public static enum Government {

        //@formatter:off
        ANARCHY("ANARCHY"),
        COMMUNISM("COMMUNISM"),
        CONFEDERACY("CONFEDERACY"),
        CORPORATE("CORPORATE"),
        COOPERATIVE("COOPERATIVE"),
        DEMOCRACY("DEMOCRACY"),
        DICTATORSHIP("DICTATORSHIP"),
        FEUDAL("FEUDAL"),
        IMPERIAL("IMPERIAL"),
        PATRONAGE("PATRONAGE"),
        PRISON_COLONY("PRISON COLONY"),
        THEOCRACY("THEOCRACY"),
        WORKSHOP("WORKSHOP"),
        NONE("NONE");
        //@formatter:on

        private final String name;

        private Government(String name) {
            this.name = name;
        }

        public static Government findBestMatching(String name) {
            Government best = null;
            float bestError = Float.MAX_VALUE;
            String fixedName = name.toUpperCase().replace("0", "O").replace("1", "I").replace("5", "S").replace("8", "B");
            for (Government e : Government.values()) {
                float error = MiscUtil.levenshteinError(e.getName(), fixedName);
                if (error <= 0.25f && error < bestError) {
                    best = e;
                    bestError = error;
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

    }

    public static enum Allegiance {

        //@formatter:off
        ALLIANCE("ALLIANCE"),
        EMPIRE("EMPIRE"),
        FEDERATION("FEDERATION"),
        INDEPENDENT("INDEPENDENT"),
        NONE("NONE");
        //@formatter:on

        private final String name;

        private Allegiance(String name) {
            this.name = name;
        }

        public static Allegiance findBestMatching(String name) {
            Allegiance best = null;
            float bestError = Float.MAX_VALUE;
            String fixedName = name.toUpperCase().replace("0", "O").replace("1", "I").replace("5", "S").replace("8", "B");
            for (Allegiance e : Allegiance.values()) {
                float error = MiscUtil.levenshteinError(e.getName(), fixedName);
                if (error <= 0.25f && error < bestError) {
                    best = e;
                    bestError = error;
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

    }

    public static enum State {

        //@formatter:off
        BOOM("BOOM"),
        BUST("BUST"),
        FAMINE("FAMINE"),
        CIVIL_UNREST("CIVIL UNREST"),
        CIVIL_WAR("CIVIL WAR"),
        ELECTION("ELECTION"),
        EXPANSION("EXPANSION"),
        LOCKDOWN("LOCKDOWN"),
        OUTBREAK("OUTBREAK"),
        WAR("WAR"),
        NONE("NONE"),
        RETREAT("RETREAT"),
        INVESTMENT("INVESTMENT");
        //@formatter:on

        private final String name;

        private State(String name) {
            this.name = name;
        }

        public static State findBestMatching(String name) {
            State best = null;
            float bestError = Float.MAX_VALUE;
            String fixedName = name.toUpperCase().replace("0", "O").replace("1", "I").replace("5", "S").replace("8", "B");
            for (State e : State.values()) {
                float error = MiscUtil.levenshteinError(e.getName(), fixedName);
                if (error <= 0.25f && error < bestError) {
                    best = e;
                    bestError = error;
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

    }

    public static enum Relationship {

        //@formatter:off
        HOSTILE("HOSTILE"),
        UNFRIENDLY("UNFRIENDLY"),
        NEUTRAL("NEUTRAL"),
        CORDIAL("CORDIAL"),
        FRIENDLY("FRIENDLY"),
        ALLIED("ALLIED");
        //@formatter:on

        private final String name;

        private Relationship(String name) {
            this.name = name;
        }

        public static Relationship findBestMatching(String name) {
            Relationship best = null;
            float bestError = Float.MAX_VALUE;
            String fixedName = name.toUpperCase().replace("0", "O").replace("1", "I").replace("5", "S").replace("8", "B");
            for (Relationship e : Relationship.values()) {
                float error = MiscUtil.levenshteinError(e.getName(), fixedName);
                if (error <= 0.25f && error < bestError) {
                    best = e;
                    bestError = error;
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

    }

    public static enum KnownLabel {

        //@formatter:off
        CONTROLLING_FACTION("CONTROLLING FACTION"),
        FACTION("FACTION"),
        GOVERNMENT("GOVERNMENT"),
        ALLEGIANCE("ALLEGIANCE"),
        INFLUENCE("INFLUENCE"),
        STATE("STATE"),
        RELATIONSHIP("RELATIONSHIP");
        //@formatter:on

        private final String name;

        private KnownLabel(String name) {
            this.name = name;
        }

        public static KnownLabel findBestMatching(String name) {
            KnownLabel best = null;
            float bestError = Float.MAX_VALUE;
            String fixedName = name.toUpperCase().replace("0", "O").replace("1", "I").replace("5", "S").replace("8", "B");
            for (KnownLabel e : KnownLabel.values()) {
                float error = MiscUtil.levenshteinError(e.getName(), fixedName);
                if (error <= 0.25f && error < bestError) {
                    best = e;
                    bestError = error;
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

    }

    public static enum KnownFaction {

        //@formatter:off
        GERMAN_PILOT_LOUNGE("GERMAN PILOT LOUNGE"),

        ALLIANCE_OF_STHA_181("ALLIANCE OF STHA 181"),
        ALLIANCE_OF_HRISASTSHI("ALLIANCE OF HRISASTSHI"),
        BAVARINGONI_BLUE_RATS("BAVARINGONI BLUE RATS"),
        BD_10_5238_PUBLIC_INC("BD-10 5238 PUBLIC INC"),
        CLAN_OF_MARIDAL("CLAN OF MARIDAL"),
        HRISASTSHI_CO("HRISASTSHI CO"),
        HRISASTSHI_EMPIRE_CONSULATE("HRISASTSHI EMPIRE CONSULATE"),
        HRISASTSHI_JET_BOYS("HRISASTSHI JET BOYS"),
        HRISASTSHI_PURPLE_LEGAL_PARTNERS("HRISASTSHI PURPLE LEGAL PARTNERS"),
        INDEPENDENTS_OF_MARIDAL("INDEPENDENTS OF MARIDAL"),
        JEN_ELABOG_FUTURE("JEN ELABOG FUTURE"),
        JUSTICE_PARTY_OF_MARIDAL("JUSTICE PARTY OF MARIDAL"),
        LAW_PARTY_OF_LTT_15899("LAW PARTY OF LTT 15899"),
        LFT_1504_LIMITED("LFT 1504 LIMITED"),
        LHS_3598_TRANSPORT_GROUP("LHS 3598 TRANSPORT GROUP"),
        LIBERALS_OF_LP_575_38("LIBERALS OF LP 575-38"),
        LP_575_38_BLUE_TRANSPORT_COMMS("LP 575-38 BLUE TRANSPORT COMMS"),
        LP_575_38_ORGANISATION("LP 575-38 ORGANISATION"),
        MARIDAL_COMMODITIES("MARIDAL COMMODITIES"),
        NEW_LP_635_46_CONFEDERATION("NEW LP 635-46 CONFEDERATION"),
        NEZ_PELLIRI_DOMINION("NEZ PELLIRI DOMINION"),
        NEZ_PELLIRI_GANG("NEZ PELLIRI GANG"),
        NEZ_PELLIRI_SILVER_GALACTIC("NEZ PELLIRI SILVER GALACTIC"),
        NGARU_CRIMSON_COUNCIL("NGARU CRIMSON COUNCIL"),
        NGARU_SERVICES("NGARU SERVICES"),
        NOEGIN_PURPLE_BOYS("NOEGIN PURPLE BOYS"),
        PARTNERSHIP_OF_NGARU("PARTNERSHIP OF NGARU"),
        ROSS_754_LABOUR("ROSS 754 LABOUR"),
        SUN_TAKUSH_POWER_PLC("SUN TAKUSH POWER PLC"),
        UNITED_LP_575_38_MOVEMENT("UNITED LP 575-38 MOVEMENT"),
        UNITING_NOEGIN("UNITING NOEGIN"),
        UZUMERU_NETCOMS_INCORPORATED("UZUMERU NETCOMS INCORPORATED"),
        V1703_AQUILAE_NATURAL_LIMITED("V1703 AQUILAE NATURAL LIMITED");
        //@formatter:on

        private final String name;

        private KnownFaction(String name) {
            this.name = name;
        }

        public static KnownFaction findBestMatching(String name) {
            KnownFaction best = null;
            float bestError = Float.MAX_VALUE;
            String fixedName = name.toUpperCase().replace("0", "O").replace("1", "I").replace("5", "S").replace("8", "B");
            for (KnownFaction e : KnownFaction.values()) {
                float error = MiscUtil.levenshteinError(e.getName(), fixedName);
                if (error <= 0.25f && error < bestError) {
                    best = e;
                    bestError = error;
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

    }

}
