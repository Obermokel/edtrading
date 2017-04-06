package borg.edtrading;

import borg.edtrading.cfg.Constants;
import borg.edtrading.google.GoogleSpreadsheet;
import borg.edtrading.google.GoogleTable;
import borg.edtrading.ocr.CharacterLocator;
import borg.edtrading.ocr.OcrExecutor;
import borg.edtrading.ocr.OcrResult;
import borg.edtrading.ocr.OcrTask;
import borg.edtrading.ocr.TextLine;
import borg.edtrading.ocr.screenshots.Region;
import borg.edtrading.ocr.screenshots.Screenshot;
import borg.edtrading.ocr.templatematching.Template;
import borg.edtrading.util.MiscUtil;
import org.apache.commons.io.FileExistsException;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;

/**
 * FactionScannerApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class FactionScannerApp {

    static final Logger logger = LogManager.getLogger(FactionScannerApp.class);

    //    private static final boolean SKIP_GOOGLE_UPDATE = false;

    private static final String spreadsheetId = "1z5USvjTp_htXdsd2o3qrm6DUgL7tlGmHoB8Xh51Fms0";

    private static CharacterLocator characterLocator = null;
    private static List<Template> templates = null;

    public static void main(String[] args) throws IOException {

        while (!Thread.interrupted()) {
            characterLocator = new CharacterLocator(2, 40, 16, 40, 1); // min 2x16, max 40x40, 1px border
            templates = Template.fromFolder("BodyScanner");

            processUploadedScreenshots();

            // TODO Check all screenshots, remember last date

            try {
                Thread.sleep(10000L);
            } catch (InterruptedException e) {
                break;
            }
        }
    }

    private static void processUploadedScreenshots() {
        File uploadDir = new File(Constants.FACTION_SCREENSHOTS_DIR, "upload");
        File doneDir = new File(Constants.FACTION_SCREENSHOTS_DIR, "done");
        File errorDir = new File(Constants.FACTION_SCREENSHOTS_DIR, "error");

        List<File> screenshotFiles = FactionScannerApp.selectAllScreenshots(uploadDir);
        for (File screenshotFile : screenshotFiles) {
            try {
                SystemFactions systemFactions = processScreenshotFile(screenshotFile);
                logger.info("Successfully scanned " + screenshotFile);
                File destDir = new File(doneDir, systemFactions.getSystemName());
                File destFile = new File(destDir, screenshotFile.getName());
                if (destFile.exists()) {
                    destFile.delete();
                }
                FileUtils.moveFileToDirectory(screenshotFile, destDir, true);
            } catch (Exception e1) {
                if (System.currentTimeMillis() - screenshotFile.lastModified() < 600000L) {
                    logger.warn("Failed to process " + screenshotFile + ": " + e1);
                } else {
                    logger.error("Moving unparseable screenshot to error dir: " + screenshotFile, e1);
                    try {
                        FileUtils.moveFileToDirectory(screenshotFile, errorDir, true);
                        if (e1 instanceof FactionScanException) {
                            FactionScanException fse = (FactionScanException) e1;
                            if (StringUtils.isNotEmpty(fse.getReasonCode()) && fse.getOcrResult() != null && fse.getOcrResult().getAllTextLinesDebugImage() != null) {
                                BufferedImage debugImage = fse.getOcrResult().getAllTextLinesDebugImage();
                                Graphics2D g = debugImage.createGraphics();
                                g.setFont(new Font("Tahoma", Font.BOLD, 32));
                                g.setColor(Color.RED);
                                g.drawString(fse.getReasonMessage(), 1000, 100);
                                g.dispose();
                                ImageIO.write(debugImage, "PNG", new File(errorDir, screenshotFile.getName().replace(".png", " " + fse.getReasonCode() + ".png")));
                            }
                        }
                    } catch (FileExistsException e2) {
                        screenshotFile.delete();
                    } catch (IOException e2) {
                        logger.error("Failed to move " + screenshotFile + " to " + errorDir, e2);
                    }
                }
            }
        }
    }

    private static SystemFactions processScreenshotFile(File screenshotFile) throws IOException, ParseException {
        Screenshot screenshot = Screenshot.loadFromFile(screenshotFile, 3840, 2160, null);
        Region region = screenshot.getAsRegion(); //x=0,y=350,w=840,h=1620

        OcrTask ocrTask = new OcrTask(region, characterLocator, templates);
        //        ocrTask.setDebugAlphanumTemplates(true);
        //        ocrTask.setDebugAlphanumTextLines(true);
        //        ocrTask.setDebugAllTemplates(true);
        ocrTask.setDebugAllTextLines(true);
        OcrResult ocrResult = new OcrExecutor().executeOcr(ocrTask);
        //        ocrResult.writeDebugImages();

        SystemFactions systemFactions = new SystemFactions("FAKE SYSTEM");
        updateSystemFactions(systemFactions, ocrResult);
        updateDateAndSystemFromFilename(systemFactions, screenshotFile);

        String date = new SimpleDateFormat("dd.MM.yyyy").format(systemFactions.getDate());
        String tableNameInfluence = "INFLUENCE_" + systemFactions.getSystemName().toUpperCase().replaceAll("\\W", "_");
        GoogleSpreadsheet gplInfluence = new GoogleSpreadsheet(spreadsheetId, tableNameInfluence);
        GoogleTable tblInfluence = gplInfluence.getTable(tableNameInfluence);
        String tableNameState = "STATE_" + systemFactions.getSystemName().toUpperCase().replaceAll("\\W", "_");
        GoogleSpreadsheet gplState = new GoogleSpreadsheet(spreadsheetId, tableNameState);
        GoogleTable tblState = gplState.getTable(tableNameState);
        if (tblInfluence == null) {
            throw new FactionScanException("TABLE NOT FOUND", "Table '" + tableNameInfluence + "' not found in Google sheet", ocrResult);
            //throw new RuntimeException("Table '" + tableName + "' not found");
        } else {
            for (KnownFaction faction : systemFactions.getFactions().keySet()) {
                if (systemFactions.getFactions().get(faction).getInfluence() != null) {
                    String factionName = faction.getName();
                    String influence = String.format(Locale.GERMANY, "%.1f%%", systemFactions.getFactions().get(faction).getInfluence());

                    int colIdx = tblInfluence.getColumnIndex(factionName);
                    if (colIdx < 0) {
                        logger.info("Adding column '" + factionName + "' to table '" + tableNameInfluence + "'");
                        colIdx = tblInfluence.addColumn(factionName);
                    }
                    int rowIdx = tblInfluence.getRowIndex(date);
                    if (rowIdx < 0) {
                        logger.info("Adding row '" + date + "' to table '" + tableNameInfluence + "'");
                        rowIdx = tblInfluence.addRow(date);
                    }
                    String existingValue = tblInfluence.getCellValue(rowIdx, colIdx);
                    if (StringUtils.isNotEmpty(existingValue) && !existingValue.equals(influence)) {
                        logger.warn("Overwriting " + existingValue + " with " + influence + " for " + factionName + " (" + date + ")");
                    }
                    tblInfluence.setCellValue(rowIdx, colIdx, influence);
                }
                if (systemFactions.getFactions().get(faction).getState() != null) {
                    String factionName = faction.getName();
                    String state = systemFactions.getFactions().get(faction).getState().toString();

                    int colIdx = tblState.getColumnIndex(factionName);
                    if (colIdx < 0) {
                        logger.info("Adding column '" + factionName + "' to table '" + tableNameState + "'");
                        colIdx = tblState.addColumn(factionName);
                    }
                    int rowIdx = tblState.getRowIndex(date);
                    if (rowIdx < 0) {
                        logger.info("Adding row '" + date + "' to table '" + tableNameState + "'");
                        rowIdx = tblState.addRow(date);
                    }
                    String existingValue = tblState.getCellValue(rowIdx, colIdx);
                    if (StringUtils.isNotEmpty(existingValue) && !existingValue.equals(state)) {
                        logger.warn("Overwriting " + existingValue + " with " + state + " for " + factionName + " (" + date + ")");
                    }
                    tblState.setCellValue(rowIdx, colIdx, state);
                }
            }
        }

        return systemFactions;
    }

    private static void updateDateAndSystemFromFilename(SystemFactions systemFactions, File screenshotFile) throws ParseException {
        // Date
        Date date = new Date(screenshotFile.lastModified());
        Pattern p = Pattern.compile("(\\d{4}\\-\\d{2}\\-\\d{2})_.*");
        Matcher m = p.matcher(screenshotFile.getName());
        if (m.matches()) {
            date = new SimpleDateFormat("yyyy-MM-dd").parse(m.group(1));
        }
        systemFactions.setDate(DateUtils.truncate(date, Calendar.DATE));

        // System name
        p = Pattern.compile(".*_\\d+_([^_]+)_\\d+\\.png");
        m = p.matcher(screenshotFile.getName());
        if (m.matches()) {
            systemFactions.setSystemName(m.group(1).toUpperCase());
        }
    }

    private static void updateSystemFactions(SystemFactions systemFactions, OcrResult ocrResult) throws ParseException {
        KnownFaction currentFaction = null;
        KnownLabel currentLabel = null;
        String currentValue = null;
        for (TextLine tl : ocrResult.getTextLines()) {
            if (tl.getxInScreenshot() >= 850) {
                continue; // Too far right, not part of the left info panel
            }
            String text = tl.toText().replace("→", " ").trim();
            if ("BACK".equals(text) || "EXIT".equals(text)) {
                text = text + ": " + text;
            }
            if (text.contains(":")) {
                // New label starts here. Current value is finished.
                if (currentValue != null) {
                    if (currentLabel == KnownLabel.CONTROLLING_FACTION) {
                        systemFactions.setControllingFaction(KnownFaction.findBestMatching(currentValue));
                    } else if (currentLabel == KnownLabel.FACTION) {
                        currentFaction = KnownFaction.findBestMatching(currentValue);
                        if (currentFaction == null && systemFactions.getFactions().size() < 2) {
                            throw new FactionScanException("FACTION UNKNOWN", "Failed to parse '" + currentValue + "' to a faction name", ocrResult);
                            //System.exit(1);
                            //unknownFactions.add(currentValue);
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
                            try {
                                systemFaction.setInfluence(new BigDecimal(fixedValue.trim()));
                            } catch (NumberFormatException e) {
                                throw new FactionScanException("INFLUENCE UNKNOWN", "Failed to parse '" + currentValue + "' (fixed to '" + fixedValue + "') to influence of " + currentFaction, ocrResult);
                                //System.exit(1);
                                //systemFaction.setInfluence(new BigDecimal("99.9"));
                            }
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

    //    static List<File> selectSpecificScreenshot(String filename) {
    //        return Arrays.asList(new File(Constants.FACTION_SCREENSHOTS_DIR, filename));
    //    }

    //    static List<File> selectRandomScreenshot(File uploadDir) {
    //        List<File> all = selectAllScreenshots(uploadDir);
    //        Collections.shuffle(all);
    //        return all.subList(0, 1);
    //    }

    static List<File> selectAllScreenshots(File uploadDir) {
        File[] fileArray = uploadDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().endsWith(".png");
            }
        });
        List<File> fileList = new ArrayList<>(fileArray.length);
        for (File f : fileArray) {
            if (System.currentTimeMillis() - f.lastModified() > 10000L) {
                fileList.add(f);
            }
        }
        Collections.sort(fileList, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                return f1.getName().compareTo(f2.getName());
            }
        });
        return fileList;
    }

    public static class SystemFactions {

        private String systemName = null;
        private Date date = null;
        private KnownFaction controllingFaction = null;
        private SortedMap<KnownFaction, SystemFaction> factions = null;

        public SystemFactions(String systemName) {
            this.setSystemName(systemName);
            this.setFactions(new TreeMap<>());
        }

        public void mergeWith(SystemFactions other) {
            if (other != null) {
                if (other.getControllingFaction() != null && this.getControllingFaction() == null) {
                    this.setControllingFaction(other.getControllingFaction());
                }
                for (KnownFaction faction : other.getFactions().keySet()) {
                    SystemFaction otherFaction = other.getFactions().get(faction);
                    SystemFaction thisFaction = this.getFactions().get(faction);
                    if (thisFaction == null) {
                        this.getFactions().put(faction, otherFaction);
                    } else {
                        if (thisFaction.getGovernment() == null && otherFaction.getGovernment() != null) {
                            thisFaction.setGovernment(otherFaction.getGovernment());
                        }
                        if (thisFaction.getAllegiance() == null && otherFaction.getAllegiance() != null) {
                            thisFaction.setAllegiance(otherFaction.getAllegiance());
                        }
                        if (thisFaction.getInfluence() == null && otherFaction.getInfluence() != null) {
                            thisFaction.setInfluence(otherFaction.getInfluence());
                        }
                        if (thisFaction.getState() == null && otherFaction.getState() != null) {
                            thisFaction.setState(otherFaction.getState());
                        }
                        if (thisFaction.getRelationship() == null && otherFaction.getRelationship() != null) {
                            thisFaction.setRelationship(otherFaction.getRelationship());
                        }
                    }
                }
            }
        }

        public String getSystemName() {
            return this.systemName;
        }

        public void setSystemName(String systemName) {
            this.systemName = systemName;
        }

        public Date getDate() {
            return this.date;
        }

        public void setDate(Date date) {
            this.date = date;
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
        ANARCHY("ANARCHY", "ANARCHIE"),
        COMMUNISM("COMMUNISM", "KOMMUNISMUS"),
        CONFEDERACY("CONFEDERACY", "KONFÖDERATION"),
        CORPORATE("CORPORATE", "KONZERNPOLITIK"),
        COOPERATIVE("COOPERATIVE", "KOOPERATIVE"),
        DEMOCRACY("DEMOCRACY", "DEMOKRATIE"),
        DICTATORSHIP("DICTATORSHIP", "DIKTATUR"),
        FEUDAL("FEUDAL", "FEUDALSYSTEM"),
        IMPERIAL("IMPERIAL", "IMPERIUM"),
        PATRONAGE("PATRONAGE", "PATRONAT"),
        PRISON_COLONY("PRISON COLONY", "STRÄFLINGSKOLONIE"),
        THEOCRACY("THEOCRACY", "GOTTESSTAAT"),
        WORKSHOP("WORKSHOP", "WERKSTATT"),
        NONE("NONE", "LEER");
        //@formatter:on

        private final String name;
        private final List<String> otherNames;

        private Government(String name, String... otherNames) {
            this.name = name;
            this.otherNames = otherNames == null || otherNames.length <= 0 ? Collections.emptyList() : Arrays.asList(otherNames);
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
                for (String otherName : e.getOtherNames()) {
                    error = MiscUtil.levenshteinError(otherName, fixedName);
                    if (error <= 0.25f && error < bestError) {
                        best = e;
                        bestError = error;
                    }
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

        public List<String> getOtherNames() {
            return this.otherNames;
        }

    }

    public static enum Allegiance {

        //@formatter:off
        ALLIANCE("ALLIANCE", "ALLIANZ"),
        EMPIRE("EMPIRE", "IMPERIUM"),
        FEDERATION("FEDERATION", "FÖDERATION"),
        INDEPENDENT("INDEPENDENT", "UNABHÄNGIG"),
        NONE("NONE", "LEER");
        //@formatter:on

        private final String name;
        private final List<String> otherNames;

        private Allegiance(String name, String... otherNames) {
            this.name = name;
            this.otherNames = otherNames == null || otherNames.length <= 0 ? Collections.emptyList() : Arrays.asList(otherNames);
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
                for (String otherName : e.getOtherNames()) {
                    error = MiscUtil.levenshteinError(otherName, fixedName);
                    if (error <= 0.25f && error < bestError) {
                        best = e;
                        bestError = error;
                    }
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

        public List<String> getOtherNames() {
            return this.otherNames;
        }

    }

    public static enum State {

        //@formatter:off
        BOOM("BOOM", "AUFSCHWUNG"),
        BUST("BUST", "KRISE"),
        FAMINE("FAMINE", "HUNGERSNOT"),
        CIVIL_UNREST("CIVIL UNREST", "UNRUHEN"),
        CIVIL_WAR("CIVIL WAR", "BÜRGERKRIEG"),
        ELECTION("ELECTION", "WAHLEN"),
        EXPANSION("EXPANSION"),
        LOCKDOWN("LOCKDOWN", "ABRIEGELUNG"),
        OUTBREAK("OUTBREAK", "AUSBRUCH"),
        WAR("WAR", "KRIEG"),
        NONE("NONE", "N/V"),
        RETREAT("RETREAT", "PLEITE"),
        INVESTMENT("INVESTMENT", "INVESTITION");
        //@formatter:on

        private final String name;
        private final List<String> otherNames;

        private State(String name, String... otherNames) {
            this.name = name;
            this.otherNames = otherNames == null || otherNames.length <= 0 ? Collections.emptyList() : Arrays.asList(otherNames);
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
                for (String otherName : e.getOtherNames()) {
                    error = MiscUtil.levenshteinError(otherName, fixedName);
                    if (error <= 0.25f && error < bestError) {
                        best = e;
                        bestError = error;
                    }
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

        public List<String> getOtherNames() {
            return this.otherNames;
        }

    }

    public static enum Relationship {

        //@formatter:off
        HOSTILE("HOSTILE", "FEINDLICH"),
        UNFRIENDLY("UNFRIENDLY", "NICHT WOHLGESINNT"),
        NEUTRAL("NEUTRAL"),
        CORDIAL("CORDIAL", "HERZLICH"),
        FRIENDLY("FRIENDLY", "WOHLGESINNT"),
        ALLIED("ALLIED", "VERBÜNDET");
        //@formatter:on

        private final String name;
        private final List<String> otherNames;

        private Relationship(String name, String... otherNames) {
            this.name = name;
            this.otherNames = otherNames == null || otherNames.length <= 0 ? Collections.emptyList() : Arrays.asList(otherNames);
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
                for (String otherName : e.getOtherNames()) {
                    error = MiscUtil.levenshteinError(otherName, fixedName);
                    if (error <= 0.25f && error < bestError) {
                        best = e;
                        bestError = error;
                    }
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

        public List<String> getOtherNames() {
            return this.otherNames;
        }

    }

    public static enum KnownLabel {

        //@formatter:off
        CONTROLLING_FACTION("CONTROLLING FACTION", "KONTROLLIERENDE FRAKTION"),
        FACTION("FACTION", "FRAKTION"),
        GOVERNMENT("GOVERNMENT", "REGIERUNG"),
        ALLEGIANCE("ALLEGIANCE", "ZUGEHÖRIGKEIT"),
        INFLUENCE("INFLUENCE", "EINFLUSS"),
        STATE("STATE", "ZUSTAND"),
        RELATIONSHIP("RELATIONSHIP", "BEZIEHUNG"),
        BACK("BACK", "ZURÜCK"),
        EXIT("EXIT", "VERLASSEN");
        //@formatter:on

        private final String name;
        private final List<String> otherNames;

        private KnownLabel(String name, String... otherNames) {
            this.name = name;
            this.otherNames = otherNames == null || otherNames.length <= 0 ? Collections.emptyList() : Arrays.asList(otherNames);
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
                for (String otherName : e.getOtherNames()) {
                    error = MiscUtil.levenshteinError(otherName, fixedName);
                    if (error <= 0.25f && error < bestError) {
                        best = e;
                        bestError = error;
                    }
                }
            }
            return best;
        }

        public String getName() {
            return this.name;
        }

        public List<String> getOtherNames() {
            return this.otherNames;
        }

    }

    public static enum KnownFaction {

        //@formatter:off
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
        /** Present in Ngaru and Noegin */
        NGARU_SERVICES("NGARU SERVICES"),
        NOEGIN_PURPLE_BOYS("NOEGIN PURPLE BOYS"),
        PARTNERSHIP_OF_NGARU("PARTNERSHIP OF NGARU"),
        ROSS_754_LABOUR("ROSS 754 LABOUR"),
        SUN_TAKUSH_POWER_PLC("SUN TAKUSH POWER PLC"),
        UNITED_LP_575_38_MOVEMENT("UNITED LP 575-38 MOVEMENT"),
        UNITING_NOEGIN("UNITING NOEGIN"),
        UZUMERU_NETCOMS_INCORPORATED("UZUMERU NETCOMS INCORPORATED"),
        V1703_AQUILAE_NATURAL_LIMITED("V1703 AQUILAE NATURAL LIMITED"),
        PARTNERSHIP_OF_ROSS_193("PARTNERSHIP OF ROSS 193"), // Ngaru
        LHS_3564_CONSERVATIVES("LHS 3564 CONSERVATIVES"), // Nez Pelliri
        PEOPLE_S_MIKINN_LIBERALS("PEOPLE'S MIKINN LIBERALS"), // Mikinn
        _51_AQUILAE_SILVER_PUBLIC_INC("51 AQUILAE SILVER PUBLIC INC"), // Mikinn
        MOB_OF_MIKINN("MOB OF MIKINN"), // Mikinn
        BUREAU_OF_MIKINN_LEAGUE("BUREAU OF MIKINN LEAGUE"), // Mikinn
        MIKINN_GOLD_FEDERAL_INDUSTRIES("MIKINN GOLD FEDERAL INDUSTRIES"), // Mikinn
        LP_635_46_GOLD_POWER_NETWORK("LP 635-46 GOLD POWER NETWORK"), // LP 635-46
        LP_635_46_SYSTEMS("LP 635-46 SYSTEMS"), // LP 635-46
        EARLS_OF_LP_635_46("EARLS OF LP 635-46"), // LP 635-46
        LHS_3564_SYSTEMS("LHS 3564 SYSTEMS"), // LP 635-46
        LALANDE_39866_CO("LALANDE 39866 CO"), // LALANDE 39866
        PROGRESSIVE_PARTY_OF_LALANDE_39866("PROGRESSIVE PARTY OF LALANDE 39866"), // LALANDE 39866
        LALANDE_39866_CORP_("LALANDE 39866 CORP."), // LALANDE 39866
        LALANDE_39866_GOLD_CREW("LALANDE 39866 GOLD CREW"), // LALANDE 39866
        AUTOCRACY_OF_LALANDE_39866("AUTOCRACY OF LALANDE 39866"), // LALANDE 39866
        GERMAN_PILOT_LOUNGE("GERMAN PILOT LOUNGE");
        //@formatter:on

        private final String name;

        private KnownFaction(String name) {
            this.name = name;
        }

        public static KnownFaction findBestMatching(String name) {
            KnownFaction best = null;
            float bestError = Float.MAX_VALUE;
            String fixedName = name.toUpperCase();
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

    public static class FactionScanException extends RuntimeException {

        private static final long serialVersionUID = -373760834141110883L;

        private final String reasonCode;
        private final String reasonMessage;
        private final OcrResult ocrResult;

        public FactionScanException(String reasonCode, String reasonMessage, OcrResult ocrResult) {
            super(reasonMessage);

            this.reasonCode = reasonCode;
            this.reasonMessage = reasonMessage;
            this.ocrResult = ocrResult;
        }

        public FactionScanException(String reasonCode, String reasonMessage, OcrResult ocrResult, Throwable cause) {
            super(reasonMessage, cause);

            this.reasonCode = reasonCode;
            this.reasonMessage = reasonMessage;
            this.ocrResult = ocrResult;
        }

        public String getReasonCode() {
            return this.reasonCode;
        }

        public String getReasonMessage() {
            return this.reasonMessage;
        }

        public OcrResult getOcrResult() {
            return this.ocrResult;
        }

    }

}
