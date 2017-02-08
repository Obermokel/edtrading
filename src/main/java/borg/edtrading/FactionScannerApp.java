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
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

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
        characterLocator = new CharacterLocator(2, 40, 16, 40, 1); // min 2x16, max 40x40, 1px border
        templates = Template.fromFolder("BodyScanner");

        while (!Thread.interrupted()) {
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
                FileUtils.moveFileToDirectory(screenshotFile, new File(doneDir, systemFactions.getSystemName()), true);
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
        systemFactions.setSystemName(guessSystemNameByFactions(systemFactions, screenshotFile, ocrResult));

        String tableName = "INFLUENCE_" + systemFactions.getSystemName().toUpperCase().replaceAll("\\W", "_");
        String date = new SimpleDateFormat("dd.MM.yyyy").format(systemFactions.getDate());
        GoogleSpreadsheet gplInfAndState = new GoogleSpreadsheet(spreadsheetId, tableName);
        GoogleTable tbl = gplInfAndState.getTable(tableName);
        if (tbl == null) {
            throw new FactionScanException("TABLE NOT FOUND", "Table '" + tableName + "' not found in Google sheet", ocrResult);
            //throw new RuntimeException("Table '" + tableName + "' not found");
        } else {
            for (KnownFaction faction : systemFactions.getFactions().keySet()) {
                if (systemFactions.getFactions().get(faction).getInfluence() != null) {
                    String factionName = faction.getName();
                    String influence = String.format(Locale.GERMANY, "%.1f%%", systemFactions.getFactions().get(faction).getInfluence());

                    int colIdx = tbl.getColumnIndex(factionName);
                    if (colIdx < 0) {
                        logger.info("Adding column '" + factionName + "' to table '" + tableName + "'");
                        colIdx = tbl.addColumn(factionName);
                    }
                    int rowIdx = tbl.getRowIndex(date);
                    if (rowIdx < 0) {
                        logger.info("Adding row '" + date + "' to table '" + tableName + "'");
                        rowIdx = tbl.addRow(date);
                    }
                    String existingValue = tbl.getCellValue(rowIdx, colIdx);
                    if (StringUtils.isNotEmpty(existingValue)) {
                        logger.warn("Overwriting " + existingValue + " with " + influence + " for " + factionName + " (" + date + ")");
                    }
                    tbl.setCellValue(rowIdx, colIdx, influence);
                }
            }
        }

        return systemFactions;
    }

    //    static SortedSet<String> unknownFactions = new TreeSet<>();
    //
    //    public static void main(String[] args) throws IOException, ParseException {
    //        List<File> screenshotFiles = FactionScannerApp.selectAllScreenshots();
    //
    //        CharacterLocator characterLocator = new CharacterLocator(2, 40, 16, 40, 1); // min 2x16, max 40x40, 1px border
    //        List<Template> templates = Template.fromFolder("BodyScanner");
    //
    //        SortedMap<String, SystemFactions> systemsByName = new TreeMap<>();
    //        for (File screenshotFile : screenshotFiles) {
    //            SystemFactions systemFactions = new SystemFactions("FAKE");
    //
    //            Screenshot screenshot = Screenshot.loadFromFile(screenshotFile, 3840, 2160, null);
    //            Region region = screenshot.getAsRegion();
    //            //x=0,y=350,w=840,h=1620
    //
    //            OcrTask ocrTask = new OcrTask(region, characterLocator, templates);
    //            ocrTask.setDebugAlphanumTemplates(false);
    //            ocrTask.setDebugAlphanumTextLines(false);
    //            ocrTask.setDebugAllTemplates(false);
    //            ocrTask.setDebugAllTextLines(false);
    //            OcrResult ocrResult = new OcrExecutor().executeOcr(ocrTask);
    //            ocrResult.writeDebugImages();
    //            updateSystemFactions(systemFactions, ocrResult);
    //            String systemName = guessSystemNameByFactions(systemFactions, screenshotFile);
    //            systemFactions.setSystemName(systemName);
    //            SystemFactions prev = systemsByName.put(systemName, systemFactions);
    //            if (prev != null) {
    //                systemFactions.mergeWith(prev);
    //            }
    //        }
    //
    //        Sheets service = getSheetsService();
    //        String spreadsheetId = "1z5USvjTp_htXdsd2o3qrm6DUgL7tlGmHoB8Xh51Fms0";
    //        for (SystemFactions systemFactions : systemsByName.values()) {
    //            List<Object> row = new ArrayList<>();
    //            String tableName = "INFLUENCE_" + systemFactions.getSystemName().toUpperCase().replace(" ", "_").replace("-", "_");
    //            String columnRange = "A:" + "ABCDEFGHIJKLMNOPQRSTUVWXYZ".charAt(systemFactions.getFactions().size());
    //            row.add(new SimpleDateFormat("dd.MM.yyyy").format(systemFactions.getDate()));
    //
    //            System.out.println("==== " + systemFactions.getSystemName() + " ====");
    //            System.out.println("---- " + systemFactions.getControllingFaction() + " ----");
    //            for (KnownFaction faction : systemFactions.getFactions().keySet()) {
    //                System.out.println("* " + faction.getName());
    //                SystemFaction systemFaction = systemFactions.getFactions().get(faction);
    //                System.out.println("Government:    " + systemFaction.getGovernment());
    //                System.out.println("Allegiance:    " + systemFaction.getAllegiance());
    //                System.out.println("Influence:     " + systemFaction.getInfluence());
    //                System.out.println("State:         " + systemFaction.getState());
    //                System.out.println("Relationship:  " + systemFaction.getRelationship());
    //                row.add(String.format(Locale.GERMANY, "%.1f%%", systemFaction.getInfluence()));
    //            }
    //            ValueRange vr = new ValueRange();
    //            List<List<Object>> rows = new ArrayList<>();
    //            rows.add(row);
    //            vr.setValues(rows);
    //            if (!SKIP_GOOGLE_UPDATE) {
    //                Append append = service.spreadsheets().values().append(spreadsheetId, tableName + "!" + columnRange, vr);
    //                append.setValueInputOption("USER_ENTERED");
    //                AppendValuesResponse appendResponse = append.execute();
    //                System.out.println(appendResponse.toPrettyString());
    //            }
    //        }
    //        service.spreadsheets().get("").setIncludeGridData(true).execute().getSheets().get(0).getData().get(0).getRowData();
    //        for (String unknownFaction : unknownFactions) {
    //            System.out.println(unknownFaction);
    //        }
    //    }

    private static void updateSystemFactions(SystemFactions systemFactions, OcrResult ocrResult) throws ParseException {
        File screenshotFile = ocrResult.getOcrTask().getScreenshotRegion().getScreenshot().getFile();
        Date date = new Date(screenshotFile.lastModified());
        if (screenshotFile.getName().matches("\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}\\-\\d{2}\\-\\d{2} .+")) {
            date = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss").parse(screenshotFile.getName().substring(0, "0000-00-00 00-00-00".length()));
        }
        if (DateUtils.toCalendar(date).get(Calendar.HOUR_OF_DAY) >= 20) {
            date = DateUtils.addDays(DateUtils.truncate(date, Calendar.DATE), 1); // Treat as next day already (BGS should have been updated)
        } else if (DateUtils.toCalendar(date).get(Calendar.HOUR_OF_DAY) >= 19 && DateUtils.toCalendar(date).get(Calendar.MINUTE) >= 30) {
            date = DateUtils.addDays(DateUtils.truncate(date, Calendar.DATE), 1); // Treat as next day already (BGS should have been updated)
        } else {
            date = DateUtils.truncate(date, Calendar.DATE);
        }
        systemFactions.setDate(date);

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
                        if (currentFaction == null && systemFactions.getFactions().size() < 3) {
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

    private static String guessSystemNameByFactions(SystemFactions systemFactions, File screenshotFile, OcrResult ocrResult) {
        Set<KnownFaction> factions = systemFactions.getFactions().keySet();
        List<String> possibleSystemNames = new ArrayList<>();

        // NEZ_PELLIRI_GANG: Nez Pelliri && LP 635-46

        if (factions.contains(KnownFaction.INDEPENDENTS_OF_MARIDAL) || factions.contains(KnownFaction.JUSTICE_PARTY_OF_MARIDAL)) {
            possibleSystemNames.add("MARIDAL");
        }
        if (factions.contains(KnownFaction.LP_575_38_BLUE_TRANSPORT_COMMS) || factions.contains(KnownFaction.LIBERALS_OF_LP_575_38) || factions.contains(KnownFaction.LP_575_38_ORGANISATION)) {
            possibleSystemNames.add("LP 575-38");
        }
        if (factions.contains(KnownFaction.ALLIANCE_OF_HRISASTSHI) || factions.contains(KnownFaction.HRISASTSHI_CO) || factions.contains(KnownFaction.HRISASTSHI_JET_BOYS)) {
            possibleSystemNames.add("HRISASTSHI");
        }
        if (factions.contains(KnownFaction.PEOPLE_S_MIKINN_LIBERALS) || factions.contains(KnownFaction.MOB_OF_MIKINN)) {
            possibleSystemNames.add("MIKINN");
        } else if (factions.contains(KnownFaction.BUREAU_OF_MIKINN_LEAGUE) && factions.contains(KnownFaction.MIKINN_GOLD_FEDERAL_INDUSTRIES)) {
            possibleSystemNames.add("MIKINN");
        } else if (factions.contains(KnownFaction._51_AQUILAE_SILVER_PUBLIC_INC) && factions.contains(KnownFaction.MIKINN_GOLD_FEDERAL_INDUSTRIES)) {
            possibleSystemNames.add("MIKINN");
        } else if (factions.contains(KnownFaction._51_AQUILAE_SILVER_PUBLIC_INC) && factions.contains(KnownFaction.BUREAU_OF_MIKINN_LEAGUE)) {
            possibleSystemNames.add("MIKINN");
        }
        if (factions.contains(KnownFaction.NEZ_PELLIRI_GANG) && factions.contains(KnownFaction.EARLS_OF_LP_635_46) && factions.contains(KnownFaction.LHS_3564_SYSTEMS)) {
            possibleSystemNames.add("LP 635-46");
        } else if (factions.contains(KnownFaction.NEW_LP_635_46_CONFEDERATION) && factions.contains(KnownFaction.LHS_3564_CONSERVATIVES) && factions.contains(KnownFaction.LP_635_46_GOLD_POWER_NETWORK)) {
            possibleSystemNames.add("LP 635-46");
        } else if (factions.contains(KnownFaction.LP_635_46_GOLD_POWER_NETWORK) && factions.contains(KnownFaction.EARLS_OF_LP_635_46) && factions.contains(KnownFaction.LHS_3564_CONSERVATIVES)) {
            possibleSystemNames.add("LP 635-46");
        } else if (factions.contains(KnownFaction.LP_635_46_GOLD_POWER_NETWORK) && factions.contains(KnownFaction.NEZ_PELLIRI_GANG) && factions.contains(KnownFaction.LHS_3564_CONSERVATIVES)) {
            possibleSystemNames.add("LP 635-46");
        }
        if (factions.contains(KnownFaction.PARTNERSHIP_OF_NGARU) || factions.contains(KnownFaction.NGARU_CRIMSON_COUNCIL)) {
            possibleSystemNames.add("NGARU");
        } else if (factions.contains(KnownFaction.NGARU_SERVICES) && factions.contains(KnownFaction.PARTNERSHIP_OF_ROSS_193)) {
            possibleSystemNames.add("NGARU");
        }
        if (factions.contains(KnownFaction.NEZ_PELLIRI_GANG) && factions.contains(KnownFaction.NEZ_PELLIRI_DOMINION) && factions.contains(KnownFaction.LHS_3564_CONSERVATIVES)) {
            possibleSystemNames.add("NEZ PELLIRI");
        } else if (factions.contains(KnownFaction.NEZ_PELLIRI_GANG) && factions.contains(KnownFaction.NEZ_PELLIRI_SILVER_GALACTIC) && factions.contains(KnownFaction.LHS_3564_CONSERVATIVES)) {
            possibleSystemNames.add("NEZ PELLIRI");
        }
        if (factions.contains(KnownFaction.ALLIANCE_OF_STHA_181) || factions.contains(KnownFaction.UNITING_NOEGIN) || factions.contains(KnownFaction.NOEGIN_PURPLE_BOYS)) {
            possibleSystemNames.add("NOEGIN");
        }
        if (factions.contains(KnownFaction.UZUMERU_NETCOMS_INCORPORATED) || factions.contains(KnownFaction.BAVARINGONI_BLUE_RATS)) {
            possibleSystemNames.add("BAVARINGONI");
        }

        if (possibleSystemNames.isEmpty()) {
            throw new FactionScanException("SYSTEM UNKNOWN", "I cannot tell the system name from the scanned factions: " + factions, ocrResult);
        } else if (possibleSystemNames.size() > 1) {
            //            if (possibleSystemNames.size() == 2 && possibleSystemNames.contains("NEZ PELLIRI") && possibleSystemNames.contains("LP 635-46")) {
            //                if (factions.contains(KnownFaction.GERMAN_PILOT_LOUNGE) && systemFactions.getFactions().get(KnownFaction.GERMAN_PILOT_LOUNGE).getInfluence() != null) {
            //                    if (systemFactions.getFactions().get(KnownFaction.GERMAN_PILOT_LOUNGE).getInfluence().doubleValue() >= 40) {
            //                        return "NEZ PELLIRI";
            //                    } else {
            //                        return "LP 635-46";
            //                    }
            //                }
            //            }
            throw new FactionScanException("SYSTEM AMBIGUOUS", "The scanned factions are present in more than one system: " + possibleSystemNames, ocrResult);
        } else {
            return possibleSystemNames.get(0);
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
            fileList.add(f);
        }
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
        RELATIONSHIP("RELATIONSHIP"),
        BACK("BACK"),
        EXIT("EXIT");
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
        GERMAN_PILOT_LOUNGE("GERMAN PILOT LOUNGE");
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
