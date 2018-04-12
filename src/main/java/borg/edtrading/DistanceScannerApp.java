package borg.edtrading;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.apache.commons.io.FileExistsException;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import borg.edtrading.cfg.Constants;
import borg.edtrading.ocr.CharacterLocator;
import borg.edtrading.ocr.OcrExecutor;
import borg.edtrading.ocr.OcrResult;
import borg.edtrading.ocr.OcrTask;
import borg.edtrading.ocr.TextLine;
import borg.edtrading.ocr.screenshots.Region;
import borg.edtrading.ocr.screenshots.Screenshot;
import borg.edtrading.ocr.templatematching.Template;
import borg.edtrading.util.MiscUtil;

public class DistanceScannerApp {

    static final Logger logger = LogManager.getLogger(DistanceScannerApp.class);

    private static CharacterLocator characterLocator = null;
    private static List<Template> templates = null;

    public static void main(String[] args) throws IOException {
        //        while (!Thread.interrupted()) {
        List<File> screenshotFiles = DistanceScannerApp.selectAllScreenshots(Constants.DISTANCES_SCREENSHOTS_DIR);

        if (!screenshotFiles.isEmpty()) {
            characterLocator = new CharacterLocator(2, 40, 16, 40, 1); // min 2x16, max 40x40, 1px border
            //templates = Template.fromFolder("BodyScanner"); // FIXME on mac
            templates = Template.fromFolder(new File(System.getProperty("user.home"), "BodyScanner"));

            processUploadedScreenshots(screenshotFiles);
        }

        //            try {
        //                Thread.sleep(10000L);
        //            } catch (InterruptedException e) {
        //                break;
        //            }
        //        }
    }

    static List<File> selectAllScreenshots(File srcDir) {
        File[] fileArray = srcDir.listFiles(new FileFilter() {
            @Override
            public boolean accept(File f) {
                return f.getName().startsWith("Distance_") && f.getName().endsWith(".png");
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
                return f1.getName().toLowerCase().compareTo(f2.getName().toLowerCase());
            }
        });
        return fileList;
    }

    private static void processUploadedScreenshots(List<File> screenshotFiles) {
        File doneDir = new File(Constants.DISTANCES_SCREENSHOTS_DIR, "done");
        File errorDir = new File(Constants.DISTANCES_SCREENSHOTS_DIR, "error");
        File duplicatesDir = new File(Constants.DISTANCES_SCREENSHOTS_DIR, "duplicates");

        for (File screenshotFile : screenshotFiles) {
            try {
                if (screenshotFile.getName().matches(".+\\(\\d+\\).png")) {
                    FileUtils.moveFileToDirectory(screenshotFile, duplicatesDir, true);
                } else {
                    processScreenshotFile(screenshotFile);
                    logger.info("Successfully scanned " + screenshotFile);
                    File destFile = new File(doneDir, screenshotFile.getName());
                    if (destFile.exists()) {
                        destFile.delete();
                    }
                    FileUtils.moveFileToDirectory(screenshotFile, doneDir, true);
                }
            } catch (Exception e1) {
                if (System.currentTimeMillis() - screenshotFile.lastModified() < 600000L) {
                    logger.warn("Failed to process " + screenshotFile + ": " + e1);
                } else {
                    logger.error("Moving unparseable screenshot to error dir: " + screenshotFile, e1);
                    try {
                        FileUtils.moveFileToDirectory(screenshotFile, errorDir, true);
                        //                        if (e1 instanceof FactionScanException) {
                        //                            FactionScanException fse = (FactionScanException) e1;
                        //                            if (StringUtils.isNotEmpty(fse.getReasonCode()) && fse.getOcrResult() != null
                        //                                    && fse.getOcrResult().getAllTextLinesDebugImage() != null) {
                        //                                BufferedImage debugImage = fse.getOcrResult().getAllTextLinesDebugImage();
                        //                                Graphics2D g = debugImage.createGraphics();
                        //                                g.setFont(new Font("Tahoma", Font.BOLD, 32));
                        //                                g.setColor(Color.RED);
                        //                                g.drawString(fse.getReasonMessage(), 1000, 100);
                        //                                g.dispose();
                        //                                ImageIO.write(debugImage, "PNG",
                        //                                        new File(errorDir, screenshotFile.getName().replace(".png", " " + fse.getReasonCode() + ".png")));
                        //                            }
                        //                        }
                    } catch (FileExistsException e2) {
                        screenshotFile.delete();
                    } catch (IOException e2) {
                        logger.error("Failed to move " + screenshotFile + " to " + errorDir, e2);
                    }
                }
            }
        }
    }

    private static void processScreenshotFile(File screenshotFile) throws IOException, ParseException {
        Screenshot screenshot = Screenshot.loadFromFile(screenshotFile, 3840, 2160, null);
        Region region = screenshot.getAsRegion(); //x=0,y=350,w=840,h=1620

        OcrTask ocrTask = new OcrTask(region, characterLocator, templates);
        ocrTask.setDebugAlphanumTemplates(true);
        ocrTask.setDebugAlphanumTextLines(true);
        ocrTask.setDebugAllTemplates(true);
        ocrTask.setDebugAllTextLines(true);
        OcrResult ocrResult = new OcrExecutor().executeOcr(ocrTask);
        ocrResult.writeDebugImages();

        for (TextLine tl : ocrResult.getTextLines()) {
            //            logger.info(tl.toString());
            String text = tl.toText().replace("→", " ").trim();
            if (text.contains(":")) {
                String[] labelAndValue = text.split(":");
                if (labelAndValue.length == 2) {
                    if (MiscUtil.levenshteinError(labelAndValue[0], "DISTANCE") < 0.25f) {
                        String fixedValue = labelAndValue[1] //
                                .replace("O", "0") //
                                .replace("S", "5") //
                                .replace("B", "8") //
                                .replace("L", "") //
                                .replace("Y", ""); //
                        logger.info(fixedValue);
                    }
                }
            }
        }

        //        SystemFactions systemFactions = new SystemFactions("FAKE SYSTEM");
        //        updateSystemFactions(systemFactions, ocrResult);
        //        updateDateAndSystemFromFilename(systemFactions, screenshotFile);
        //
        //        String date = new SimpleDateFormat("dd.MM.yyyy").format(systemFactions.getDate());
        //        String tableNameInfluence = "INFLUENCE_" + systemFactions.getSystemName().toUpperCase().replaceAll("\\W", "_");
        //        GoogleSpreadsheet gplInfluence = new GoogleSpreadsheet(spreadsheetId, tableNameInfluence);
        //        GoogleTable tblInfluence = gplInfluence.getTable(tableNameInfluence);
        //        String tableNameState = "STATE_" + systemFactions.getSystemName().toUpperCase().replaceAll("\\W", "_");
        //        GoogleSpreadsheet gplState = new GoogleSpreadsheet(spreadsheetId, tableNameState);
        //        GoogleTable tblState = gplState.getTable(tableNameState);
        //        if (tblInfluence == null) {
        //            throw new FactionScanException("TABLE NOT FOUND", "Table '" + tableNameInfluence + "' not found in Google sheet", ocrResult);
        //            //throw new RuntimeException("Table '" + tableName + "' not found");
        //        } else {
        //            for (KnownFaction faction : systemFactions.getFactions().keySet()) {
        //                if (systemFactions.getFactions().get(faction).getInfluence() != null) {
        //                    String factionName = faction.getName();
        //                    String influence = String.format(Locale.GERMANY, "%.1f%%", systemFactions.getFactions().get(faction).getInfluence());
        //
        //                    int colIdx = tblInfluence.getColumnIndex(factionName);
        //                    if (colIdx < 0) {
        //                        logger.info("Adding column '" + factionName + "' to table '" + tableNameInfluence + "'");
        //                        colIdx = tblInfluence.addColumn(factionName);
        //                    }
        //                    int rowIdx = tblInfluence.getRowIndex(date);
        //                    if (rowIdx < 0) {
        //                        logger.info("Adding row '" + date + "' to table '" + tableNameInfluence + "'");
        //                        rowIdx = tblInfluence.addRow(date);
        //                    }
        //                    String existingValue = tblInfluence.getCellValue(rowIdx, colIdx);
        //                    boolean allowOverwrite = true;
        //                    if (StringUtils.isNotEmpty(existingValue) && !existingValue.equals(influence)) {
        //                        BigDecimal existingValueBD = new BigDecimal(existingValue.replace(",", ".").replace("%", ""));
        //                        if (systemFactions.getFactions().get(faction).getInfluence().floatValue() <= 0) {
        //                            allowOverwrite = false;
        //                        } else if (existingValueBD.floatValue() <= 0) {
        //                            logger.warn("Overwriting " + existingValue + " with " + influence + " for " + factionName + " (" + date + ")");
        //                        } else {
        //                            allowOverwrite = false;
        //                        }
        //                    }
        //                    if (allowOverwrite) {
        //                        tblInfluence.setCellValue(rowIdx, colIdx, influence);
        //                    }
        //                }
        //                if (systemFactions.getFactions().get(faction).getState() != null) {
        //                    String factionName = faction.getName();
        //                    String state = systemFactions.getFactions().get(faction).getState().toString();
        //
        //                    int colIdx = tblState.getColumnIndex(factionName);
        //                    if (colIdx < 0) {
        //                        logger.info("Adding column '" + factionName + "' to table '" + tableNameState + "'");
        //                        colIdx = tblState.addColumn(factionName);
        //                    }
        //                    int rowIdx = tblState.getRowIndex(date);
        //                    if (rowIdx < 0) {
        //                        logger.info("Adding row '" + date + "' to table '" + tableNameState + "'");
        //                        rowIdx = tblState.addRow(date);
        //                    }
        //                    String existingValue = tblState.getCellValue(rowIdx, colIdx);
        //                    if (StringUtils.isNotEmpty(existingValue) && !existingValue.equals(state)) {
        //                        logger.warn("Overwriting " + existingValue + " with " + state + " for " + factionName + " (" + date + ")");
        //                    }
        //                    tblState.setCellValue(rowIdx, colIdx, state);
        //                }
        //            }
        //        }
        //
        //        return systemFactions;
    }

}
