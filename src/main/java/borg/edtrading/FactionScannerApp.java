package borg.edtrading;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.Serializable;
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

import org.apache.commons.io.FileExistsException;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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
			File uploadDir = new File(Constants.FACTION_SCREENSHOTS_DIR, "upload");
			List<File> screenshotFiles = FactionScannerApp.selectAllScreenshots(uploadDir);

			if (!screenshotFiles.isEmpty()) {
				characterLocator = new CharacterLocator(2, 40, 16, 40, 1); // min 2x16, max 40x40, 1px border
				templates = Template.fromFolder("BodyScanner");

				reloadFactionNames();
				processUploadedScreenshots(screenshotFiles);
			}

			try {
				Thread.sleep(10000L);
			} catch (InterruptedException e) {
				break;
			}
		}
	}

	private static void reloadFactionNames() {
		File factionsFile = new File(Constants.FACTION_SCREENSHOTS_DIR, "factions.txt");

		try {
			KnownFaction.reload(factionsFile);
		} catch (IOException e) {
			logger.error("Failed to reload faction names from " + factionsFile, e);
		}
	}

	private static void processUploadedScreenshots(List<File> screenshotFiles) {
		File doneDir = new File(Constants.FACTION_SCREENSHOTS_DIR, "done");
		File errorDir = new File(Constants.FACTION_SCREENSHOTS_DIR, "error");
		File duplicatesDir = new File(Constants.FACTION_SCREENSHOTS_DIR, "duplicates");

		for (File screenshotFile : screenshotFiles) {
			try {
				if (screenshotFile.getName().matches(".+\\(\\d+\\).png")) {
					FileUtils.moveFileToDirectory(screenshotFile, duplicatesDir, true);
				} else {
					SystemFactions systemFactions = processScreenshotFile(screenshotFile);
					logger.info("Successfully scanned " + screenshotFile);
					File destDir = new File(doneDir, systemFactions.getSystemName());
					File destFile = new File(destDir, screenshotFile.getName());
					if (destFile.exists()) {
						destFile.delete();
					}
					FileUtils.moveFileToDirectory(screenshotFile, destDir, true);
				}
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
					boolean allowOverwrite = true;
					if (StringUtils.isNotEmpty(existingValue) && !existingValue.equals(influence)) {
						//BigDecimal existingValueBD = new BigDecimal(existingValue.replace(",", ".").replace("%", ""));
						if (systemFactions.getFactions().get(faction).getInfluence().floatValue() <= 0) {
							allowOverwrite = false;
						} else {
							logger.warn("Overwriting " + existingValue + " with " + influence + " for " + factionName + " (" + date + ")");
						}
					}
					if (allowOverwrite) {
						tblInfluence.setCellValue(rowIdx, colIdx, influence);
					}
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
							String fixedValue = currentValue.toUpperCase().replace(" ", "").replace("%", "").replace(",", ".").replace("▪", "");
							fixedValue = fixedValue.replace("O", "0").replace("D", "0").replace("I", "1").replace("S", "5").replace("B", "8");
							try {
								systemFaction.setInfluence(new BigDecimal(fixedValue.trim()));
							} catch (NumberFormatException e) {
								systemFaction.setInfluence(new BigDecimal("-1"));
								//throw new FactionScanException("INFLUENCE UNKNOWN", "Failed to parse '" + currentValue + "' (fixed to '" + fixedValue + "') to influence of " + currentFaction, ocrResult);
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
		LOCKDOWN("LOCKDOWN", "KRIEGSRECHT"),
		OUTBREAK("OUTBREAK", "AUSBRUCH"),
		WAR("WAR", "KRIEG"),
		NONE("NONE", "N/V"),
		RETREAT("RETREAT", "RÜCKZUG"),
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

	public static class KnownFaction implements Serializable, Comparable<KnownFaction> {

		private static final long serialVersionUID = 8240710068001174107L;

		private static final List<KnownFaction> FACTIONS = new ArrayList<>();

		private String name = null;

		public KnownFaction(String name) {
			this.setName(name);
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			KnownFaction other = (KnownFaction) obj;
			if (this.name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!this.name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
			return result;
		}

		@Override
		public int compareTo(KnownFaction other) {
			return this.name.toUpperCase().compareTo(other.name.toUpperCase());
		}

		@Override
		public String toString() {
			return this.name;
		}

		public static void reload(File file) throws IOException {
			KnownFaction.FACTIONS.clear();

			List<String> lines = FileUtils.readLines(file, "UTF-8");
			for (String line : lines) {
				if (StringUtils.isNotBlank(line)) {
					KnownFaction.FACTIONS.add(new KnownFaction(line.trim()));
				}
			}
		}

		public static KnownFaction findBestMatching(String name) {
			KnownFaction best = null;
			float bestError = Float.MAX_VALUE;
			String fixedName = name.toUpperCase();
			for (KnownFaction e : KnownFaction.FACTIONS) {
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

		public void setName(String name) {
			this.name = name;
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
