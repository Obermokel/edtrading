package borg.edtrading.eddb.updater;

import borg.edtrading.Constants;
import borg.edtrading.bodyscanner.ScannedBodyInfo;
import borg.edtrading.data.BodyInfo;
import borg.edtrading.data.Item;
import borg.edtrading.data.Item.ItemType;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.openqa.selenium.By;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.Keys;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * BodyUpdater
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyUpdater implements Closeable {

    static final Logger logger = LogManager.getLogger(BodyUpdater.class);

    private WebDriver driver = null;
    private EddbHistory history = null;

    public BodyUpdater(String username, String password) throws IOException {
        this.history = EddbHistory.load();

        //this.driver = new ChromeDriver();
        this.driver = new FirefoxDriver();
        //this.driver = new HtmlUnitDriver(false);

        this.driver.manage().timeouts().pageLoadTimeout(1, TimeUnit.MINUTES);
        this.driver.get("http://ross.eddb.io");
        this.driver.manage().window().maximize();

        try {
            this.driver.findElement(By.linkText("Login")).click();

            this.driver.findElement(By.cssSelector("#loginform-username")).sendKeys(username);
            this.driver.findElement(By.cssSelector("#loginform-password")).sendKeys(password);
            if (!this.driver.findElement(By.cssSelector("#loginform-rememberme")).isSelected()) {
                this.driver.findElement(By.cssSelector("#loginform-rememberme")).click();
            }
            this.driver.findElement(By.name("login-button")).click();
        } catch (NoSuchElementException e) {
            // Already logged in
        }
    }

    @Override
    public void close() throws IOException {
        if (this.driver != null) {
            this.driver.close();
        }
    }

    public void updateBody(ScannedBodyInfo scannedBodyInfo) throws SystemNotFoundException, IOException {
        if (StringUtils.isEmpty(scannedBodyInfo.getSystemName())) {
            throw new IllegalArgumentException("scannedBodyInfo has no systemName");
        } else if (StringUtils.isEmpty(scannedBodyInfo.getBodyName())) {
            throw new IllegalArgumentException("scannedBodyInfo has no bodyName");
        } else {
            try {
                this.openSystemPage(scannedBodyInfo.getSystemName());
                this.openOrCreateBodyPage(scannedBodyInfo);
                this.enterBasicBodyInformation(scannedBodyInfo);
                this.updatePlanetMaterials(scannedBodyInfo);
                this.submitBodyPage();
                this.history.addScreenshotFinished(scannedBodyInfo.getSystemName(), scannedBodyInfo.getBodyName(), scannedBodyInfo.getScreenshotFilename());
            } catch (TimeoutException | NoSuchElementException | InvalidElementStateException e) {
                logger.error("Failed to process " + scannedBodyInfo.getScreenshotFilename(), e);
                this.history.addScreenshotFailed(scannedBodyInfo.getSystemName(), scannedBodyInfo.getBodyName(), scannedBodyInfo.getScreenshotFilename());
                if (this.driver instanceof TakesScreenshot) {
                    try {
                        final String timestamp = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(new Date());
                        FileUtils.write(new File(Constants.TEMP_DIR, "ERROR_" + timestamp + "_" + scannedBodyInfo.getScreenshotFilename().replace(".png", ".html")), this.driver.getPageSource(), "UTF-8");
                        File screenshotFile = ((TakesScreenshot) this.driver).getScreenshotAs(OutputType.FILE);
                        FileUtils.copyFile(screenshotFile, new File(Constants.TEMP_DIR, "ERROR_" + timestamp + "_" + scannedBodyInfo.getScreenshotFilename()));
                    } catch (Exception se) {
                        logger.warn("Failed to take screenshot: " + se);
                    }
                }
            }
        }
    }

    private void enterBasicBodyInformation(ScannedBodyInfo scannedBodyInfo) throws IOException {
        final String systemName = scannedBodyInfo.getSystemName();
        final String bodyName = fixBodyName(systemName, scannedBodyInfo.getBodyName());
        final String filename = scannedBodyInfo.getScreenshotFilename();

        BigDecimal distanceLsInteger = scannedBodyInfo.getDistanceLs() == null ? null : new BigDecimal(scannedBodyInfo.getDistanceLs().intValue());

        logger.info("Updating body '" + bodyName + "' in system '" + systemName + "'");

        this.updateTextField("bodyform-name", systemName, bodyName, EddbHistory.FIELD_BODY_NAME, bodyName, filename);
        this.updateNumericField("bodyform-distance_to_spawn", systemName, bodyName, EddbHistory.FIELD_DISTANCE_FROM_ARRIVAL, distanceLsInteger, filename);
        // TODO reserve, terraforming
        this.updateSearchableDropdown("s2id_bodyform-type_id", null, systemName, bodyName, EddbHistory.FIELD_BODY_TYPE, scannedBodyInfo.getBodyType(), filename);
        this.updateNumericField("bodyform-earth_masses", systemName, bodyName, EddbHistory.FIELD_EARTH_MASSES, scannedBodyInfo.getEarthMasses(), filename);
        this.updateNumericField("bodyform-radius", systemName, bodyName, EddbHistory.FIELD_RADIUS, scannedBodyInfo.getRadiusKm(), filename);
        this.updateNumericField("bodyform-gravity", systemName, bodyName, EddbHistory.FIELD_GRAVITY, scannedBodyInfo.getGravityG(), filename);
        this.updateNumericField("bodyform-surface_temperature", systemName, bodyName, EddbHistory.FIELD_SURFACE_TEMP, scannedBodyInfo.getSurfaceTempK(), filename);
        this.updateSearchableDropdown("s2id_bodyform-volcanism_type_id", "bodyform-surface_temperature", systemName, bodyName, EddbHistory.FIELD_VOLCANISM, scannedBodyInfo.getVolcanism(), filename);
        this.updateSearchableDropdown("s2id_bodyform-atmosphere_type_id", "bodyform-surface_temperature", systemName, bodyName, EddbHistory.FIELD_ATMOSPHERE_TYPE, scannedBodyInfo.getAtmosphereType(), filename);
        this.updateSolidComposition(systemName, bodyName, scannedBodyInfo.getComposition(), filename);
        this.updateNumericField("bodyform-orbital_period", systemName, bodyName, EddbHistory.FIELD_ORBITAL_PERIOD, scannedBodyInfo.getOrbitalPeriodD(), filename);
        this.updateNumericField("bodyform-semi_major_axis", systemName, bodyName, EddbHistory.FIELD_SEMI_MAJOR_AXIS, scannedBodyInfo.getSemiMajorAxisAU(), filename);
        this.updateNumericField("bodyform-orbital_eccentricity", systemName, bodyName, EddbHistory.FIELD_ORBITAL_ECCENTRICITY, scannedBodyInfo.getOrbitalEccentricity(), filename);
        this.updateNumericField("bodyform-orbital_inclination", systemName, bodyName, EddbHistory.FIELD_ORBITAL_INCLINATION, scannedBodyInfo.getOrbitalInclinationDeg(), filename);
        this.updateNumericField("bodyform-arg_of_periapsis", systemName, bodyName, EddbHistory.FIELD_ARG_OF_PERIAPSIS, scannedBodyInfo.getArgOfPeriapsisDeg(), filename);
        this.updateNumericField("bodyform-rotational_period", systemName, bodyName, EddbHistory.FIELD_ROTATIONAL_PERIOD, scannedBodyInfo.getRotationalPeriodD(), filename);
        if (scannedBodyInfo.getTidallyLocked() != null) {
            String label = scannedBodyInfo.getTidallyLocked().booleanValue() ? "Yes" : "No";
            this.updateClickableDropdown("s2id_bodyform-is_rotational_period_tidally_locked", "bodysolidcomposition-0-share", systemName, bodyName, EddbHistory.FIELD_TIDALLY_LOCKED, label, filename);
        }
        this.updateNumericField("bodyform-axis_tilt", systemName, bodyName, EddbHistory.FIELD_AXIAL_TILT, scannedBodyInfo.getAxialTiltDeg(), filename);
    }

    private void updateSolidComposition(final String systemName, final String bodyName, LinkedHashMap<BodyInfo, BigDecimal> composition, String screenshotFilename) throws IOException {
        int idx = 0;
        if (composition != null) {
            for (BodyInfo material : composition.keySet()) {
                BigDecimal percent = composition.get(material);
                this.updateNumericField("bodysolidcomposition-" + idx + "-share", systemName, bodyName, EddbHistory.FIELD_SOLID_COMPOSITION_SHARE + idx, percent, screenshotFilename);
                this.updateSearchableDropdown("s2id_bodysolidcomposition-" + idx + "-solid_component_id", null, systemName, bodyName, EddbHistory.FIELD_SOLID_COMPOSITION_NAME + idx, material, screenshotFilename);
                idx++;
            }
        }
        for (; idx < 3; idx++) {
            this.updateNumericField("bodysolidcomposition-" + idx + "-share", systemName, bodyName, EddbHistory.FIELD_SOLID_COMPOSITION_SHARE + idx, null, screenshotFilename);
            this.updateSearchableDropdown("s2id_bodysolidcomposition-" + idx + "-solid_component_id", null, systemName, bodyName, EddbHistory.FIELD_SOLID_COMPOSITION_NAME + idx, (String) null, screenshotFilename);
        }
    }

    private void updatePlanetMaterials(ScannedBodyInfo scannedBodyInfo) throws IOException {
        if (scannedBodyInfo.getPlanetMaterials() != null) {
            final String systemName = scannedBodyInfo.getSystemName();
            final String bodyName = fixBodyName(systemName, scannedBodyInfo.getBodyName());
            final String filename = scannedBodyInfo.getScreenshotFilename();

            logger.trace("Opening planet materials editor");
            this.driver.findElement(By.cssSelector("div#elementMaterialEditorController a.btn")).click();

            List<WebElement> elementRows = this.driver.findElements(By.cssSelector("div#elementMaterialEditorController div.elementRow"));
            for (WebElement elementRow : elementRows) {
                String cssClass = elementRow.getAttribute("class");
                boolean markedAsExisting = cssClass.contains("userSelected");

                String elementName = elementRow.findElement(By.cssSelector("div.elementDataRow div.elementName")).getText().replaceAll("\\s", " ");
                elementName = elementName.substring(elementName.indexOf(" ")).trim();

                WebElement toggleLink = elementRow.findElement(By.cssSelector("div.elementDataRow div.elementName a"));
                WebElement shareInput = elementRow.findElement(By.cssSelector("div.elementDataRow div.elementShare input"));

                Item element = Item.findBestMatching(elementName, ItemType.ELEMENT);
                if (element == null) {
                    logger.warn("Unknown element: '" + elementName + "'");
                } else {
                    BigDecimal newValue = scannedBodyInfo.getPlanetMaterials().get(element);
                    if (!markedAsExisting && newValue != null) {
                        this.clickToggleLink(toggleLink, systemName, bodyName, EddbHistory.FIELD_PLANET_MATERIALS_NAME + element.getName(), true, filename);
                    }
                    this.updateNumericField(shareInput, systemName, bodyName, EddbHistory.FIELD_PLANET_MATERIALS_SHARE + element.getName(), newValue, filename);
                    if (markedAsExisting && newValue == null) {
                        this.clickToggleLink(toggleLink, systemName, bodyName, EddbHistory.FIELD_PLANET_MATERIALS_NAME + element.getName(), false, filename);
                    }
                }
            }
        }
    }

    private void clickToggleLink(WebElement toggleLink, String systemName, String bodyName, String fieldName, boolean turnOn, String screenshotFilename) throws IOException {
        String historyEntry = this.history.lookup(systemName, bodyName, fieldName);

        if (historyEntry != null) {
            logger.trace("Skipping " + fieldName + "=" + turnOn + " of body " + bodyName + " in system " + systemName + ". Entry from history: " + historyEntry);
        } else {
            logger.trace("Setting " + fieldName + "=" + turnOn + " of body " + bodyName + " in system " + systemName);

            this.history.toggle(systemName, bodyName, fieldName, turnOn, screenshotFilename);
            toggleLink.click();
        }
    }

    private void updateSearchableDropdown(String dropdownCssId, String inputAboveCssId, String systemName, String bodyName, String fieldName, BodyInfo newValue, String screenshotFilename) throws IOException {
        this.updateSearchableDropdown(dropdownCssId, inputAboveCssId, systemName, bodyName, fieldName, newValue == null ? null : newValue.getName(), screenshotFilename);
    }

    private void updateSearchableDropdown(String dropdownCssId, String inputAboveCssId, String systemName, String bodyName, String fieldName, String newValue, String screenshotFilename) throws IOException {
        String historyEntry = this.history.lookup(systemName, bodyName, fieldName);

        if (historyEntry != null) {
            logger.trace("Skipping " + fieldName + "=" + newValue + " of body " + bodyName + " in system " + systemName + ". Entry from history: " + historyEntry);
        } else {
            logger.trace("Setting " + fieldName + "=" + newValue + " of body " + bodyName + " in system " + systemName);

            if (StringUtils.isNotEmpty(inputAboveCssId)) {
                new Actions(driver).moveToElement(driver.findElement(By.cssSelector("footer.footer")), 0, 0).build().perform();
                new Actions(driver).moveToElement(driver.findElement(By.id(inputAboveCssId)), 0, 0).build().perform();
            }

            String oldValue = null;
            WebElement surroundingDiv = this.driver.findElement(By.id(dropdownCssId));
            WebElement spanChosen = surroundingDiv.findElement(By.cssSelector("span.select2-chosen"));
            WebElement abbrClear = null;
            try {
                abbrClear = surroundingDiv.findElement(By.cssSelector("abbr.select2-search-choice-close"));
                if (abbrClear.isDisplayed()) {
                    oldValue = spanChosen.getText();
                }
            } catch (NoSuchElementException e) {
                // No value selected yet
            }
            WebElement spanDropDown = surroundingDiv.findElement(By.cssSelector("span.select2-arrow"));

            if (oldValue == null) {
                if (newValue == null) {
                    // No change
                } else {
                    // Set
                    this.history.set(systemName, bodyName, fieldName, newValue, screenshotFilename);
                    spanDropDown.click();
                    WebElement searchInput = new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.cssSelector("#select2-drop input")));
                    searchInput.click();
                    searchInput.sendKeys(newValue);
                    searchInput.sendKeys(Keys.ENTER);
                }
            } else {
                if (newValue == null) {
                    // Erase
                    this.history.erase(systemName, bodyName, fieldName, oldValue, screenshotFilename);
                    abbrClear.click();
                } else if (newValue.equalsIgnoreCase(oldValue)) {
                    // No change
                } else {
                    // Change
                    this.history.change(systemName, bodyName, fieldName, oldValue, newValue, screenshotFilename);
                    abbrClear.click();
                    spanDropDown.click();
                    WebElement searchInput = new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.cssSelector("#select2-drop input")));
                    searchInput.click();
                    searchInput.sendKeys(newValue);
                    searchInput.sendKeys(Keys.ENTER);
                }
            }
        }
    }

    private void updateClickableDropdown(String dropdownCssId, String inputAboveCssId, String systemName, String bodyName, String fieldName, String newValue, String screenshotFilename) throws IOException {
        String historyEntry = this.history.lookup(systemName, bodyName, fieldName);

        if (historyEntry != null) {
            logger.trace("Skipping " + fieldName + "=" + newValue + " of body " + bodyName + " in system " + systemName + ". Entry from history: " + historyEntry);
        } else {
            logger.trace("Setting " + fieldName + "=" + newValue + " of body " + bodyName + " in system " + systemName);

            if (StringUtils.isNotEmpty(inputAboveCssId)) {
                new Actions(driver).moveToElement(driver.findElement(By.cssSelector("footer.footer")), 0, 0).build().perform();
                new Actions(driver).moveToElement(driver.findElement(By.id(inputAboveCssId)), 0, 0).build().perform();
            }

            String oldValue = null;
            WebElement surroundingDiv = this.driver.findElement(By.id(dropdownCssId));
            WebElement spanChosen = surroundingDiv.findElement(By.cssSelector("span.select2-chosen"));
            WebElement abbrClear = null;
            try {
                abbrClear = surroundingDiv.findElement(By.cssSelector("abbr.select2-search-choice-close"));
                if (abbrClear.isDisplayed()) {
                    oldValue = spanChosen.getText();
                }
            } catch (NoSuchElementException e) {
                // No value selected yet
            }
            WebElement spanDropDown = surroundingDiv.findElement(By.cssSelector("span.select2-arrow"));

            if (oldValue == null) {
                if (newValue == null) {
                    // No change
                } else {
                    // Set
                    this.history.set(systemName, bodyName, fieldName, newValue, screenshotFilename);
                    spanDropDown.click();
                    this.clickOption(newValue);
                }
            } else {
                if (newValue == null) {
                    // Erase
                    this.history.erase(systemName, bodyName, fieldName, oldValue, screenshotFilename);
                    abbrClear.click();
                } else if (newValue.equalsIgnoreCase(oldValue)) {
                    // No change
                } else {
                    // Change
                    this.history.change(systemName, bodyName, fieldName, oldValue, newValue, screenshotFilename);
                    spanDropDown.click();
                    this.clickOption(newValue);
                }
            }
        }
    }

    private void clickOption(String label) {
        new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.cssSelector("div.select2-drop-active div.select2-result-label")));
        List<WebElement> options = this.driver.findElements(By.cssSelector("div.select2-drop-active div.select2-result-label"));
        for (WebElement we : options) {
            if (we.getText().equalsIgnoreCase(label)) {
                logger.trace("Clicking " + label);
                we.click();
                break;
            }
        }
    }

    private void updateTextField(String cssId, String systemName, String bodyName, String fieldName, String newValue, String screenshotFilename) throws IOException {
        this.updateTextField(this.driver.findElement(By.id(cssId)), systemName, bodyName, fieldName, newValue, screenshotFilename);
    }

    private void updateTextField(WebElement input, String systemName, String bodyName, String fieldName, String newValue, String screenshotFilename) throws IOException {
        String historyEntry = this.history.lookup(systemName, bodyName, fieldName);

        if (historyEntry != null) {
            logger.trace("Skipping " + fieldName + "=" + newValue + " of body " + bodyName + " in system " + systemName + ". Entry from history: " + historyEntry);
        } else {
            logger.trace("Setting " + fieldName + "=" + newValue + " of body " + bodyName + " in system " + systemName);

            String oldValue = StringUtils.isEmpty(input.getAttribute("value")) ? null : input.getAttribute("value");

            if (oldValue == null) {
                if (newValue == null) {
                    // No change
                } else {
                    // Set
                    this.history.set(systemName, bodyName, fieldName, newValue, screenshotFilename);
                    input.click();
                    input.sendKeys(newValue);
                }
            } else {
                if (newValue == null) {
                    // Erase
                    this.history.erase(systemName, bodyName, fieldName, oldValue, screenshotFilename);
                    input.click();
                    input.sendKeys(Keys.chord(Keys.LEFT_CONTROL, "a"));
                    input.sendKeys(Keys.BACK_SPACE);
                } else if (newValue.equals(oldValue)) {
                    // No change
                } else {
                    // Change
                    this.history.change(systemName, bodyName, fieldName, oldValue, newValue, screenshotFilename);
                    input.click();
                    input.sendKeys(Keys.chord(Keys.LEFT_CONTROL, "a"));
                    input.sendKeys(Keys.BACK_SPACE);
                    input.sendKeys(newValue);
                }
            }
        }
    }

    private void updateNumericField(String cssId, String systemName, String bodyName, String fieldName, BigDecimal newValue, String screenshotFilename) throws IOException {
        this.updateNumericField(this.driver.findElement(By.id(cssId)), systemName, bodyName, fieldName, newValue, screenshotFilename);
    }

    private void updateNumericField(WebElement input, String systemName, String bodyName, String fieldName, BigDecimal newValue, String screenshotFilename) throws IOException {
        String historyEntry = this.history.lookup(systemName, bodyName, fieldName);

        if (historyEntry != null) {
            logger.trace("Skipping " + fieldName + "=" + newValue + " of body " + bodyName + " in system " + systemName + ". Entry from history: " + historyEntry);
        } else {
            logger.trace("Setting " + fieldName + "=" + newValue + " of body " + bodyName + " in system " + systemName);

            BigDecimal oldValue = StringUtils.isEmpty(input.getAttribute("value")) ? null : new BigDecimal(input.getAttribute("value"));

            if (oldValue == null) {
                if (newValue == null) {
                    // No change
                } else {
                    // Set
                    this.history.set(systemName, bodyName, fieldName, String.valueOf(newValue), screenshotFilename);
                    input.click();
                    input.sendKeys(String.valueOf(newValue));
                }
            } else {
                if (newValue == null) {
                    // Erase
                    this.history.erase(systemName, bodyName, fieldName, String.valueOf(oldValue), screenshotFilename);
                    input.click();
                    input.sendKeys(Keys.chord(Keys.LEFT_CONTROL, "a"));
                    input.sendKeys(Keys.BACK_SPACE);
                } else if (newValue.compareTo(oldValue) == 0) {
                    // No change
                } else {
                    // Change
                    this.history.change(systemName, bodyName, fieldName, String.valueOf(oldValue), String.valueOf(newValue), screenshotFilename);
                    input.click();
                    input.sendKeys(Keys.chord(Keys.LEFT_CONTROL, "a"));
                    input.sendKeys(Keys.BACK_SPACE);
                    input.sendKeys(String.valueOf(newValue));
                }
            }
        }
    }

    /**
     * Clicks on the 'Systems' link in the header, enters the given <code>systemName</code> into the search
     * field, waits for the search result, and clicks on the link which has exactly the <code>systemName</code>
     * as text.
     *
     * <p>
     * If successful we should be on the system detail page after this method.
     * </p>
     *
     * @param systemName
     * @throws SystemNotFoundException
     */
    private void openSystemPage(String systemName) throws SystemNotFoundException {
        // Go to systems page
        logger.trace("Clicking on 'Systems' link in the header");
        this.driver.findElement(By.linkText("Systems")).click();

        // Search the current system
        logger.trace("Submitting search for system '" + systemName + "'");
        WebElement systemSearchInput = this.driver.findElement(By.name("SystemSearch[name]"));
        systemSearchInput.sendKeys(systemName);
        systemSearchInput.sendKeys(Keys.ENTER);

        // Wait for the search result to display the current system and click on it
        new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.cssSelector("div.system-index table.table tbody a")));
        List<WebElement> allLinks = this.driver.findElements(By.cssSelector("div.system-index table.table tbody a"));
        List<WebElement> systemLinks = new ArrayList<>(1);
        for (WebElement link : allLinks) {
            if (link.getText().equalsIgnoreCase(systemName)) {
                systemLinks.add(link);
            }
        }
        if (systemLinks.isEmpty()) {
            String msg = "System '" + systemName + "' not found on ROSS. Found " + allLinks.size() + " others: " + linksToStrings(allLinks);
            logger.warn(msg);
            throw new SystemNotFoundException(msg);
        } else if (systemLinks.size() > 1) {
            throw new RuntimeException("Multiple links to system '" + systemName + "' found: " + linksToStrings(systemLinks));
        } else {
            String href = systemLinks.get(0).getAttribute("href");
            String nameOnRoss = systemLinks.get(0).getText();
            logger.debug("Clicking on link to system '" + systemName + "': " + href + " (" + nameOnRoss + ")");
            systemLinks.get(0).click();
            new WebDriverWait(this.driver, 30).until(ExpectedConditions.titleContains("Update System: " + nameOnRoss + " - ROSS"));

            // Scroll to bodies section
            for (WebElement panelHeading : this.driver.findElements(By.cssSelector("div.panel-heading"))) {
                if (panelHeading.getText().equalsIgnoreCase("Bodies in system " + systemName)) {
                    new Actions(driver).moveToElement(driver.findElement(By.cssSelector("footer.footer")), 0, 0).build().perform();
                    new Actions(driver).moveToElement(panelHeading, 0, 0).build().perform();
                    break;
                }
            }
        }
    }

    private void openOrCreateBodyPage(ScannedBodyInfo scannedBodyInfo) throws IOException {
        String bodyName = fixBodyName(scannedBodyInfo.getSystemName(), scannedBodyInfo.getBodyName());
        List<WebElement> allLinks = this.driver.findElements(By.tagName("a"));
        List<WebElement> bodyLinks = new ArrayList<>(1);
        for (WebElement link : allLinks) {
            if (link.getText().equalsIgnoreCase(bodyName)) {
                bodyLinks.add(link);
            }
        }
        if (bodyLinks.isEmpty()) {
            this.createBody(scannedBodyInfo);
        } else if (bodyLinks.size() > 1) {
            throw new RuntimeException("Multiple links to body '" + bodyName + "' found: " + linksToStrings(bodyLinks));
        } else {
            String href = bodyLinks.get(0).getAttribute("href");
            String nameOnRoss = bodyLinks.get(0).getText();
            logger.debug("Clicking on link to body '" + bodyName + "': " + href + " (" + nameOnRoss + ")");
            bodyLinks.get(0).click();
            new WebDriverWait(this.driver, 60).until(ExpectedConditions.titleContains("Update Body: " + nameOnRoss + " - ROSS"));
        }
    }

    private void createBody(ScannedBodyInfo scannedBodyInfo) throws IOException {
        if (scannedBodyInfo.getBodyGroup() == null) {
            throw new IllegalArgumentException("scannedBodyInfo has no bodyGroup");
        } else if (scannedBodyInfo.getBodyGroup() == BodyInfo.GROUP_RINGS) {
            throw new IllegalArgumentException("Cannot create new body of type RINGS. Corresponding STAR or PLANET must be created first.");
        } else {
            final String systemName = scannedBodyInfo.getSystemName();
            final String bodyName = fixBodyName(systemName, scannedBodyInfo.getBodyName());
            final String bodyGroup = scannedBodyInfo.getBodyGroup().getName();
            final String filename = scannedBodyInfo.getScreenshotFilename();

            BigDecimal distanceLsInteger = scannedBodyInfo.getDistanceLs() == null ? null : new BigDecimal(scannedBodyInfo.getDistanceLs().intValue());

            logger.info("Creating new body '" + bodyName + "' in system '" + systemName + "'");

            logger.trace("Clicking on 'Add new body to system " + systemName + "' link");
            this.driver.findElement(By.linkText("Add new body to system " + systemName)).click();

            this.updateTextField("bodyform-name", systemName, bodyName, EddbHistory.FIELD_BODY_NAME, bodyName, filename);
            this.updateSearchableDropdown("s2id_bodyform-group_id", null, systemName, bodyName, EddbHistory.FIELD_BODY_GROUP, bodyGroup, filename);
            this.updateNumericField("bodyform-distance_to_spawn", systemName, bodyName, EddbHistory.FIELD_DISTANCE_FROM_ARRIVAL, distanceLsInteger, filename);

            logger.trace("Submitting 'Add new body to system " + systemName + "' form");
            this.submitBodyPage();
        }
    }

    private void submitBodyPage() {
        logger.trace("Submitting body form");
        this.driver.findElement(By.id("bodyform-name")).submit();
    }

    public boolean isScreenshotFinished(String filename) throws IOException {
        return this.history.isScreenshotFinished(filename);
    }

    public boolean isScreenshotFailed(String filename) throws IOException {
        return this.history.isScreenshotFailed(filename);
    }

    /**
     * TODO Check scanner fix, maybe add two variants (scanned vs beautified)
     *
     * @deprecated Should already be fixed by the scanner?
     */
    @Deprecated
    private static String fixBodyName(String systemName, String bodyName) {
        if (bodyName.toLowerCase().startsWith(systemName.toLowerCase() + " ")) {
            return systemName + bodyName.substring(systemName.length());
        } else {
            return bodyName;
        }
    }

    private static List<String> linksToStrings(List<WebElement> links) {
        List<String> result = new ArrayList<>(links.size());
        for (WebElement link : links) {
            String href = link.getAttribute("href");
            String text = link.getText();
            result.add("<a href=\"" + href + "\">" + text + "</a>");
        }
        return result;
    }

}