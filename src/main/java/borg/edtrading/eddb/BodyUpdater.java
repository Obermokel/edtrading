package borg.edtrading.eddb;

import borg.edtrading.data.BodyInfo;
import borg.edtrading.data.ScannedBodyInfo;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.io.Closeable;
import java.io.IOException;
import java.math.BigDecimal;

/**
 * BodyUpdater
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class BodyUpdater implements Closeable {

    static final Logger logger = LogManager.getLogger(BodyUpdater.class);

    private WebDriver driver = null;

    public static void main(String[] args) throws IOException, InterruptedException {
        BodyUpdater bodyUpdater = new BodyUpdater("Mokel DeLorean", "foobar");

        try {
            bodyUpdater.updateBody(null);
        } finally {
            bodyUpdater.close();
        }
    }

    public BodyUpdater(String username, String password) {
        //this.driver = new ChromeDriver();
        this.driver = new FirefoxDriver();
        //this.driver = new HtmlUnitDriver(false);

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

    public void updateBody(ScannedBodyInfo scannedBodyInfo) throws InterruptedException {
        if (StringUtils.isEmpty(scannedBodyInfo.getSystemName())) {
            throw new IllegalArgumentException("scannedBodyInfo has no systemName");
        } else if (StringUtils.isEmpty(scannedBodyInfo.getBodyName())) {
            throw new IllegalArgumentException("scannedBodyInfo has no bodyName");
        } else {
            this.driver.findElement(By.linkText("Systems")).click();

            WebElement systemSearchInput = this.driver.findElement(By.name("SystemSearch[name]"));
            systemSearchInput.sendKeys(scannedBodyInfo.getSystemName());
            systemSearchInput.sendKeys(Keys.ENTER);

            try {
                new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.linkText(scannedBodyInfo.getSystemName()))).click();
            } catch (TimeoutException e) {
                throw new RuntimeException("System '" + scannedBodyInfo.getSystemName() + "' not found on ROSS");
            }

            try {
                new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.linkText(scannedBodyInfo.getBodyName()))).click();
            } catch (TimeoutException e) {
                this.createBody(scannedBodyInfo);
            }

            logger.info("Updating body '" + scannedBodyInfo.getBodyName() + "' in system '" + scannedBodyInfo.getSystemName() + "'");

            if (scannedBodyInfo.getBodyType() != null) {
                this.driver.findElement(By.id("s2id_bodyform-type_id")).click();
                WebElement searchInput = new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.cssSelector("#select2-drop input")));
                searchInput.click();
                searchInput.sendKeys(scannedBodyInfo.getBodyType().getName());
                searchInput.sendKeys(Keys.ENTER);
            }

            if (scannedBodyInfo.getEarthMasses() != null) {
                this.driver.findElement(By.id("bodyform-earth_masses")).sendKeys(String.valueOf(scannedBodyInfo.getEarthMasses()));
            }

            if (scannedBodyInfo.getRadiusKm() != null) {
                this.driver.findElement(By.id("bodyform-radius")).sendKeys(String.valueOf(scannedBodyInfo.getRadiusKm()));
            }

            if (scannedBodyInfo.getGravityG() != null) {
                this.driver.findElement(By.id("bodyform-gravity")).sendKeys(String.valueOf(scannedBodyInfo.getGravityG()));
            }

            if (scannedBodyInfo.getSurfaceTempK() != null) {
                this.driver.findElement(By.id("bodyform-surface_temperature")).sendKeys(String.valueOf(scannedBodyInfo.getSurfaceTempK()));
            }

            if (scannedBodyInfo.getVolcanism() != null) {
                new Actions(driver).moveToElement(driver.findElement(By.cssSelector("footer.footer")), 0, 0).build().perform();
                new Actions(driver).moveToElement(driver.findElement(By.id("bodyform-surface_temperature")), 0, 0).build().perform();
                this.driver.findElement(By.id("s2id_bodyform-volcanism_type_id")).click();
                WebElement searchInput = new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.cssSelector("#select2-drop input")));
                searchInput.click();
                searchInput.sendKeys(scannedBodyInfo.getVolcanism().getName());
                searchInput.sendKeys(Keys.ENTER);
            }

            if (scannedBodyInfo.getAtmosphereType() != null) {
                new Actions(driver).moveToElement(driver.findElement(By.cssSelector("footer.footer")), 0, 0).build().perform();
                new Actions(driver).moveToElement(driver.findElement(By.id("bodyform-surface_temperature")), 0, 0).build().perform();
                this.driver.findElement(By.id("s2id_bodyform-atmosphere_type_id")).click();
                WebElement searchInput = new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.cssSelector("#select2-drop input")));
                searchInput.click();
                searchInput.sendKeys(scannedBodyInfo.getAtmosphereType().getName());
                searchInput.sendKeys(Keys.ENTER);
            }

            if (scannedBodyInfo.getComposition() != null) {
                int index = 0;
                for (BodyInfo material : scannedBodyInfo.getComposition().keySet()) {
                    BigDecimal percent = scannedBodyInfo.getComposition().get(material);
                    if (percent != null) {
                        this.driver.findElement(By.id("bodysolidcomposition-" + index + "-share")).sendKeys(String.valueOf(percent));
                    }
                    this.driver.findElement(By.id("s2id_bodysolidcomposition-" + index + "-solid_component_id")).click();
                    WebElement searchInput = new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.cssSelector("#select2-drop input")));
                    searchInput.click();
                    searchInput.sendKeys(material.getName());
                    searchInput.sendKeys(Keys.ENTER);
                    index++;
                }
            }

            if (scannedBodyInfo.getOrbitalPeriodD() != null) {
                this.driver.findElement(By.id("bodyform-orbital_period")).sendKeys(String.valueOf(scannedBodyInfo.getOrbitalPeriodD()));
            }

            if (scannedBodyInfo.getSemiMajorAxisAU() != null) {
                this.driver.findElement(By.id("bodyform-semi_major_axis")).sendKeys(String.valueOf(scannedBodyInfo.getSemiMajorAxisAU()));
            }

            if (scannedBodyInfo.getOrbitalEccentricity() != null) {
                this.driver.findElement(By.id("bodyform-orbital_eccentricity")).sendKeys(String.valueOf(scannedBodyInfo.getOrbitalEccentricity()));
            }

            if (scannedBodyInfo.getOrbitalInclinationDeg() != null) {
                this.driver.findElement(By.id("bodyform-orbital_inclination")).sendKeys(String.valueOf(scannedBodyInfo.getOrbitalInclinationDeg()));
            }

            if (scannedBodyInfo.getArgOfPeriapsisDeg() != null) {
                this.driver.findElement(By.id("bodyform-arg_of_periapsis")).sendKeys(String.valueOf(scannedBodyInfo.getArgOfPeriapsisDeg()));
            }

            if (scannedBodyInfo.getRotationalPeriodD() != null) {
                this.driver.findElement(By.id("bodyform-rotational_period")).sendKeys(String.valueOf(scannedBodyInfo.getRotationalPeriodD()));
            }

            //            if (scannedBodyInfo.getTidallyLocked() != null) {
            //                new Actions(driver).moveToElement(driver.findElement(By.cssSelector("footer.footer")), 0, 0).build().perform();
            //                new Actions(driver).moveToElement(driver.findElement(By.id("bodysolidcomposition-0-share")), 0, 0).build().perform();
            //                this.driver.findElement(By.id("s2id_bodyform-is_rotational_period_tidally_locked")).click();
            //                WebElement searchInput = new WebDriverWait(this.driver, 30).until(ExpectedConditions.elementToBeClickable(By.cssSelector("#select2-drop input")));
            //                searchInput.click();
            //                searchInput.sendKeys(scannedBodyInfo.getTidallyLocked().booleanValue() ? "Yes" : "No");
            //                searchInput.sendKeys(Keys.ENTER);
            //            }

            if (scannedBodyInfo.getAxialTiltDeg() != null) {
                this.driver.findElement(By.id("bodyform-axis_tilt")).sendKeys(String.valueOf(scannedBodyInfo.getAxialTiltDeg()));
            }

            Thread.sleep(30000L);
            this.driver.findElement(By.id("bodyform-name")).submit();
            throw new RuntimeException("testing");
        }
    }

    private void createBody(ScannedBodyInfo scannedBodyInfo) {
        logger.info("Creating new body '" + scannedBodyInfo.getBodyName() + "' in system '" + scannedBodyInfo.getSystemName() + "'");

        this.driver.findElement(By.linkText("Add new body to system " + scannedBodyInfo.getSystemName())).click();

        this.driver.findElement(By.id("bodyform-name")).sendKeys(scannedBodyInfo.getBodyName());
        this.driver.findElement(By.id("s2id_bodyform-group_id")).click(); // Body type, i.e. star/belt/planet
        this.driver.findElement(By.id("s2id_autogen1_search")).sendKeys("Planet");
        this.driver.findElement(By.id("s2id_autogen1_search")).sendKeys(Keys.ENTER);

        if (scannedBodyInfo.getDistanceLs() != null) {
            this.driver.findElement(By.id("bodyform-distance_to_spawn")).sendKeys(String.valueOf(scannedBodyInfo.getDistanceLs().intValue()));
        }

        this.driver.findElement(By.id("bodyform-name")).submit();
    }

}
