package borg.edtrading.eddb;

import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.ListIterator;

/**
 * EddbHistory
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class EddbHistory {

    static final Logger logger = LogManager.getLogger(EddbHistory.class);

    public static final String ACTION_SET = "SET";
    public static final String ACTION_TOGGLE = "TOGGLE";
    public static final String ACTION_CHANGE = "CHANGE";
    public static final String ACTION_ERASE = "ERASE";

    public static final String FIELD_BODY_NAME = "bodyName";
    public static final String FIELD_BODY_GROUP = "bodyGroup";
    public static final String FIELD_DISTANCE_FROM_ARRIVAL = "distanceFromArrival";
    public static final String FIELD_BODY_TYPE = "bodyType";
    public static final String FIELD_EARTH_MASSES = "earthMasses";
    public static final String FIELD_RADIUS = "radius";
    public static final String FIELD_GRAVITY = "gravity";
    public static final String FIELD_SURFACE_TEMP = "surfaceTemp";
    public static final String FIELD_VOLCANISM = "volcanism";
    public static final String FIELD_ATMOSPHERE_TYPE = "atmosphereType";
    public static final String FIELD_SOLID_COMPOSITION_SHARE = "solidCompositionShare";
    public static final String FIELD_SOLID_COMPOSITION_NAME = "solidCompositionName";
    public static final String FIELD_ORBITAL_PERIOD = "orbitalPeriod";
    public static final String FIELD_SEMI_MAJOR_AXIS = "semiMajorAxis";
    public static final String FIELD_ORBITAL_ECCENTRICITY = "orbitalEccentricity";
    public static final String FIELD_ORBITAL_INCLINATION = "orbitalInclination";
    public static final String FIELD_ARG_OF_PERIAPSIS = "argOfPeriapsis";
    public static final String FIELD_ROTATIONAL_PERIOD = "rotationalPeriod";
    public static final String FIELD_TIDALLY_LOCKED = "tidallyLocked";
    public static final String FIELD_AXIAL_TILT = "axialTilt";
    public static final String FIELD_PLANET_MATERIALS_NAME = "planetMaterialsName";
    public static final String FIELD_PLANET_MATERIALS_SHARE = "planetMaterialsShare";

    private final File logfile;
    private final List<String> history;

    private EddbHistory() throws IOException {
        this.logfile = new File(System.getProperty("user.home"), "eddbHistory.log");
        this.history = this.readHistory(this.logfile);
    }

    private List<String> readHistory(File logfile) throws IOException {
        return logfile.exists() ? FileUtils.readLines(logfile, "UTF-8") : new ArrayList<>();
    }

    public static EddbHistory load() throws IOException {
        return new EddbHistory();
    }

    public String lookup(String systemName, String bodyName, String fieldName) {
        final String searchFor = ("|" + systemName + "|" + bodyName + "|" + fieldName + "|").toLowerCase();
        for (String entry : this.history) {
            if (entry.toLowerCase().contains(searchFor)) {
                return searchFor;
            }
        }
        return null;
    }

    public int removeAll(String systemName, String bodyName) throws IOException {
        final String searchFor = ("|" + systemName + "|" + bodyName + "|").toLowerCase();
        int count = 0;
        ListIterator<String> it = this.history.listIterator();
        while (it.hasNext()) {
            String entry = it.next();
            if (entry.toLowerCase().contains(searchFor)) {
                it.remove();
                count++;
            }
        }
        FileUtils.write(this.logfile, "", "UTF-8", false);
        for (String remainingEntry : this.history) {
            FileUtils.write(this.logfile, remainingEntry + "\n", "UTF-8", true);
        }
        return count;
    }

    public boolean isScreenshotFinished(String screenshotFilename) throws IOException {
        final String searchFor = ("|SCREENSHOT|FINISHED|null|null|" + screenshotFilename).toLowerCase();
        for (String entry : this.history) {
            if (entry.toLowerCase().contains(searchFor)) {
                return true;
            }
        }
        return false;
    }

    public void addScreenshotFinished(String systemName, String bodyName, String screenshotFilename) throws IOException {
        this.log(systemName, bodyName, "SCREENSHOT", "FINISHED", null, null, screenshotFilename);
    }

    public void set(String systemName, String bodyName, String fieldName, String newValue, String screenshotFilename) throws IOException {
        this.log(systemName, bodyName, fieldName, ACTION_SET, null, newValue, screenshotFilename);
    }

    public void toggle(String systemName, String bodyName, String fieldName, boolean turnOn, String screenshotFilename) throws IOException {
        this.log(systemName, bodyName, fieldName, ACTION_TOGGLE, turnOn ? "off" : "on", turnOn ? "on" : "off", screenshotFilename);
    }

    public void change(String systemName, String bodyName, String fieldName, String oldValue, String newValue, String screenshotFilename) throws IOException {
        this.log(systemName, bodyName, fieldName, ACTION_CHANGE, oldValue, newValue, screenshotFilename);
    }

    public void erase(String systemName, String bodyName, String fieldName, String oldValue, String screenshotFilename) throws IOException {
        this.log(systemName, bodyName, fieldName, ACTION_ERASE, oldValue, null, screenshotFilename);
    }

    private void log(String systemName, String bodyName, String fieldName, String action, String oldValue, String newValue, String screenshotFilename) throws IOException {
        final String timestamp = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date());
        //@formatter:off
        StringBuilder line = new StringBuilder(timestamp)
                .append("|").append(systemName)
                .append("|").append(bodyName)
                .append("|").append(fieldName)
                .append("|").append(action)
                .append("|").append(oldValue)
                .append("|").append(newValue)
                .append("|").append(screenshotFilename);
        //@formatter:on
        this.history.add(line.toString());
        FileUtils.write(this.logfile, line + "\n", "UTF-8", true);
    }

}
