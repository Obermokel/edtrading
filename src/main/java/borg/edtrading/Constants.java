package borg.edtrading;

import java.io.File;

/**
 * Constants
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface Constants {

    File TEMP_DIR = new File(System.getProperty("user.home"), "edtmp");

    /**
     * Where screenshots are saved by Fraps, PlayClaw etc
     */
    File SCREENSHOTS_DIR = new File(System.getProperty("user.home"), "Google Drive\\Game Screenshots\\elitedangerous64");

    /**
     * Where screenshots showing surface mats are stored
     */
    File SURFACE_MATS_DIR = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\Surface Materials");

    /**
     * Where this program stores its data
     */
    File EDTRADING_BASE_DIR = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\edtrading");
    File TEMPLATES_DIR = new File(EDTRADING_BASE_DIR, "Templates");
    File AUTO_LEARNED_DIR = new File(TEMP_DIR, "Auto-Learned");
    File UNKNOWN_DIR = new File(TEMP_DIR, "Unknown");
    File INVENTORY_SCREENSHOTS_DIR = new File(EDTRADING_BASE_DIR, "Inventory Screenshots");

    File RARES_FILE = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\edrares.csv");
    File COMMODITIES_FILE = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\commodities.json");
    //File SYSTEMS_FILE = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\systems.json");
    File SYSTEMS_FILE = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\systems_populated.json");
    File BODIES_FILE = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\bodies.json");
    File STATIONS_FILE = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\stations.json");
    File LISTINGS_FILE = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\listings.csv");

    /**
     * Auto-learn if a 0 has been detected as O or vice versa? Should usually be set to false.
     * If set to false C, D or any other char detected as O or 0 will still be auto-learned.
     */
    boolean LEARN_0_VS_O = false;

}
