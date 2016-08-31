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
    File INVENTORY_SCREENSHOTS_DIR = new File(EDTRADING_BASE_DIR, "Inventory Screenshots");

}
