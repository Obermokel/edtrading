package borg.edtrading.cfg;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * Constants
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface Constants {

    /*
     * ==== LEARNING ====
     */

    boolean LEARN_Z_VS_z = true;

    /**
     * Auto-learn if a 0 has been detected as O or vice versa? Should usually be set to false.
     * If set to false C, D or any other char detected as O or 0 will still be auto-learned.
     */
    boolean LEARN_0_VS_O = true;

    /*
     * ==== DIRECTORIES ====
     */

    File TEMP_DIR = new File(System.getProperty("user.home"), "edtmp");

    /**
     * Where screenshots are saved by Fraps, PlayClaw etc
     */
    File SCREENSHOTS_DIR = new File(System.getProperty("user.home"), "Google Drive\\Game Screenshots\\elitedangerous64");

    /**
     * Where screenshots showing surface mats are stored
     */
    File SURFACE_MATS_DIR = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\Surface Materials");

    String SURFACE_MATS_SUBDIR = "_FULLSYS_";

    /**
     * Where screenshots showing factions are stored
     */
    File FACTION_SCREENSHOTS_DIR = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\Factions");

    /**
     * Where this program stores its data
     */
    File EDTRADING_BASE_DIR = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\edtrading");
    File TEMPLATES_DIR = new File(EDTRADING_BASE_DIR, "Templates");
    File AUTO_LEARNED_DIR = new File(TEMP_DIR, "Auto-Learned");
    File UNKNOWN_DIR = new File(TEMP_DIR, "Unknown");
    File JOURNAL_DIR = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\Journal");

    /*
     * ==== OTHER ====
     */

    Set<String> SCOOPABLE_SPECTRAL_CLASSES = new HashSet<>(Arrays.asList("O", "B", "A", "F", "G", "K", "K_RedGiant", "K_OrangeGiant", "M", "M_RedGiant", "M_OrangeGiant"));

}
