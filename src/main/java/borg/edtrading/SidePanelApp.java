package borg.edtrading;

import borg.edtrading.gui.InventoryPanel;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.sidepanel.Inventory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.FlowLayout;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.swing.JFrame;

/**
 * SidePanelApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SidePanelApp {

    static final Logger logger = LogManager.getLogger(SidePanelApp.class);

    public static void main(String[] args) throws IOException {
        String commander = "Mokel DeLorean";
        Path journalDir = Paths.get(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\Journal"); // TODO Use live dir

        // Create the reader thread which will initially read the entire journal
        JournalReaderThread journalReaderThread = new JournalReaderThread(journalDir);

        // Create and register the journal listeners
        Inventory inventory = Inventory.load(commander);
        journalReaderThread.addListener(inventory);

        // Init the reader from existing files, then start to watch for changes
        journalReaderThread.init();
        journalReaderThread.start();

        // Create all panels and register them to their corresponding journal listener
        InventoryPanel inventoryPanel = new InventoryPanel(inventory);
        inventory.addListener(inventoryPanel);

        // Construct the window with all panels
        JFrame frame = new JFrame("SidePanel");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLayout(new FlowLayout());
        frame.add(inventoryPanel);
        frame.pack();
        frame.setVisible(true);
    }

}
