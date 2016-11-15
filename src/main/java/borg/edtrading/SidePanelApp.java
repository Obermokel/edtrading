package borg.edtrading;

import borg.edtrading.gui.InventoryPanel;
import borg.edtrading.gui.RawJournalLogPanel;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.sidepanel.Inventory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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

        // Create all panels and register them to their corresponding feed
        InventoryPanel inventoryPanel = new InventoryPanel(inventory);
        inventory.addListener(inventoryPanel);
        RawJournalLogPanel rawJournalLogPanel = new RawJournalLogPanel();
        journalReaderThread.addListener(rawJournalLogPanel);

        // Construct the window with all panels
        JFrame frame = new JFrame("SidePanel");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridx = 0;
        gbc.gridy = 0;
        frame.add(inventoryPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        frame.add(rawJournalLogPanel, gbc);
        frame.pack();
        frame.setVisible(true);
    }

}
