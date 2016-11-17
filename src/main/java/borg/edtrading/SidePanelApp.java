package borg.edtrading;

import borg.edtrading.gui.InventoryPanel;
import borg.edtrading.gui.JournalLogPanel;
import borg.edtrading.gui.StatusPanel;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.sidepanel.Inventory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BorderLayout;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;

/**
 * SidePanelApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SidePanelApp {

    static final Logger logger = LogManager.getLogger(SidePanelApp.class);

    public static void main(String[] args) throws IOException {
        String commander = "Mokel DeLorean";
        Path journalDir = Paths.get(System.getProperty("user.home"));
        if ("boris".equalsIgnoreCase(journalDir.getFileName().toString())) {
            journalDir = journalDir.resolve("Saved Games\\Frontier Developments\\Elite Dangerous");
        } else {
            journalDir = journalDir.resolve("Google Drive\\Elite Dangerous\\Journal");
        }

        //        try {
        //            UIManager.setLookAndFeel("com.jtattoo.plaf.noire.NoireLookAndFeel");
        //        } catch (Exception e) {
        //            e.printStackTrace();
        //        }

        // Create the reader thread
        JournalReaderThread journalReaderThread = new JournalReaderThread(journalDir);

        // Create and register the journal listeners
        Inventory inventory = Inventory.load(commander);
        journalReaderThread.addListener(inventory);
        JournalLogPanel journalLogPanel = new JournalLogPanel(journalReaderThread);

        // Init the reader from existing files, then start to watch for changes
        journalReaderThread.init();
        journalReaderThread.start();

        // Create all panels
        StatusPanel statusPanel = new StatusPanel(inventory);
        InventoryPanel inventoryPanel = new InventoryPanel(inventory);

        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Inventory", inventoryPanel);

        // Construct the window with all panels
        JFrame frame = new JFrame("SidePanel");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLayout(new BorderLayout());
        frame.add(statusPanel, BorderLayout.NORTH);
        frame.add(tabbedPane, BorderLayout.CENTER);
        frame.add(new JScrollPane(journalLogPanel), BorderLayout.SOUTH);
        //frame.pack();
        frame.setSize(1280, 720);
        frame.setLocation(400, 200);
        frame.setVisible(true);

        inventoryPanel.setDividerLocation(0.8);
    }

}
