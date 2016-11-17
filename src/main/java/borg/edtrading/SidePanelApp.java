package borg.edtrading;

import borg.edtrading.gui.InventoryPanel;
import borg.edtrading.gui.JournalLogPanel;
import borg.edtrading.gui.StatusPanel;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.sidepanel.GameSession;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.TravelHistory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BorderLayout;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.UIManager;

/**
 * SidePanelApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SidePanelApp {

    static final Logger logger = LogManager.getLogger(SidePanelApp.class);

    public static void main(String[] args) throws IOException {
        Path journalDir = Paths.get(System.getProperty("user.home"));
        if (!"Guenther".equalsIgnoreCase(journalDir.getFileName().toString())) {
            journalDir = journalDir.resolve("Saved Games\\Frontier Developments\\Elite Dangerous");
        } else {
            journalDir = journalDir.resolve("Google Drive\\Elite Dangerous\\Journal");
        }

        try {
            UIManager.setLookAndFeel("com.jtattoo.plaf.noire.NoireLookAndFeel");
        } catch (Exception e) {
            e.printStackTrace();
        }

        // Create the reader thread
        JournalReaderThread journalReaderThread = new JournalReaderThread(journalDir);

        // Create and register the journal listeners
        GameSession gameSession = new GameSession(journalReaderThread);
        TravelHistory travelHistory = new TravelHistory(journalReaderThread, gameSession);
        Inventory inventory = new Inventory(journalReaderThread, gameSession);

        // Init the reader from existing files, then start to watch for changes
        journalReaderThread.init();
        journalReaderThread.start();

        // Create all panels
        JournalLogPanel journalLogPanel = new JournalLogPanel(journalReaderThread);
        StatusPanel statusPanel = new StatusPanel(gameSession, travelHistory, inventory);
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
