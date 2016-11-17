package borg.edtrading;

import borg.edtrading.gui.InventoryPanel;
import borg.edtrading.gui.JournalLogPanel;
import borg.edtrading.gui.StatusPanel;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.sidepanel.GameSession;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.ShipModuleList;
import borg.edtrading.sidepanel.TravelHistory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BorderLayout;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
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
public class SidePanelApp implements WindowListener {

    static final Logger logger = LogManager.getLogger(SidePanelApp.class);

    private JournalReaderThread journalReaderThread = null;
    private GameSession gameSession = null;
    private TravelHistory travelHistory = null;
    private Inventory inventory = null;

    public static void main(String[] args) throws IOException {
        new SidePanelApp().start();
    }

    private void start() throws IOException {
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
        journalReaderThread = new JournalReaderThread(journalDir);

        // Create and register the journal listeners
        gameSession = new GameSession(journalReaderThread);
        travelHistory = new TravelHistory(journalReaderThread, gameSession);
        inventory = new Inventory(journalReaderThread, gameSession);
        new ShipModuleList(gameSession);

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
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.addWindowListener(this);
        frame.setLayout(new BorderLayout());
        frame.add(statusPanel, BorderLayout.NORTH);
        frame.add(tabbedPane, BorderLayout.CENTER);
        frame.add(new JScrollPane(journalLogPanel), BorderLayout.SOUTH);
        //frame.pack();
        frame.setSize(1800, 900);
        frame.setLocation(10, 10);
        frame.setVisible(true);

        inventoryPanel.setDividerLocation(0.8);
    }

    @Override
    public void windowOpened(WindowEvent e) {
        // Do nothing
    }

    @Override
    public void windowClosing(WindowEvent e) {
        journalReaderThread.interrupt();

        try {
            inventory.save();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    @Override
    public void windowClosed(WindowEvent e) {
        while (journalReaderThread.isAlive()) {
            try {
                Thread.sleep(10);
            } catch (InterruptedException ex) {
                break;
            }
        }

        System.exit(0);
    }

    @Override
    public void windowIconified(WindowEvent e) {
        // Do nothing
    }

    @Override
    public void windowDeiconified(WindowEvent e) {
        // Do nothing
    }

    @Override
    public void windowActivated(WindowEvent e) {
        // Do nothing
    }

    @Override
    public void windowDeactivated(WindowEvent e) {
        // Do nothing
    }

}
