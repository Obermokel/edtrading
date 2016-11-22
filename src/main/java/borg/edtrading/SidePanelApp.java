package borg.edtrading;

import borg.edtrading.gui.InventoryPanel;
import borg.edtrading.gui.JournalLogPanel;
import borg.edtrading.gui.ScansPanel;
import borg.edtrading.gui.ShipyardPanel;
import borg.edtrading.gui.StatusPanel;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.sidepanel.GameSession;
import borg.edtrading.sidepanel.GameSessionListener;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.ShipLoadout;
import borg.edtrading.sidepanel.ShipModule;
import borg.edtrading.sidepanel.ShipModuleList;
import borg.edtrading.sidepanel.TravelHistory;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.UIManager;

/**
 * SidePanelApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class SidePanelApp implements WindowListener, GameSessionListener {

    static final Logger logger = LogManager.getLogger(SidePanelApp.class);

    private JFrame frame = null;
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

        frame = new JFrame("SidePanel");

        // Create the reader thread
        journalReaderThread = new JournalReaderThread(journalDir);

        // Create and register the journal listeners
        gameSession = new GameSession(journalReaderThread);
        gameSession.addListener(this);
        travelHistory = new TravelHistory(journalReaderThread, gameSession);
        inventory = new Inventory(journalReaderThread, gameSession);
        new ShipModuleList(gameSession);

        // Init the reader from existing files, then start to watch for changes
        journalReaderThread.init();
        journalReaderThread.start();

        //        LinkedList<VisitedSystem> visitedSystems = this.travelHistory.getVisitedSystems();
        //        for (int i = visitedSystems.size() - 1; i >= 0; i--) {
        //            VisitedSystem vs = visitedSystems.get(i);
        //            if (vs.getRemainingPayout() != 0) {
        //                logger.debug(String.format(Locale.US, "[%s] SYSTEM %-40s %d", new SimpleDateFormat("dd. MMM HH:mm").format(vs.getTimestamp()), vs.getSystemName(), vs.getRemainingPayout()));
        //            }
        //            for (ScannedBody sb : vs.getScannedBodies()) {
        //                if (sb.getRemainingBasePayout() != 0) {
        //                    logger.debug(String.format(Locale.US, "[%s] %40s %-40s %d", new SimpleDateFormat("dd. MMM HH:mm").format(sb.getTimestamp()), sb.getBodyClass(), sb.getBodyName(), sb.getRemainingBasePayout()));
        //                }
        //            }
        //        }

        // Create all panels
        JournalLogPanel journalLogPanel = new JournalLogPanel(journalReaderThread);
        StatusPanel statusPanel = new StatusPanel(gameSession, travelHistory, inventory);
        InventoryPanel inventoryPanel = new InventoryPanel(inventory);
        ScansPanel scansPanel = new ScansPanel(travelHistory);
        ShipyardPanel shipyardPanel = new ShipyardPanel(gameSession);

        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.setFont(new Font("Sans Serif", Font.BOLD, 18));
        tabbedPane.addTab("Inventory", inventoryPanel);
        tabbedPane.addTab("Scans", scansPanel);
        tabbedPane.addTab("Shipyard", shipyardPanel);

        // Construct the window with all panels
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.addWindowListener(this);
        frame.setLayout(new BorderLayout());
        frame.add(statusPanel, BorderLayout.NORTH);
        frame.add(tabbedPane, BorderLayout.CENTER);
        frame.add(new JScrollPane(journalLogPanel), BorderLayout.SOUTH);
        frame.setSize(1800, 900);
        frame.setLocation(10, 10);
        //        frame.setSize(1280, 720);
        //        frame.setLocation(300, 100);
        frame.setVisible(true);
    }

    @Override
    public void onGameLoaded(String commander, String gameMode, String group, ShipLoadout ship) {
        if (this.frame != null) {
            if (StringUtils.isNotEmpty(group)) {
                this.frame.setTitle(String.format(Locale.US, "CMDR %s - %s: %s", commander, gameMode, group));
            } else {
                this.frame.setTitle(String.format(Locale.US, "CMDR %s - %s", commander, gameMode));
            }
        }
    }

    @Override
    public void onShipModuleChanged(String slot, ShipModule oldModule, ShipModule newModule) {
        // Do nothing
    }

    @Override
    public void onShipChanged(ShipLoadout oldShip, ShipLoadout newShip) {
        // Do nothing
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
