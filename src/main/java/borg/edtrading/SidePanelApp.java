package borg.edtrading;

import borg.edtrading.cfg.Config;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.eddn.EddnReaderThread;
import borg.edtrading.gui.DiscoveryPanel;
import borg.edtrading.gui.InventoryPanel;
import borg.edtrading.gui.JournalLogPanel;
import borg.edtrading.gui.ScansPanel;
import borg.edtrading.gui.ShipyardPanel;
import borg.edtrading.gui.StatusPanel;
import borg.edtrading.gui.TransactionsPanel;
import borg.edtrading.journal.JournalReaderThread;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.services.EddbService;
import borg.edtrading.sidepanel.GameSession;
import borg.edtrading.sidepanel.GameSessionListener;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.InventoryListener;
import borg.edtrading.sidepanel.ScannedBody;
import borg.edtrading.sidepanel.ShipLoadout;
import borg.edtrading.sidepanel.ShipModule;
import borg.edtrading.sidepanel.ShipModuleList;
import borg.edtrading.sidepanel.Transactions;
import borg.edtrading.sidepanel.TravelHistory;
import borg.edtrading.sidepanel.TravelHistoryListener;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

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
public class SidePanelApp implements WindowListener, GameSessionListener, TravelHistoryListener, InventoryListener {

    static final Logger logger = LogManager.getLogger(SidePanelApp.class);

    private static final AnnotationConfigApplicationContext APPCTX = new AnnotationConfigApplicationContext(Config.class);

    public static final boolean BIG_AND_BLACK = true;

    private JFrame frame = null;
    private JTabbedPane tabbedPane = null;
    private JournalReaderThread journalReaderThread = null;
    private EddnReaderThread eddnReaderThread = null;
    private GameSession gameSession = null;
    private Inventory inventory = null;
    private Transactions transactions = null;
    private TravelHistory travelHistory = null;

    public static void main(String[] args) throws IOException {
        new SidePanelApp().start();
    }

    private void start() throws IOException {
        Path journalDir = Paths.get(System.getProperty("user.home"));
        if (!"Guenther".equalsIgnoreCase(Paths.get(System.getProperty("user.home")).getFileName().toString())) {
            journalDir = journalDir.resolve("Saved Games\\Frontier Developments\\Elite Dangerous");
        } else {
            journalDir = journalDir.resolve("Google Drive\\Elite Dangerous\\Journal");
        }

        if (SidePanelApp.BIG_AND_BLACK) {
            try {
                UIManager.setLookAndFeel("com.jtattoo.plaf.noire.NoireLookAndFeel");
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        APPCTX.getBean(EddbService.class).updateEddbData(/* forceReindex = */ false);

        frame = new JFrame("SidePanel");

        // Create the reader thread
        journalReaderThread = new JournalReaderThread(journalDir);
        eddnReaderThread = new EddnReaderThread();

        // Create and register the journal listeners
        gameSession = new GameSession(journalReaderThread);
        gameSession.addListener(this);
        inventory = new Inventory(journalReaderThread, gameSession);
        transactions = new Transactions(journalReaderThread);
        travelHistory = new TravelHistory(journalReaderThread, gameSession);
        new ShipModuleList(gameSession);

        // Init the reader from existing files, then start to watch for changes
        journalReaderThread.init();
        journalReaderThread.start();
        eddnReaderThread.start();

        inventory.addListener(this);
        travelHistory.addListener(this);

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
        TransactionsPanel transactionsPanel = new TransactionsPanel(transactions);
        ScansPanel scansPanel = new ScansPanel(travelHistory);
        DiscoveryPanel discoveryPanel = new DiscoveryPanel(APPCTX, travelHistory);
        ShipyardPanel shipyardPanel = new ShipyardPanel(gameSession);

        eddnReaderThread.addListener(discoveryPanel);

        tabbedPane = new JTabbedPane();
        if (SidePanelApp.BIG_AND_BLACK) {
            tabbedPane.setFont(new Font("Sans Serif", Font.BOLD, 18));
        }
        tabbedPane.addTab("Inventory", inventoryPanel);
        tabbedPane.addTab("Transactions", transactionsPanel);
        tabbedPane.addTab("Scans", scansPanel);
        tabbedPane.addTab("Discovery", discoveryPanel);
        tabbedPane.addTab("Shipyard", shipyardPanel);

        // Construct the window with all panels
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.addWindowListener(this);
        frame.setLayout(new BorderLayout());
        frame.add(statusPanel, BorderLayout.NORTH);
        frame.add(tabbedPane, BorderLayout.CENTER);
        if ("Guenther".equalsIgnoreCase(Paths.get(System.getProperty("user.home")).getFileName().toString())) {
            frame.add(new JScrollPane(journalLogPanel), BorderLayout.SOUTH);
        }
        if (SidePanelApp.BIG_AND_BLACK) {
            frame.setSize(1800, 900);
            frame.setLocation(10, 10);
        } else {
            frame.setSize(1280, 720);
            frame.setLocation(300, 100);
        }
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
        eddnReaderThread.interrupt();
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
        while (eddnReaderThread.isAlive()) {
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

    @Override
    public void onSystemChanged() {
        tabbedPane.setSelectedIndex(3);
    }

    @Override
    public void onLocationChanged() {
        // Do nothing
    }

    @Override
    public void onBodyScanned(ScannedBody scannedBody) {
        if (scannedBody.getDistanceFromArrivalLS() != null && scannedBody.getDistanceFromArrivalLS().floatValue() > 0f) {
            tabbedPane.setSelectedIndex(2);
        }
    }

    @Override
    public void onFuelLevelChanged(float newFuelLevel) {
        // Do nothing
    }

    @Override
    public void onExplorationDataSold(SellExplorationDataEntry journalEntry) {
        tabbedPane.setSelectedIndex(2);
    }

    @Override
    public void onInventoryReset(ItemType type, String name, int count) {
        tabbedPane.setSelectedIndex(0);
    }

    @Override
    public void onInventoryCollected(ItemType type, String name, int count) {
        tabbedPane.setSelectedIndex(0);
    }

    @Override
    public void onInventoryDiscarded(ItemType type, String name, int count) {
        tabbedPane.setSelectedIndex(0);
    }

    @Override
    public void onInventorySpent(ItemType type, String name, int count) {
        tabbedPane.setSelectedIndex(0);
    }

}
