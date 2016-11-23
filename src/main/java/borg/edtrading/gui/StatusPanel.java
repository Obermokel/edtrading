package borg.edtrading.gui;

import borg.edtrading.SidePanelApp;
import borg.edtrading.data.Coord;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.sidepanel.GameSession;
import borg.edtrading.sidepanel.GameSessionListener;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.InventoryListener;
import borg.edtrading.sidepanel.ShipLoadout;
import borg.edtrading.sidepanel.ShipModule;
import borg.edtrading.sidepanel.TravelHistory;
import borg.edtrading.sidepanel.TravelHistoryListener;
import borg.edtrading.util.FuelAndJumpRangeLookup;
import borg.edtrading.util.MiscUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Locale;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Timer;

/**
 * StatusPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class StatusPanel extends JPanel implements GameSessionListener, TravelHistoryListener, InventoryListener {

    private static final long serialVersionUID = -712826465072375525L;

    static final Logger logger = LogManager.getLogger(StatusPanel.class);

    private final GameSession gameSession;
    private final TravelHistory travelHistory;
    private final Inventory inventory;

    private JLabel locationLabel = new JLabel("System / Body");
    private JLabel factionAndAllegianceLabel = new JLabel("Faction (Allegiance)");
    private JLabel economyAndStateLabel = new JLabel("Economy (State)");
    private JLabel governmentAndSecurityLabel = new JLabel("Government (Security)");

    private JLabel shipNameLabel = new JLabel("CMDR Name (Game Mode: Group)");

    private AnimatedLabel dataLabel = new AnimatedLabel("Data: 0");
    private AnimatedLabel matsLabel = new AnimatedLabel("Mats: 0");
    private JLabel cargoLabel = new JLabel("Cargo: 0t");
    private AnimatedLabel fuelLabel = new AnimatedLabel("Fuel: 0t");
    private JLabel distanceFromSolLabel = new JLabel("Sol: 0 Ly");
    private JLabel jumpLabel = new JLabel("Jump: 0.00 Ly");
    private JLabel explLabel = new JLabel("Expl: 0 CR");

    private Timer dataTimer = null;
    private Timer matsTimer = null;
    private Timer fuelTimer = null;

    public StatusPanel(GameSession gameSession, TravelHistory travelHistory, Inventory inventory) {
        this.gameSession = gameSession;
        this.travelHistory = travelHistory;
        this.inventory = inventory;

        this.fuelTimer = new Timer(10, this.fuelLabel);
        this.fuelTimer.setRepeats(true);
        this.dataTimer = new Timer(10, this.dataLabel);
        this.dataTimer.setRepeats(true);
        this.matsTimer = new Timer(10, this.matsLabel);
        this.matsTimer.setRepeats(true);

        Font font = new Font("Sans Serif", Font.BOLD, 24);
        int hgap = 40;
        int vgap = 4;

        if (SidePanelApp.BIG_AND_BLACK) {
            this.locationLabel.setFont(font);
            this.factionAndAllegianceLabel.setFont(font);
            this.economyAndStateLabel.setFont(font);
            this.governmentAndSecurityLabel.setFont(font);
            this.shipNameLabel.setFont(font);
            this.dataLabel.setFont(font);
            this.matsLabel.setFont(font);
            this.cargoLabel.setFont(font);
            this.fuelLabel.setFont(font);
            this.distanceFromSolLabel.setFont(font);
            this.jumpLabel.setFont(font);
            this.explLabel.setFont(font);
        }

        this.distanceFromSolLabel.setForeground(Color.GRAY);
        this.jumpLabel.setForeground(Color.GRAY);

        this.setLayout(new BorderLayout());
        JPanel topPanel = new JPanel(new BorderLayout());
        this.add(topPanel, BorderLayout.NORTH);
        JPanel bottomPanel = new JPanel(new BorderLayout());
        this.add(bottomPanel, BorderLayout.CENTER);

        JPanel leftTopPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, hgap, vgap));
        leftTopPanel.add(this.locationLabel);
        leftTopPanel.add(this.factionAndAllegianceLabel);
        topPanel.add(leftTopPanel, BorderLayout.WEST);

        JPanel leftBottomPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, hgap, vgap));
        leftBottomPanel.add(this.economyAndStateLabel);
        leftBottomPanel.add(this.governmentAndSecurityLabel);
        bottomPanel.add(leftBottomPanel, BorderLayout.WEST);

        JPanel centerTopPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, hgap, vgap));
        centerTopPanel.add(this.shipNameLabel);
        topPanel.add(centerTopPanel, BorderLayout.CENTER);

        JPanel rightTopPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, hgap, vgap));
        rightTopPanel.add(this.dataLabel);
        rightTopPanel.add(this.matsLabel);
        rightTopPanel.add(this.cargoLabel);
        rightTopPanel.add(this.fuelLabel);
        topPanel.add(rightTopPanel, BorderLayout.EAST);

        JPanel rightBottomPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, hgap, vgap));
        rightBottomPanel.add(this.distanceFromSolLabel);
        rightBottomPanel.add(this.jumpLabel);
        rightBottomPanel.add(this.explLabel);
        bottomPanel.add(rightBottomPanel, BorderLayout.EAST);

        // Initial update
        this.updatePanel();

        // Listen
        gameSession.addListener(this);
        travelHistory.addListener(this);
        inventory.addListener(this);
    }

    @Override
    public void onGameLoaded(String commander, String gameMode, String group, ShipLoadout ship) {
        this.updatePanel();
    }

    @Override
    public void onShipModuleChanged(String slot, ShipModule oldModule, ShipModule newModule) {
        this.updatePanel();
    }

    @Override
    public void onShipChanged(ShipLoadout oldShip, ShipLoadout newShip) {
        this.updatePanel();
    }

    @Override
    public void onLocationChanged() {
        this.updatePanel();
    }

    @Override
    public void onFuelLevelChanged(float newFuelLevel) {
        this.updatePanel();
    }

    @Override
    public void onExplorationDataSold(SellExplorationDataEntry journalEntry) {
        this.updatePanel();
    }

    @Override
    public void onInventoryReset(ItemType type, String name, int count) {
        this.updatePanel();
    }

    @Override
    public void onInventoryCollected(ItemType type, String name, int count) {
        this.updatePanel();
    }

    @Override
    public void onInventoryDiscarded(ItemType type, String name, int count) {
        this.updatePanel();
    }

    @Override
    public void onInventorySpent(ItemType type, String name, int count) {
        this.updatePanel();
    }

    private void updatePanel() {
        if (StringUtils.isEmpty(this.travelHistory.getBodyName())) {
            this.locationLabel.setText(String.format(Locale.US, "%s", this.travelHistory.getSystemName()));
        } else {
            this.locationLabel.setText(String.format(Locale.US, "%s / %s", this.travelHistory.getSystemName(), this.travelHistory.getBodyName()));
        }
        if (StringUtils.isEmpty(this.travelHistory.getFaction()) || "None".equals(this.travelHistory.getFaction())) {
            this.factionAndAllegianceLabel.setText("No faction");
        } else {
            this.factionAndAllegianceLabel.setText(String.format(Locale.US, "%s (%s)", this.travelHistory.getFaction(), this.travelHistory.getAllegiance()));
        }
        if (StringUtils.isEmpty(this.travelHistory.getEconomy()) || "None".equals(this.travelHistory.getEconomy())) {
            this.economyAndStateLabel.setText("No economy");
        } else if (StringUtils.isEmpty(this.travelHistory.getState()) || "None".equals(this.travelHistory.getState())) {
            this.economyAndStateLabel.setText(String.format(Locale.US, "%s", this.travelHistory.getEconomy()));
        } else {
            this.economyAndStateLabel.setText(String.format(Locale.US, "%s (%s)", this.travelHistory.getEconomy(), this.travelHistory.getState()));
        }
        if (StringUtils.isEmpty(this.travelHistory.getGovernment()) || "None".equals(this.travelHistory.getGovernment())) {
            this.governmentAndSecurityLabel.setText("No government");
        } else {
            this.governmentAndSecurityLabel.setText(String.format(Locale.US, "%s (%s)", this.travelHistory.getGovernment(), this.travelHistory.getSecurity()));
        }

        if (this.gameSession.getCurrentShipLoadout() != null) {
            if (StringUtils.isNotEmpty(this.gameSession.getCurrentShipLoadout().getShipName())) {
                this.shipNameLabel.setText(this.gameSession.getCurrentShipLoadout().getShipName());
            } else {
                this.shipNameLabel.setText(this.gameSession.getCurrentShipLoadout().getShipType());
            }
        }

        int totalData = this.inventory.getTotal(ItemType.DATA);
        int capacityData = this.inventory.getCapacity(ItemType.DATA);
        float percentData = (float) totalData / (float) capacityData;
        this.dataLabel.setText(String.format(Locale.US, "Data: %d", totalData));
        if (percentData >= 0.9f) {
            this.dataLabel.setForeground(Color.RED);
            if (!this.dataTimer.isRunning()) {
                this.dataTimer.start();
            }
        } else if (percentData >= 0.8f) {
            if (this.dataTimer.isRunning()) {
                this.dataTimer.stop();
            }
            this.dataLabel.setForeground(Color.ORANGE);
        } else {
            if (this.dataTimer.isRunning()) {
                this.dataTimer.stop();
            }
            this.dataLabel.setForeground(Color.GRAY);
        }

        int totalMats = this.inventory.getTotal(ItemType.ELEMENT) + this.inventory.getTotal(ItemType.MANUFACTURED);
        int capacityMats = this.inventory.getCapacity(ItemType.ELEMENT);
        float percentMats = (float) totalMats / (float) capacityMats;
        this.matsLabel.setText(String.format(Locale.US, "Mats: %d", totalMats));
        if (percentMats >= 0.9f) {
            this.matsLabel.setForeground(Color.RED);
            if (!this.matsTimer.isRunning()) {
                this.matsTimer.start();
            }
        } else if (percentMats >= 0.8f) {
            if (this.matsTimer.isRunning()) {
                this.matsTimer.stop();
            }
            this.matsLabel.setForeground(Color.ORANGE);
        } else {
            if (this.matsTimer.isRunning()) {
                this.matsTimer.stop();
            }
            this.matsLabel.setForeground(Color.GRAY);
        }

        int totalCargo = this.inventory.getTotal(ItemType.COMMODITY); // This does not include drones
        int capacityCargo = Math.max(totalCargo, this.inventory.getCapacity(ItemType.COMMODITY));
        float percentCargo = (float) totalCargo / (float) capacityCargo;
        this.cargoLabel.setText(String.format(Locale.US, "Cargo: %dt", totalCargo));
        if (percentCargo >= 1.0f) {
            this.cargoLabel.setForeground(Color.RED); // Full. Red, but do not flash as this is common business.
        } else if (percentCargo >= 0.9f) {
            this.cargoLabel.setForeground(Color.ORANGE);
        } else if (percentCargo <= 0.0f) {
            this.cargoLabel.setForeground(Color.GRAY); // Only be gray (barely visible) when completely empty.
        } else {
            this.cargoLabel.setForeground(Color.LIGHT_GRAY); // Use light gray (better visibility) if s.th. is loaded.
        }

        this.distanceFromSolLabel.setText(String.format(Locale.US, "Sol: %.0f Ly", new Coord(0, 0, 0).distanceTo(this.travelHistory.getCoord())));
        float totalFuel = this.travelHistory.getFuelLevel();
        float capacityFuel = Math.max(totalFuel, this.travelHistory.getFuelCapacity());
        float percentFuel = totalFuel / capacityFuel;
        float maxFuelPerJump = 1f;
        if (this.gameSession.getCurrentShipLoadout() != null && this.gameSession.getCurrentShipLoadout().getMaxFuelPerJump() != null) {
            maxFuelPerJump = this.gameSession.getCurrentShipLoadout().getMaxFuelPerJump();
        }
        float percentMaxFuelPerJump = totalFuel / maxFuelPerJump;
        this.fuelLabel.setText(String.format(Locale.US, "Fuel: %.0ft", totalFuel));
        if (percentFuel <= 0.125f || percentMaxFuelPerJump <= 1.5f) {
            this.fuelLabel.setForeground(Color.RED);
            if (!this.fuelTimer.isRunning()) {
                this.fuelTimer.start();
            }
        } else if (percentFuel <= 0.2f || percentMaxFuelPerJump <= 2.0f) {
            if (this.fuelTimer.isRunning()) {
                this.fuelTimer.stop();
            }
            this.fuelLabel.setForeground(Color.ORANGE);
        } else {
            if (this.fuelTimer.isRunning()) {
                this.fuelTimer.stop();
            }
            this.fuelLabel.setForeground(Color.GRAY);
        }

        float minJumpRange = MiscUtil.getAsFloat(this.gameSession.getCurrentShipLoadout().getFullTankJumpRange(), 1.0f);
        float maxJumpRange = MiscUtil.getAsFloat(this.gameSession.getCurrentShipLoadout().getOptTankJumpRange(), 1.0f);
        float currentJumpRange = FuelAndJumpRangeLookup.estimateCurrentJumpRange(totalFuel, (int) capacityFuel, maxFuelPerJump, minJumpRange, maxJumpRange);
        this.jumpLabel.setText(String.format(Locale.US, "Jump: %.2f Ly", currentJumpRange));
        int explPayout = this.travelHistory.estimateRemainingExplorationPayout();
        this.explLabel.setText(String.format(Locale.US, "Expl: %,d CR", explPayout));
        if (explPayout >= 1000000) {
            this.explLabel.setForeground(Color.RED);
        } else if (explPayout >= 250000) {
            this.explLabel.setForeground(Color.ORANGE);
        } else if (explPayout > 0) {
            this.explLabel.setForeground(Color.LIGHT_GRAY);
        } else {
            this.explLabel.setForeground(Color.GRAY);
        }
    }

    public static class AnimatedLabel extends JLabel implements ActionListener {

        private static final long serialVersionUID = -7475510223114721124L;

        private int flashRed = 255;
        private int flashInc = -5;

        public AnimatedLabel() {
            super();
        }

        public AnimatedLabel(Icon image, int horizontalAlignment) {
            super(image, horizontalAlignment);
        }

        public AnimatedLabel(Icon image) {
            super(image);
        }

        public AnimatedLabel(String text, Icon icon, int horizontalAlignment) {
            super(text, icon, horizontalAlignment);
        }

        public AnimatedLabel(String text, int horizontalAlignment) {
            super(text, horizontalAlignment);
        }

        public AnimatedLabel(String text) {
            super(text);
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            this.flashRed += this.flashInc;

            if (this.flashRed <= 50) {
                this.flashRed = 50;
                this.flashInc *= -1;
            } else if (this.flashRed >= 255) {
                this.flashRed = 255;
                this.flashInc *= -1;
            } else if (this.flashRed == 200) {
                if (this.flashInc == 10) {
                    this.flashInc = 5;
                } else if (this.flashInc == -5) {
                    this.flashInc = -10;
                }
            }

            this.setForeground(new Color(this.flashRed, 0, 0));
        }

    }

}
