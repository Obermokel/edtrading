package borg.edtrading.gui;

import borg.edtrading.data.Item.ItemType;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.InventoryListener;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
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
public class StatusPanel extends JPanel implements InventoryListener {

    private static final long serialVersionUID = -712826465072375525L;

    static final Logger logger = LogManager.getLogger(StatusPanel.class);

    private final Inventory inventory;

    private JLabel locationLabel = new JLabel("System / Body");
    private JLabel factionAndAllegianceLabel = new JLabel("Faction (Allegiance)");
    private JLabel economyAndStateLabel = new JLabel("Economy (State)");
    private JLabel governmentAndSecurityLabel = new JLabel("Government (Security)");

    private AnimatedLabel fuelLabel = new AnimatedLabel("Fuel: 0.00t");

    private JLabel explLabel = new JLabel("Expl: 0 CR");
    private AnimatedLabel dataLabel = new AnimatedLabel("Data: 0");
    private AnimatedLabel matsLabel = new AnimatedLabel("Mats: 0");
    private JLabel cargoLabel = new JLabel("Cargo: 0t");

    private Timer fuelTimer = null;
    private Timer dataTimer = null;
    private Timer matsTimer = null;

    public StatusPanel(Inventory inventory) {
        this.inventory = inventory;

        this.fuelTimer = new Timer(10, this.fuelLabel);
        this.fuelTimer.setRepeats(true);
        this.dataTimer = new Timer(10, this.dataLabel);
        this.dataTimer.setRepeats(true);
        this.matsTimer = new Timer(10, this.matsLabel);
        this.matsTimer.setRepeats(true);

        this.setLayout(new BorderLayout());

        JPanel leftPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 25, 5));
        leftPanel.add(this.locationLabel);
        leftPanel.add(this.factionAndAllegianceLabel);
        leftPanel.add(this.economyAndStateLabel);
        leftPanel.add(this.governmentAndSecurityLabel);
        this.add(leftPanel, BorderLayout.WEST);

        JPanel centerPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 25, 5));
        centerPanel.add(this.fuelLabel);
        this.add(centerPanel, BorderLayout.CENTER);

        JPanel rightPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 25, 5));
        rightPanel.add(this.explLabel);
        rightPanel.add(this.dataLabel);
        rightPanel.add(this.matsLabel);
        rightPanel.add(this.cargoLabel);
        this.add(rightPanel, BorderLayout.EAST);

        // Initial update
        this.updateInventory();

        // Listen
        inventory.addListener(this);
    }

    @Override
    public void onInventoryReset(ItemType type, String name, int count) {
        this.updateInventory();
    }

    @Override
    public void onInventoryCollected(ItemType type, String name, int count) {
        this.updateInventory();
    }

    @Override
    public void onInventoryDiscarded(ItemType type, String name, int count) {
        this.updateInventory();
    }

    @Override
    public void onInventorySpent(ItemType type, String name, int count) {
        this.updateInventory();
    }

    private void updateInventory() {
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
            this.dataLabel.setForeground(Color.RED);
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
            this.matsLabel.setForeground(Color.RED);
        } else {
            if (this.matsTimer.isRunning()) {
                this.matsTimer.stop();
            }
            this.matsLabel.setForeground(Color.GRAY);
        }

        int totalCargo = this.inventory.getTotal(ItemType.COMMODITY);
        //        int haveDrones = this.inventory.getHave(Item.DRONES.getName());
        //        totalCargo = Math.max(0, totalCargo - haveDrones);
        int capacityCargo = this.inventory.getCapacity(ItemType.COMMODITY);
        float percentCargo = (float) totalCargo / (float) capacityCargo;
        this.cargoLabel.setText(String.format(Locale.US, "Cargo: %dt", totalCargo));
        if (percentCargo >= 1.0f) {
            this.cargoLabel.setForeground(Color.RED);
        } else if (percentCargo <= 0.0f) {
            this.cargoLabel.setForeground(Color.GRAY);
        } else {
            this.cargoLabel.setForeground(Color.LIGHT_GRAY);
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
