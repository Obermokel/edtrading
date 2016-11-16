package borg.edtrading.gui;

import borg.edtrading.data.Item.ItemType;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.InventoryListener;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.util.Locale;

import javax.swing.JLabel;
import javax.swing.JPanel;

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

    private JLabel fuelLabel = new JLabel("Fuel: 0.00t");

    private JLabel explLabel = new JLabel("Expl: 0 CR");
    private JLabel dataLabel = new JLabel("Data: 0");
    private JLabel matsLabel = new JLabel("Mats: 0");
    private JLabel cargoLabel = new JLabel("Cargo: 0t");

    public StatusPanel(Inventory inventory) {
        this.inventory = inventory;

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
            this.dataLabel.setForeground(Color.RED); // TODO Animation
        } else if (percentData >= 0.8f) {
            this.dataLabel.setForeground(Color.RED);
        } else {
            this.dataLabel.setForeground(Color.LIGHT_GRAY); // TODO Look&Feel default
        }

        int totalMats = this.inventory.getTotal(ItemType.ELEMENT) + this.inventory.getTotal(ItemType.MANUFACTURED);
        int capacityMats = this.inventory.getCapacity(ItemType.ELEMENT);
        float percentMats = (float) totalMats / (float) capacityMats;
        this.matsLabel.setText(String.format(Locale.US, "Mats: %d", totalMats));
        if (percentMats >= 0.9f) {
            this.matsLabel.setForeground(Color.RED); // TODO Animation
        } else if (percentMats >= 0.8f) {
            this.matsLabel.setForeground(Color.RED);
        } else {
            this.matsLabel.setForeground(Color.LIGHT_GRAY); // TODO Look&Feel default
        }

        int totalCargo = this.inventory.getTotal(ItemType.COMMODITY);
        int capacityCargo = this.inventory.getCapacity(ItemType.COMMODITY);
        float percentCargo = (float) totalCargo / (float) capacityCargo;
        this.cargoLabel.setText(String.format(Locale.US, "Cargo: %dt", totalCargo));
        if (percentCargo >= 1.0f) {
            this.cargoLabel.setForeground(Color.RED);
        } else if (percentCargo <= 0.0f) {
            this.cargoLabel.setForeground(Color.LIGHT_GRAY); // TODO Look&Feel default
        } else {
            this.cargoLabel.setForeground(Color.BLACK);
        }
    }

}
