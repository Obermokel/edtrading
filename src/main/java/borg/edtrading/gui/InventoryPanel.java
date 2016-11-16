package borg.edtrading.gui;

import borg.edtrading.data.Item.ItemType;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.InventoryListener;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Font;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.border.TitledBorder;
import javax.swing.table.AbstractTableModel;

/**
 * InventoryPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryPanel extends JSplitPane implements InventoryListener {

    private static final long serialVersionUID = 4657223926306497803L;

    static final Logger logger = LogManager.getLogger(InventoryPanel.class);

    private static final int HISTORY_SIZE = 100;

    private final Inventory inventory;
    private Map<ItemType, InventoryTableModel> tableModelsByType = new HashMap<>();

    private final LinkedList<String> history = new LinkedList<>();
    private final JList<String> historyList = new JList<>();

    public InventoryPanel(Inventory inventory) {
        this.inventory = inventory;

        Box inventoryBox = new Box(BoxLayout.X_AXIS);
        for (ItemType type : Arrays.asList(ItemType.ELEMENT, ItemType.MANUFACTURED, ItemType.DATA, ItemType.COMMODITY)) {
            // Add the table for this item type
            InventoryTableModel tableModel = new InventoryTableModel(this.inventory, type);
            this.tableModelsByType.put(type, tableModel);
            JTable table = new JTable(tableModel);
            table.setAutoCreateRowSorter(true);
            for (int i = 0; i < 3; i++) {
                if (i == 0) {
                    table.getColumnModel().getColumn(i).setPreferredWidth(220);
                } else {
                    table.getColumnModel().getColumn(i).setPreferredWidth(70);
                }
            }
            JScrollPane scrollPane = new JScrollPane(table);
            scrollPane.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), type.name(), TitledBorder.CENTER, TitledBorder.TOP));
            inventoryBox.add(scrollPane);
        }
        this.setLeftComponent(inventoryBox);

        this.historyList.setFont(new Font("Consolas", Font.PLAIN, 10));
        JScrollPane scrollPane = new JScrollPane(this.historyList);
        scrollPane.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "HISTORY", TitledBorder.CENTER, TitledBorder.TOP));
        this.setRightComponent(scrollPane);

        this.setResizeWeight(1); // Resize left box, keep right history same size

        inventory.addListener(this);
    }

    @Override
    public void onInventoryReset(ItemType type, String name, int count) {
        this.tableModelsByType.get(type).refresh();
        this.addToHistory(String.format(Locale.US, "Reset: %s (%s) to %d", name, type.name(), count));
        this.repaint();
    }

    @Override
    public void onInventoryCollected(ItemType type, String name, int count) {
        this.tableModelsByType.get(type).refresh();
        this.addToHistory(String.format(Locale.US, "Collected: %dx %s (%s)", count, name, type.name()));
        this.repaint();
    }

    @Override
    public void onInventoryDiscarded(ItemType type, String name, int count) {
        this.tableModelsByType.get(type).refresh();
        this.addToHistory(String.format(Locale.US, "Discarded: %dx %s (%s)", count, name, type.name()));
        this.repaint();
    }

    @Override
    public void onInventorySpent(ItemType type, String name, int count) {
        this.tableModelsByType.get(type).refresh();
        this.addToHistory(String.format(Locale.US, "Spent: %dx %s (%s)", count, name, type.name()));
        this.repaint();
    }

    private void addToHistory(String line) {
        if (StringUtils.isNotEmpty(line)) {
            this.history.addFirst(line);
            while (this.history.size() > HISTORY_SIZE) {
                this.history.removeLast();
            }
            this.historyList.setListData(this.history.toArray(new String[this.history.size()]));
        }
    }

    public static class InventoryTableModel extends AbstractTableModel {

        private static final long serialVersionUID = 6225525347374881663L;

        private final Inventory inventory;
        private final ItemType type;

        private List<InventoryTableRow> rows = null;

        public InventoryTableModel(Inventory inventory, ItemType type) {
            this.inventory = inventory;
            this.type = type;
            this.refresh();
        }

        public void refresh() {
            List<InventoryTableRow> rows = new ArrayList<>();
            for (String name : this.inventory.getNames(this.type)) {
                int have = this.inventory.getHave(name);
                int surplus = this.inventory.getSurplus(name);
                if (have != 0) {
                    rows.add(new InventoryTableRow(name, have, surplus));
                }
            }
            this.rows = rows;
            this.fireTableDataChanged();
        }

        @Override
        public int getColumnCount() {
            return 3;
        }

        @Override
        public String getColumnName(int columnIndex) {
            if (columnIndex == 0) {
                return "Name";
            } else if (columnIndex == 1) {
                return "Have";
            } else if (columnIndex == 2) {
                return "Surplus";
            } else {
                return null;
            }
        }

        @Override
        public int getRowCount() {
            return this.rows.size();
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            InventoryTableRow row = this.rows.get(rowIndex);

            if (columnIndex == 0) {
                return row.getName();
            } else if (columnIndex == 1) {
                return row.getHave();
            } else if (columnIndex == 2) {
                return row.getSurplus();
            } else {
                return null;
            }
        }

        @Override
        public Class getColumnClass(int columnIndex) {
            if (columnIndex == 0) {
                return String.class;
            } else if (columnIndex == 1) {
                return Integer.class;
            } else if (columnIndex == 2) {
                return Integer.class;
            } else {
                return null;
            }
        }

    }

    public static class InventoryTableRow implements Serializable {

        private static final long serialVersionUID = -2201846690768816471L;

        private String name = null;
        private int have = 0;
        private int surplus = 0;

        public InventoryTableRow(String name, int have, int surplus) {
            this.setName(name);
            this.setHave(have);
            this.setSurplus(surplus);
        }

        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public int getHave() {
            return this.have;
        }

        public void setHave(int have) {
            this.have = have;
        }

        public int getSurplus() {
            return this.surplus;
        }

        public void setSurplus(int surplus) {
            this.surplus = surplus;
        }

    }

}
