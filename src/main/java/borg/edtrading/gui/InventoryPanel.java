package borg.edtrading.gui;

import borg.edtrading.data.Item.ItemType;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.InventoryListener;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.border.TitledBorder;
import javax.swing.table.AbstractTableModel;

/**
 * InventoryPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryPanel extends JPanel implements InventoryListener {

    private static final long serialVersionUID = 4657223926306497803L;

    static final Logger logger = LogManager.getLogger(InventoryPanel.class);

    private final Inventory inventory;

    public InventoryPanel(Inventory inventory) {
        this.inventory = inventory;

        this.onInventoryChanged();
    }

    @Override
    public void onInventoryReset(ItemType type, String name, int count) {
        this.onInventoryChanged();
    }

    @Override
    public void onInventoryCollected(ItemType type, String name, int count) {
        this.onInventoryChanged();
    }

    @Override
    public void onInventoryDiscarded(ItemType type, String name, int count) {
        this.onInventoryChanged();
    }

    @Override
    public void onInventorySpent(ItemType type, String name, int count) {
        this.onInventoryChanged();
    }

    private void onInventoryChanged() {
        this.removeAll();
        this.setLayout(new FlowLayout());

        for (ItemType type : Arrays.asList(ItemType.ELEMENT, ItemType.MANUFACTURED, ItemType.DATA, ItemType.COMMODITY)) {
            List<InventoryTableRow> rows = new ArrayList<>();
            for (String name : this.inventory.getNames(type)) {
                rows.add(new InventoryTableRow(name, this.inventory.getHave(name), this.inventory.getSurplus(name)));
            }

            // Add the table for this item type
            JTable table = new JTable(new InventoryTableModel(rows));
            table.setPreferredSize(new Dimension(450, 800));
            table.setPreferredScrollableViewportSize(table.getPreferredSize());
            table.setFillsViewportHeight(true);
            table.setAutoCreateRowSorter(true);
            for (int i = 0; i < 3; i++) {
                if (i == 0) {
                    table.getColumnModel().getColumn(i).setPreferredWidth(250);
                } else {
                    table.getColumnModel().getColumn(i).setPreferredWidth(100);
                }
            }
            JScrollPane scrollPane = new JScrollPane(table);
            scrollPane.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), type.name(), TitledBorder.CENTER, TitledBorder.TOP));
            this.add(scrollPane);
        }

        this.repaint();
    }

    public static class InventoryTableModel extends AbstractTableModel {

        private static final long serialVersionUID = 6225525347374881663L;

        private final List<InventoryTableRow> rows;

        public InventoryTableModel(List<InventoryTableRow> rows) {
            this.rows = rows;
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
