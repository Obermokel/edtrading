package borg.edtrading.gui;

import borg.edtrading.SidePanelApp;
import borg.edtrading.data.Item.ItemType;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.InventoryListener;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultCellEditor;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.Timer;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * InventoryPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryPanel extends Box implements InventoryListener {

    private static final long serialVersionUID = 4657223926306497803L;

    static final Logger logger = LogManager.getLogger(InventoryPanel.class);

    private final Inventory inventory;
    private Map<ItemType, InventoryTableModel> tableModelsByType = new HashMap<>();

    public InventoryPanel(Inventory inventory) {
        super(BoxLayout.X_AXIS);

        this.inventory = inventory;

        for (ItemType type : Arrays.asList(ItemType.ELEMENT, ItemType.MANUFACTURED, ItemType.DATA, ItemType.COMMODITY)) {
            // Add the table for this item type
            InventoryTableModel tableModel = new InventoryTableModel(this.inventory, type);
            this.tableModelsByType.put(type, tableModel);
            JTable table = new JTable(tableModel);
            table.setAutoCreateRowSorter(true);
            table.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE); // TODO Required?
            if (SidePanelApp.BIG_AND_BLACK) {
                table.setFont(new Font("Sans Serif", Font.BOLD, 18));
            }
            table.getColumn("Name").setCellRenderer(new FlashingNameCellRenderer());
            table.getColumn("Have").setCellEditor(new PlusMinusCellEditor(new JTextField(3)));
            for (int i = 0; i < 3; i++) {
                if (i == 0) {
                    table.getColumnModel().getColumn(i).setPreferredWidth(220);
                } else {
                    table.getColumnModel().getColumn(i).setPreferredWidth(50);
                }
            }
            JScrollPane scrollPane = new JScrollPane(table);
            this.add(scrollPane);
        }

        inventory.addListener(this);
    }

    @Override
    public void onInventoryReset(ItemType type, String name, int count) {
        this.tableModelsByType.get(type).refresh(name, Color.YELLOW);
        this.repaint(); // TODO Required?
    }

    @Override
    public void onInventoryCollected(ItemType type, String name, int count) {
        this.tableModelsByType.get(type).refresh(name, Color.GREEN);
        this.repaint(); // TODO Required?
    }

    @Override
    public void onInventoryDiscarded(ItemType type, String name, int count) {
        this.tableModelsByType.get(type).refresh(name, Color.RED);
        this.repaint(); // TODO Required?
    }

    @Override
    public void onInventorySpent(ItemType type, String name, int count) {
        this.tableModelsByType.get(type).refresh(name, Color.BLUE);
        this.repaint(); // TODO Required?
    }

    public static class InventoryTableModel extends AbstractTableModel implements TableModelListener, ActionListener {

        private static final long serialVersionUID = 6225525347374881663L;

        private final Inventory inventory;
        private final ItemType type;

        private List<InventoryTableRow> rows = null;
        private Map<String, Timer> timersByName = new HashMap<>();
        private Map<String, Color> flashColorsByName = new HashMap<>();
        private Map<String, Integer> flashCountersByName = new HashMap<>();

        public InventoryTableModel(Inventory inventory, ItemType type) {
            this.inventory = inventory;
            this.type = type;
            this.refresh(null, null);
            this.addTableModelListener(this);
        }

        void refresh(String flashName, Color flashColor) {
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
            if (flashName != null && flashColor != null) {
                Timer timer = this.timersByName.get(flashName);
                if (timer == null) {
                    timer = new Timer(666, this);
                    timer.setRepeats(true);
                    timer.setActionCommand(flashName);
                    this.timersByName.put(flashName, timer);
                }
                this.flashColorsByName.put(flashName, flashColor);
                this.flashCountersByName.put(flashName, 10);
                if (!timer.isRunning()) {
                    timer.start();
                }
            }
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (e.getSource() instanceof Timer) {
                Timer timer = (Timer) e.getSource();
                String name = e.getActionCommand();

                int rowIndex = -1;
                for (int idx = 0; idx < this.rows.size(); idx++) {
                    if (this.rows.get(idx).getName().equals(name)) {
                        rowIndex = idx;
                        break;
                    }
                }

                if (rowIndex >= 0) {
                    int counter = this.flashCountersByName.getOrDefault(name, 1);
                    counter--;
                    if (counter <= 0) {
                        timer.stop();
                        this.flashColorsByName.remove(name);
                        this.flashCountersByName.remove(name);
                    } else {
                        this.flashCountersByName.put(name, counter);
                    }
                    this.fireTableRowsUpdated(rowIndex, rowIndex);
                }
            }
        }

        public Color getFlashingColor(String name) {
            int counter = this.flashCountersByName.getOrDefault(name, 0);

            if (counter > 0 && counter % 2 == 1) {
                return this.flashColorsByName.get(name);
            } else {
                return null;
            }
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
        public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
            InventoryTableRow row = this.rows.get(rowIndex);

            if (columnIndex == 1) {
                row.setHave(MiscUtil.getAsInt(aValue, 0));
                this.fireTableCellUpdated(rowIndex, columnIndex);
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

        @Override
        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return columnIndex == 1; // 'Have' column is editable
        }

        @Override
        public void tableChanged(TableModelEvent e) {
            // TODO Auto-generated method stub
            String changeType = "UNKNOWN";
            if (e.getType() == TableModelEvent.INSERT) {
                changeType = "INSERT";
            } else if (e.getType() == TableModelEvent.UPDATE) {
                changeType = "UPDATE";
            } else if (e.getType() == TableModelEvent.DELETE) {
                changeType = "DELETE";
            }
            logger.info(String.format(Locale.US, "%s col %d rows %d to %d on %s", changeType, e.getColumn(), e.getFirstRow(), e.getLastRow(), e.getSource()));

            if (e.getColumn() == 1 && e.getFirstRow() != TableModelEvent.HEADER_ROW && e.getFirstRow() == e.getLastRow()) {
                String name = (String) this.getValueAt(e.getFirstRow(), 0);
                int haveAfter = (int) this.getValueAt(e.getFirstRow(), 1);
                int haveBefore = this.inventory.getHave(name);
                logger.info(String.format(Locale.US, "%s %d -> %d", name, haveBefore, haveAfter));
                if (haveAfter != haveBefore) {
                    this.inventory.changeOffset(name, haveAfter - haveBefore);
                    this.refresh(null, null);
                }
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

    public static class FlashingNameCellRenderer extends DefaultTableCellRenderer {

        private static final long serialVersionUID = -6076484142143621910L;

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            Component comp = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            if (row >= 0) {
                String name = (String) table.getValueAt(row, 0);
                InventoryTableModel model = (InventoryTableModel) table.getModel();
                Color flashingColor = model.getFlashingColor(name);
                if (flashingColor != null) {
                    comp.setForeground(flashingColor);
                }
            }
            return comp;
        }

    }

    public static class PlusMinusCellEditor extends DefaultCellEditor {

        private static final long serialVersionUID = -6809216674791253543L;

        //        public PlusMinusCellEditor(JCheckBox checkBox) {
        //            super(checkBox);
        //        }
        //
        //        public PlusMinusCellEditor(JComboBox comboBox) {
        //            super(comboBox);
        //        }

        public PlusMinusCellEditor(JTextField textField) {
            super(textField);
        }

    }

}
