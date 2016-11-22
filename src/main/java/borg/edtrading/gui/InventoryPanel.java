package borg.edtrading.gui;

import borg.edtrading.data.Item.ItemType;
import borg.edtrading.sidepanel.Inventory;
import borg.edtrading.sidepanel.InventoryListener;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Component;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.UIManager;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;

/**
 * InventoryPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class InventoryPanel extends Box implements InventoryListener {

    private static final long serialVersionUID = 4657223926306497803L;

    static final Logger logger = LogManager.getLogger(InventoryPanel.class);

    private static final boolean SHOW_BUTTONS = false;
    private static final int HISTORY_SIZE = 100;

    private final Inventory inventory;
    private Map<ItemType, InventoryTableModel> tableModelsByType = new HashMap<>();

    private final LinkedList<String> history = new LinkedList<>();
    private final JList<String> historyList = new JList<>();

    public InventoryPanel(Inventory inventory) {
        super(BoxLayout.X_AXIS);

        this.inventory = inventory;

        for (ItemType type : Arrays.asList(ItemType.ELEMENT, ItemType.MANUFACTURED, ItemType.DATA, ItemType.COMMODITY)) {
            // Add the table for this item type
            InventoryTableModel tableModel = new InventoryTableModel(this.inventory, type);
            this.tableModelsByType.put(type, tableModel);
            JTable table = new JTable(tableModel);
            table.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);
            table.setFont(new Font("Sans Serif", Font.BOLD, 18));
            if (SHOW_BUTTONS) {
                table.getColumn("+").setCellRenderer(new ButtonRenderer());
                table.getColumn("+").setCellEditor(new ButtonEditor(new JCheckBox(), inventory, tableModel));
                table.getColumn("-").setCellRenderer(new ButtonRenderer());
                table.getColumn("-").setCellEditor(new ButtonEditor(new JCheckBox(), inventory, tableModel));
            }
            table.setAutoCreateRowSorter(true);
            if (SHOW_BUTTONS) {
                for (int i = 0; i < 5; i++) {
                    if (i == 0) {
                        table.getColumnModel().getColumn(i).setPreferredWidth(200);
                    } else if (i == 1 || i == 2) {
                        table.getColumnModel().getColumn(i).setPreferredWidth(10);
                    } else {
                        table.getColumnModel().getColumn(i).setPreferredWidth(50);
                    }
                }
            } else {
                for (int i = 0; i < 3; i++) {
                    if (i == 0) {
                        table.getColumnModel().getColumn(i).setPreferredWidth(220);
                    } else {
                        table.getColumnModel().getColumn(i).setPreferredWidth(50);
                    }
                }
            }
            JScrollPane scrollPane = new JScrollPane(table);
            this.add(scrollPane);
        }

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

    public static class InventoryTableModel extends AbstractTableModel implements TableModelListener {

        private static final long serialVersionUID = 6225525347374881663L;

        private final Inventory inventory;
        private final ItemType type;

        private List<InventoryTableRow> rows = null;

        public InventoryTableModel(Inventory inventory, ItemType type) {
            this.inventory = inventory;
            this.type = type;
            this.refresh();
            this.addTableModelListener(this);
        }

        void refresh() {
            List<InventoryTableRow> rows = new ArrayList<>();
            for (String name : this.inventory.getNames(this.type)) {
                int have = this.inventory.getHave(name);
                int surplus = this.inventory.getSurplus(name);
                if (have != 0 || SHOW_BUTTONS) {
                    rows.add(new InventoryTableRow(name, have, surplus));
                }
            }
            this.rows = rows;
            this.fireTableDataChanged();
        }

        @Override
        public int getColumnCount() {
            return SHOW_BUTTONS ? 5 : 3;
        }

        @Override
        public String getColumnName(int columnIndex) {
            if (SHOW_BUTTONS) {
                if (columnIndex == 0) {
                    return "Name";
                } else if (columnIndex == 1) {
                    return "+";
                } else if (columnIndex == 2) {
                    return "-";
                } else if (columnIndex == 3) {
                    return "Have";
                } else if (columnIndex == 4) {
                    return "Surplus";
                } else {
                    return null;
                }
            } else {
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
        }

        @Override
        public int getRowCount() {
            return this.rows.size();
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            InventoryTableRow row = this.rows.get(rowIndex);

            if (SHOW_BUTTONS) {
                if (columnIndex == 0) {
                    return row.getName();
                } else if (columnIndex == 1) {
                    return "+" + this.getValueAt(rowIndex, 0);
                } else if (columnIndex == 2) {
                    return "-" + this.getValueAt(rowIndex, 0);
                } else if (columnIndex == 3) {
                    return row.getHave();
                } else if (columnIndex == 4) {
                    return row.getSurplus();
                } else {
                    return null;
                }
            } else {
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
        }

        @Override
        public Class getColumnClass(int columnIndex) {
            if (SHOW_BUTTONS) {
                if (columnIndex == 0) {
                    return String.class;
                } else if (columnIndex == 1) {
                    return String.class;
                } else if (columnIndex == 2) {
                    return String.class;
                } else if (columnIndex == 3) {
                    return Integer.class;
                } else if (columnIndex == 4) {
                    return Integer.class;
                } else {
                    return null;
                }
            } else {
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

        @Override
        public boolean isCellEditable(int rowIndex, int columnIndex) {
            if (SHOW_BUTTONS) {
                if (columnIndex == 1) {
                    return true;
                } else if (columnIndex == 2) {
                    return true;
                } else {
                    return false;
                }
            } else {
                return columnIndex == 1; // 'Have' column is editable
            }
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
                    this.refresh();
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

    public static class ButtonRenderer extends JButton implements TableCellRenderer {

        private static final long serialVersionUID = -2294184162405104261L;

        public ButtonRenderer() {
            setOpaque(true);
        }

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            if (isSelected) {
                setForeground(table.getSelectionForeground());
                setBackground(table.getSelectionBackground());
            } else {
                setForeground(table.getForeground());
                setBackground(UIManager.getColor("Button.background"));
            }
            setText((value == null) ? "" : value.toString());
            return this;
        }
    }

    public static class ButtonEditor extends DefaultCellEditor {

        private static final long serialVersionUID = -7551303461614498752L;

        protected JButton button;

        private Inventory inventory;
        private InventoryTableModel tableModel;

        private String label;

        private boolean isPushed;

        public ButtonEditor(JCheckBox checkBox, Inventory inventory, InventoryTableModel tableModel) {
            super(checkBox);
            this.inventory = inventory;
            this.tableModel = tableModel;
            button = new JButton();
            button.setOpaque(true);
            button.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    fireEditingStopped();
                }
            });
        }

        @Override
        public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
            if (isSelected) {
                button.setForeground(table.getSelectionForeground());
                button.setBackground(table.getSelectionBackground());
            } else {
                button.setForeground(table.getForeground());
                button.setBackground(table.getBackground());
            }
            label = (value == null) ? "" : value.toString();
            button.setText(label);
            isPushed = true;
            return button;
        }

        @Override
        public Object getCellEditorValue() {
            if (isPushed) {
                if (label.startsWith("+")) {
                    inventory.incOffset(label.substring(1));
                } else if (label.startsWith("-")) {
                    inventory.decOffset(label.substring(1));
                }
                this.tableModel.refresh();
            }
            isPushed = false;
            return new String(label);
        }

        @Override
        public boolean stopCellEditing() {
            isPushed = false;
            return super.stopCellEditing();
        }

        @Override
        protected void fireEditingStopped() {
            super.fireEditingStopped();
        }

    }

}
