package borg.edtrading.gui;

import borg.edtrading.SidePanelApp;
import borg.edtrading.sidepanel.Transaction;
import borg.edtrading.sidepanel.Transactions;
import borg.edtrading.sidepanel.TransactionsListener;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Font;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;

/**
 * TransactionsPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TransactionsPanel extends Box implements TransactionsListener {

    private static final long serialVersionUID = -755645219813511370L;

    static final Logger logger = LogManager.getLogger(TransactionsPanel.class);

    private final Transactions transactions;

    private TransactionsTableModel tableModel = null;

    public TransactionsPanel(Transactions transactions) {
        super(BoxLayout.X_AXIS);

        this.transactions = transactions;

        this.tableModel = new TransactionsTableModel(this.transactions);
        JTable table = new JTable(tableModel);
        table.setAutoCreateRowSorter(true);
        if (SidePanelApp.BIG_AND_BLACK) {
            table.setFont(new Font("Sans Serif", Font.BOLD, 18));
        }
        JScrollPane scrollPane = new JScrollPane(table);
        this.add(scrollPane);

        transactions.addListener(this);
    }

    @Override
    public void onTransactionAdded(Transaction transaction) {
        this.tableModel.refresh(this.transactions);
    }

    @Override
    public void onTransactionRemoved(Transaction transaction) {
        this.tableModel.refresh(this.transactions);
    }

    public static class TransactionsTableModel extends AbstractTableModel {

        private static final long serialVersionUID = 2833982773523025638L;

        private final List<TransactionsTableRow> rows = new ArrayList<>();

        public TransactionsTableModel(Transactions transactions) {
            for (Transaction transaction : transactions.getTransactions()) {
                this.rows.add(new TransactionsTableRow(transaction));
            }
        }

        public void refresh(Transactions transactions) {
            this.rows.clear();
            for (Transaction transaction : transactions.getTransactions()) {
                this.rows.add(new TransactionsTableRow(transaction));
            }

            this.fireTableDataChanged();
        }

        @Override
        public int getRowCount() {
            return this.rows.size();
        }

        @Override
        public int getColumnCount() {
            return TransactionsTableRow.COLUMN_NAMES.length;
        }

        @Override
        public String getColumnName(int columnIndex) {
            return TransactionsTableRow.COLUMN_NAMES[columnIndex];
        }

        @Override
        public Class<?> getColumnClass(int columnIndex) {
            return TransactionsTableRow.COLUMN_CLASSES[columnIndex];
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            return this.rows.get(rowIndex).getValueAt(columnIndex);
        }

    }

    public static class TransactionsTableRow implements Serializable {

        private static final long serialVersionUID = 2891028867789461913L;

        static final String[] COLUMN_NAMES = { "Timestamp", "Type", "Name", "Faction", "System / Station", "What", "Expiry", "Credits" };
        static final Class<?>[] COLUMN_CLASSES = { Date.class, String.class, String.class, String.class, String.class, String.class, Date.class, Integer.class };

        private final Transaction transaction;

        public TransactionsTableRow(Transaction transaction) {
            this.transaction = transaction;
        }

        public Object getValueAt(int columnIndex) {
            if (columnIndex == 0) {
                return this.transaction.getTimestamp();
            } else if (columnIndex == 1) {
                return this.transaction.getType().toString();
            } else if (columnIndex == 2) {
                return this.transaction.getName();
            } else if (columnIndex == 3) {
                return this.transaction.getFaction();
            } else if (columnIndex == 4) {
                String systemAndStation = null;
                if (StringUtils.isNotEmpty(this.transaction.getDestinationSystem())) {
                    systemAndStation = this.transaction.getDestinationSystem();
                    if (StringUtils.isNotEmpty(this.transaction.getDestinationStation())) {
                        systemAndStation += (" / " + this.transaction.getDestinationStation());
                    }
                }
                return systemAndStation;
            } else if (columnIndex == 5) {
                String what = null;
                if (this.transaction.getPassengerCount() != null) {
                    what = String.format(Locale.US, "%dx %s", this.transaction.getPassengerCount(), this.transaction.getPassengerType());
                } else if (this.transaction.getCommodityCount() != null) {
                    what = String.format(Locale.US, "%dx %s", this.transaction.getCommodityCount(), this.transaction.getCommodityLocalized());
                }
                return what;
            } else if (columnIndex == 6) {
                return this.transaction.getMissionExpiryDate();
            } else if (columnIndex == 7) {
                return this.transaction.getCredits();
            } else {
                return null;
            }
        }

    }

}
