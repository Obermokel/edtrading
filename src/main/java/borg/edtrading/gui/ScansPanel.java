package borg.edtrading.gui;

import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.sidepanel.ScannedBody;
import borg.edtrading.sidepanel.TravelHistory;
import borg.edtrading.sidepanel.TravelHistoryListener;
import borg.edtrading.sidepanel.VisitedSystem;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BorderLayout;
import java.io.Serializable;
import java.util.Date;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;

/**
 * ScansPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScansPanel extends JPanel implements TravelHistoryListener {

    private static final long serialVersionUID = -4409093032216648613L;

    static final Logger logger = LogManager.getLogger(ScansPanel.class);

    private final TravelHistory travelHistory;

    private ScansTableModel tableModel = null;

    public ScansPanel(TravelHistory travelHistory) {
        this.travelHistory = travelHistory;

        //this.setLayout(new FlowLayout(FlowLayout.LEFT));
        this.setLayout(new BorderLayout());
        this.tableModel = new ScansTableModel(travelHistory);
        JTable table = new JTable(tableModel);
        table.setAutoCreateRowSorter(true);
        JScrollPane scrollPane = new JScrollPane(table);
        this.add(scrollPane, BorderLayout.CENTER);

        travelHistory.addListener(this);
    }

    @Override
    public void onLocationChanged() {
        this.tableModel.refresh(this.travelHistory);
    }

    @Override
    public void onFuelLevelChanged(float newFuelLevel) {
        // Do nothing
    }

    @Override
    public void onExplorationDataSold(SellExplorationDataEntry journalEntry) {
        this.tableModel.refresh(this.travelHistory);
    }

    public static class ScansTableModel extends AbstractTableModel {

        private static final long serialVersionUID = -8604700804393478706L;

        private final LinkedList<ScansTableRow> rows = new LinkedList<>();

        public ScansTableModel(TravelHistory travelHistory) {
            for (int idxSystem = travelHistory.getVisitedSystems().size() - 1; idxSystem >= 0; idxSystem--) {
                VisitedSystem vs = travelHistory.getVisitedSystems().get(idxSystem);
                for (int idxBody = vs.getScannedBodies().size() - 1; idxBody >= 0; idxBody--) {
                    ScannedBody sb = vs.getScannedBodies().get(idxBody);
                    this.rows.addLast(new ScansTableRow(sb));
                }
            }
        }

        public void refresh(TravelHistory travelHistory) {
            boolean upToDate = false;
            for (int idxSystem = travelHistory.getVisitedSystems().size() - 1; !upToDate && idxSystem >= 0; idxSystem--) {
                VisitedSystem vs = travelHistory.getVisitedSystems().get(idxSystem);
                for (int idxBody = vs.getScannedBodies().size() - 1; !upToDate && idxBody >= 0; idxBody--) {
                    ScannedBody sb = vs.getScannedBodies().get(idxBody);
                    if (this.rows.isEmpty() || sb.getTimestamp().after(this.rows.getFirst().getTimestamp())) {
                        this.rows.addFirst(new ScansTableRow(sb));
                    } else {
                        upToDate = true;
                    }
                }
            }

            this.fireTableDataChanged();
        }

        @Override
        public int getColumnCount() {
            return ScansTableRow.COLUMN_NAMES.length;
        }

        @Override
        public String getColumnName(int columnIndex) {
            return ScansTableRow.COLUMN_NAMES[columnIndex];
        }

        @Override
        public Class<?> getColumnClass(int columnIndex) {
            return ScansTableRow.COLUMN_CLASSES[columnIndex];
        }

        @Override
        public int getRowCount() {
            return this.rows.size();
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            return this.rows.get(rowIndex).getValueAt(columnIndex);
        }

        @Override
        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return this.rows.get(rowIndex).isCellEditable(columnIndex);
        }

    }

    public static class ScansTableRow implements Serializable {

        private static final long serialVersionUID = 419056822915512852L;

        static final String[] COLUMN_NAMES = { "Timestamp", "Body", "Type", "TFC?", "Radius", "Mass", "Temp", "Gravity", "Mats", "Payout", "1st?" };
        static final Class<?>[] COLUMN_CLASSES = { Date.class, String.class, String.class, Boolean.class, Float.class, Float.class, Float.class, Float.class, String.class, Integer.class, Boolean.class };

        private ScannedBody scannedBody = null;
        private String mats = null;

        ScansTableRow(ScannedBody sb) {
            this.scannedBody = sb;
            this.mats = findHigherThanUsualMats(sb.getMaterials());
        }

        private static String findHigherThanUsualMats(Map<String, Float> materials) {
            String result = "";
            if (materials != null) {
                for (String material : materials.keySet()) {
                    Float percentage = materials.get(material);
                    if (percentage != null) {
                        if ("foo".equals(material) && percentage > 99.9f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        }
                    }
                }
            }
            return result;
        }

        Date getTimestamp() {
            return this.scannedBody.getTimestamp();
        }

        Object getValueAt(int columnIndex) {
            switch (columnIndex) {
                case 0:
                    return this.scannedBody.getTimestamp();
                case 1:
                    return this.scannedBody.getBodyName();
                case 2:
                    return this.scannedBody.getBodyType();
                case 3:
                    return this.scannedBody.isTerraformingCandidate();
                case 4:
                    return this.scannedBody.getRadius();
                case 5:
                    return this.scannedBody.getMass();
                case 6:
                    return this.scannedBody.getSurfaceTemperature();
                case 7:
                    return this.scannedBody.getSurfaceGravity();
                case 8:
                    return this.mats;
                case 9:
                    return this.scannedBody.isFirstDiscovered() ? this.scannedBody.getRemainingBasePayout() + this.scannedBody.getRemainingBonusPayout() : this.scannedBody.getRemainingBasePayout();
                case 10:
                    return this.scannedBody.isFirstDiscovered();
                default:
                    return "";
            }
        }

        boolean isCellEditable(int columnIndex) {
            return columnIndex == 10;
        }

    }

}
