package borg.edtrading.gui;

import borg.edtrading.SidePanelApp;
import borg.edtrading.journal.entries.exploration.SellExplorationDataEntry;
import borg.edtrading.sidepanel.ScannedBody;
import borg.edtrading.sidepanel.TravelHistory;
import borg.edtrading.sidepanel.TravelHistoryListener;
import borg.edtrading.sidepanel.VisitedSystem;
import borg.edtrading.util.MiscUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Font;
import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.DefaultCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * ScansPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ScansPanel extends JPanel implements TravelHistoryListener, TableModelListener {

    private static final long serialVersionUID = -4409093032216648613L;

    static final Logger logger = LogManager.getLogger(ScansPanel.class);

    private final TravelHistory travelHistory;

    private ScansTableModel tableModel = null;

    public ScansPanel(TravelHistory travelHistory) {
        this.travelHistory = travelHistory;

        //this.setLayout(new FlowLayout(FlowLayout.LEFT));
        this.setLayout(new BorderLayout());
        this.tableModel = new ScansTableModel(travelHistory);
        this.tableModel.addTableModelListener(this);
        JTable table = new JTable(tableModel);
        if (SidePanelApp.BIG_AND_BLACK) {
            table.setFont(new Font("Sans Serif", Font.BOLD, 18));
            table.setRowHeight(24);
        }
        table.getColumn("Timestamp").setPreferredWidth(170);
        table.getColumn("Timestamp").setCellRenderer(new TimestampCellRenderer());
        table.getColumn("Body").setPreferredWidth(300);
        table.getColumn("Body").setCellRenderer(new GenericCellRenderer());
        table.getColumn("Type").setPreferredWidth(270);
        table.getColumn("Type").setCellRenderer(new GenericCellRenderer());
        table.getColumn("TFC?").setPreferredWidth(33);
        //table.getColumn("TFC?").setCellRenderer(new GenericCellRenderer());
        table.getColumn("Radius").setPreferredWidth(100);
        table.getColumn("Radius").setCellRenderer(new RadiusCellRenderer());
        table.getColumn("Mass").setPreferredWidth(100);
        table.getColumn("Mass").setCellRenderer(new MassCellRenderer());
        table.getColumn("Temp").setPreferredWidth(110);
        table.getColumn("Temp").setCellRenderer(new TempCellRenderer());
        table.getColumn("Gravity").setPreferredWidth(70);
        table.getColumn("Gravity").setCellRenderer(new GravityCellRenderer());
        table.getColumn("Mats").setPreferredWidth(600);
        table.getColumn("Mats").setCellRenderer(new GenericCellRenderer());
        table.getColumn("Payout").setPreferredWidth(110);
        table.getColumn("Payout").setCellRenderer(new PayoutCellRenderer());
        table.getColumn("1st?").setPreferredWidth(33);
        //table.getColumn("1st?").setCellRenderer(new GenericCellRenderer());
        table.getColumn("1st?").setCellEditor(new DefaultCellEditor(new JCheckBox()));
        table.setAutoCreateRowSorter(true);
        JScrollPane scrollPane = new JScrollPane(table);
        this.add(scrollPane, BorderLayout.CENTER);

        travelHistory.addListener(this);
    }

    @Override
    public void tableChanged(TableModelEvent e) {
        this.tableModel.refresh(this.travelHistory);
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
        private TravelHistory travelHistory = null;

        public ScansTableModel(TravelHistory travelHistory) {
            for (int idxSystem = travelHistory.getVisitedSystems().size() - 1; idxSystem >= 0; idxSystem--) {
                VisitedSystem vs = travelHistory.getVisitedSystems().get(idxSystem);
                for (int idxBody = vs.getScannedBodies().size() - 1; idxBody >= 0; idxBody--) {
                    ScannedBody sb = vs.getScannedBodies().get(idxBody);
                    this.rows.addLast(new ScansTableRow(sb));
                }
            }
            this.travelHistory = travelHistory;
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
            this.travelHistory = travelHistory;

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
        public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
            if (columnIndex == 10) {
                if (MiscUtil.getAsBoolean(aValue)) {
                    this.travelHistory.setToAssumedFirstDiscovery(this.getRow(rowIndex).getScannedBody().getBodyName());
                } else {
                    this.travelHistory.unsetAssumedFirstDiscovery(this.getRow(rowIndex).getScannedBody().getBodyName());
                }
                this.fireTableCellUpdated(rowIndex, columnIndex);
            }
        }

        @Override
        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return this.rows.get(rowIndex).isCellEditable(columnIndex);
        }

        ScansTableRow getRow(int rowIndex) {
            return this.rows.get(rowIndex);
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
                        if ("antimony".equals(material) && percentage >= 1.0f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("arsenic".equals(material) && percentage >= 2.1f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("cadmium".equals(material) && percentage >= 1.6f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                            //                        } else if ("carbon".equals(material) && percentage >= 14.4f) {
                            //                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                            //                        } else if ("chromium".equals(material) && percentage >= 9.4f) {
                            //                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                            //                        } else if ("germanium".equals(material) && percentage >= 4.6f) {
                            //                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                            //                        } else if ("iron".equals(material) && percentage >= 21.0f) {
                            //                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                            //                        } else if ("manganese".equals(material) && percentage >= 8.6f) {
                            //                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("mercury".equals(material) && percentage >= 0.9f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("molybdenum".equals(material) && percentage >= 1.4f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                            //                        } else if ("nickel".equals(material) && percentage >= 15.9f) {
                            //                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("niobium".equals(material) && percentage >= 1.4f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                            //                        } else if ("phosphorus".equals(material) && percentage >= 9.2f) {
                            //                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("polonium".equals(material) && percentage >= 0.6f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("ruthenium".equals(material) && percentage >= 1.3f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("selenium".equals(material) && percentage >= 4.1f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                            //                        } else if ("sulphur".equals(material) && percentage >= 17.1f) {
                            //                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("technetium".equals(material) && percentage >= 0.7f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("tellurium".equals(material) && percentage >= 1.2f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("tin".equals(material) && percentage >= 1.3f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("tungsten".equals(material) && percentage >= 1.2f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                            //                        } else if ("vanadium".equals(material) && percentage >= 4.9f) {
                            //                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("yttrium".equals(material) && percentage >= 1.3f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                            //                        } else if ("zinc".equals(material) && percentage >= 5.7f) {
                            //                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        } else if ("zirconium".equals(material) && percentage >= 2.6f) {
                            result += String.format(Locale.US, "%s%.1f%% %s", result.length() > 0 ? ", " : "", percentage, material);
                        }
                    }
                }
            }
            return result;
        }

        ScannedBody getScannedBody() {
            return this.scannedBody;
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

    public static class TimestampCellRenderer extends DefaultTableCellRenderer {

        private static final long serialVersionUID = -3193170662964769643L;

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            JLabel comp = (JLabel) super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            comp.setBorder(BorderFactory.createCompoundBorder(comp.getBorder(), BorderFactory.createEmptyBorder(0, 5, 0, 5)));
            if (value instanceof Date) {
                comp.setText(new SimpleDateFormat("dd.MM.yyyy HH:mm").format((Date) value));
            }
            return comp;
        }

    }

    public static class GenericCellRenderer extends DefaultTableCellRenderer {

        private static final long serialVersionUID = 1552177813355725235L;

        @Override
        public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
            JComponent comp = (JComponent) super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            comp.setBorder(BorderFactory.createCompoundBorder(comp.getBorder(), BorderFactory.createEmptyBorder(0, 5, 0, 5)));
            return comp;
        }

    }

    public static class RadiusCellRenderer extends DefaultTableCellRenderer {

        private static final long serialVersionUID = 1890080767427621901L;

        @Override
        public Component getTableCellRendererComponent(JTable table, Object radiusMeter, boolean isSelected, boolean hasFocus, int row, int column) {
            JLabel comp = (JLabel) super.getTableCellRendererComponent(table, radiusMeter, isSelected, hasFocus, row, column);
            comp.setBorder(BorderFactory.createCompoundBorder(comp.getBorder(), BorderFactory.createEmptyBorder(0, 5, 0, 5)));
            comp.setHorizontalAlignment(SwingConstants.RIGHT);
            if (radiusMeter instanceof Float) {
                ScansTableModel model = (ScansTableModel) table.getModel();
                boolean isStar = StringUtils.isNotEmpty(model.getRow(row).getScannedBody().getStarClass());
                if (isStar) {
                    comp.setText(String.format(Locale.US, "%.2f R☉", (float) radiusMeter / 695700000f));
                } else {
                    comp.setText(String.format(Locale.US, "%,.0fkm", (float) radiusMeter / 1000f));
                }
            }
            return comp;
        }

    }

    public static class MassCellRenderer extends DefaultTableCellRenderer {

        private static final long serialVersionUID = -2028478960461078784L;

        @Override
        public Component getTableCellRendererComponent(JTable table, Object stellarMass, boolean isSelected, boolean hasFocus, int row, int column) {
            JLabel comp = (JLabel) super.getTableCellRendererComponent(table, stellarMass, isSelected, hasFocus, row, column);
            comp.setBorder(BorderFactory.createCompoundBorder(comp.getBorder(), BorderFactory.createEmptyBorder(0, 5, 0, 5)));
            comp.setHorizontalAlignment(SwingConstants.RIGHT);
            if (stellarMass instanceof Float) {
                ScansTableModel model = (ScansTableModel) table.getModel();
                ScannedBody sb = model.getRow(row).getScannedBody();
                boolean isStar = StringUtils.isNotEmpty(sb.getStarClass());
                boolean isGasGaint = StringUtils.isNotEmpty(sb.getBodyType()) && sb.getBodyType().toLowerCase().contains("gas giant");
                if (isStar) {
                    comp.setText(String.format(Locale.US, "%.2f M☉", stellarMass));
                } else if (isGasGaint) {
                    comp.setText(String.format(Locale.US, "%.2f M♃", (float) stellarMass / 318f));
                } else {
                    comp.setText(String.format(Locale.US, "%.2f M♁", stellarMass));
                }
            }
            return comp;
        }

    }

    public static class TempCellRenderer extends DefaultTableCellRenderer {

        private static final long serialVersionUID = -5599524383613982944L;

        @Override
        public Component getTableCellRendererComponent(JTable table, Object tempKelvin, boolean isSelected, boolean hasFocus, int row, int column) {
            JLabel comp = (JLabel) super.getTableCellRendererComponent(table, tempKelvin, isSelected, hasFocus, row, column);
            comp.setBorder(BorderFactory.createCompoundBorder(comp.getBorder(), BorderFactory.createEmptyBorder(0, 5, 0, 5)));
            comp.setHorizontalAlignment(SwingConstants.RIGHT);
            if (tempKelvin instanceof Float) {
                comp.setText(String.format(Locale.US, "%,.0fK", tempKelvin));
            }
            return comp;
        }

    }

    public static class GravityCellRenderer extends DefaultTableCellRenderer {

        private static final long serialVersionUID = 7129431171069721825L;

        @Override
        public Component getTableCellRendererComponent(JTable table, Object gravityMs2, boolean isSelected, boolean hasFocus, int row, int column) {
            JLabel comp = (JLabel) super.getTableCellRendererComponent(table, gravityMs2, isSelected, hasFocus, row, column);
            comp.setBorder(BorderFactory.createCompoundBorder(comp.getBorder(), BorderFactory.createEmptyBorder(0, 5, 0, 5)));
            comp.setHorizontalAlignment(SwingConstants.RIGHT);
            if (gravityMs2 instanceof Float) {
                comp.setText(String.format(Locale.US, "%,.2fG", (float) gravityMs2 / 9.81f));
            }
            return comp;
        }

    }

    public static class PayoutCellRenderer extends DefaultTableCellRenderer {

        private static final long serialVersionUID = -5827683827505851580L;

        @Override
        public Component getTableCellRendererComponent(JTable table, Object credits, boolean isSelected, boolean hasFocus, int row, int column) {
            JLabel comp = (JLabel) super.getTableCellRendererComponent(table, credits, isSelected, hasFocus, row, column);
            comp.setBorder(BorderFactory.createCompoundBorder(comp.getBorder(), BorderFactory.createEmptyBorder(0, 5, 0, 5)));
            comp.setHorizontalAlignment(SwingConstants.RIGHT);
            if (credits instanceof Integer) {
                comp.setText(String.format(Locale.US, "%,d CR", credits));
            }
            return comp;
        }

    }

}
