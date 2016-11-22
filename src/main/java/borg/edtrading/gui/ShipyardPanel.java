package borg.edtrading.gui;

import borg.edtrading.sidepanel.GameSession;
import borg.edtrading.sidepanel.GameSessionListener;
import borg.edtrading.sidepanel.ShipLoadout;
import borg.edtrading.sidepanel.ShipModule;
import borg.edtrading.util.MiscUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Locale;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * ShipyardPanel
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ShipyardPanel extends JPanel implements GameSessionListener, ActionListener {

    private static final long serialVersionUID = 5950728889096883891L;

    static final Logger logger = LogManager.getLogger(ShipyardPanel.class);

    private final GameSession gameSession;

    private JLabel lblShipID = new JLabel();
    private JLabel lblShipType = new JLabel();
    private JTextField txtShipName = new JTextField(32);
    private JTextField txtBuyPrice = new JTextField(8);
    private JTextField txtMaxFuelPerJump = new JTextField(4);
    private JTextField txtOptFuelJumpRange = new JTextField(4);
    private JTextField txtMaxFuelJumpRange = new JTextField(4);
    private JButton btnSave = new JButton("Save");

    public ShipyardPanel(GameSession gameSession) {
        this.gameSession = gameSession;

        this.btnSave.addActionListener(this);

        this.setLayout(new BorderLayout());

        JPanel topPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 25, 5));
        topPanel.add(group(new JLabel("Ship:"), this.lblShipID, this.lblShipType));
        topPanel.add(group(new JLabel("Name:"), this.txtShipName));
        topPanel.add(group(new JLabel("Buy price:"), this.txtBuyPrice));
        topPanel.add(group(new JLabel("Max fuel per jump [t]:"), this.txtMaxFuelPerJump));
        topPanel.add(group(new JLabel("Full tank jump range [Ly]:"), this.txtMaxFuelJumpRange));
        topPanel.add(group(new JLabel("Optimum tank jump range [Ly]:"), this.txtOptFuelJumpRange));
        this.add(topPanel, BorderLayout.NORTH);

        JPanel centerPanel = new JPanel();
        centerPanel.add(new JLabel("TODO..."));
        this.add(centerPanel, BorderLayout.CENTER);

        JPanel bottomPanel = new JPanel();
        bottomPanel.add(this.btnSave);
        this.add(bottomPanel, BorderLayout.SOUTH);

        this.updatePanel();
        gameSession.addListener(this);
    }

    private static JPanel group(Component... components) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        for (Component c : components) {
            panel.add(c);
        }
        return panel;
    }

    @Override
    public void onGameLoaded(String commander, String gameMode, String group, ShipLoadout ship) {
        this.updatePanel();
    }

    @Override
    public void onShipModuleChanged(ShipModule oldModule, ShipModule newModule) {
        this.updatePanel();
    }

    @Override
    public void onShipChanged(ShipLoadout oldShip, ShipLoadout newShip) {
        this.updatePanel();
    }

    private void updatePanel() {
        ShipLoadout lo = this.gameSession.getCurrentShipLoadout();
        if (lo != null) {
            this.lblShipID.setText("#" + lo.getShipID());
            this.lblShipType.setText("(" + lo.getShipType() + ")");
            this.txtShipName.setText(MiscUtil.getAsString(lo.getShipName(), ""));
            this.txtBuyPrice.setText(String.format(Locale.US, "%d", MiscUtil.getAsInt(lo.getBuyPrice(), 0)));
            this.txtMaxFuelPerJump.setText(String.format(Locale.US, "%.2f", MiscUtil.getAsFloat(lo.getMaxFuelPerJump(), 0.0f)));
            this.txtOptFuelJumpRange.setText(String.format(Locale.US, "%.2f", MiscUtil.getAsFloat(lo.getOptTankJumpRange(), 0.0f)));
            this.txtMaxFuelJumpRange.setText(String.format(Locale.US, "%.2f", MiscUtil.getAsFloat(lo.getFullTankJumpRange(), 0.0f)));
        }
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == this.btnSave) {
            ShipLoadout lo = this.gameSession.getCurrentShipLoadout();
            if (lo != null) {
                try {
                    lo.setShipName(this.txtShipName.getText());
                    lo.setBuyPrice(MiscUtil.getAsInt(this.txtBuyPrice.getText(), 0));
                    lo.setMaxFuelPerJump(MiscUtil.getAsFloat(this.txtMaxFuelPerJump.getText(), 0.0f));
                    lo.setOptTankJumpRange(MiscUtil.getAsFloat(this.txtOptFuelJumpRange.getText(), 0.0f));
                    lo.setFullTankJumpRange(MiscUtil.getAsFloat(this.txtMaxFuelJumpRange.getText(), 0.0f));
                    this.gameSession.saveShipLoadout(this.gameSession.getCommander(), lo);
                } catch (Exception ex) {
                    logger.error("Failed to save " + lo, ex);
                }
            }
        }
    }

}
