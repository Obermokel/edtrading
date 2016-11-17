package borg.edtrading.sidepanel;

import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.util.Locale;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * ShipModuleList
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class ShipModuleList implements GameSessionListener {

    static final Logger logger = LogManager.getLogger(ShipModuleList.class);

    private SortedSet<ShipModule> knownShipModules = new TreeSet<>();

    public ShipModuleList(GameSession gameSession) {
        gameSession.addListener(this);
    }

    @Override
    public void onGameLoaded(String commander, String gameMode, String group, ShipLoadout ship) {
        // Do nothing
    }

    @Override
    public void onShipModuleChanged(ShipModule oldModule, ShipModule newModule) {
        if (oldModule != null) {
            this.knownShipModules.add(oldModule);
        }
        if (newModule != null) {
            this.knownShipModules.add(newModule);
        }
        File knownShipModulesFile = new File(System.getProperty("user.home"), ".knownShipModules.txt");
        try {
            FileUtils.write(knownShipModulesFile, String.format(Locale.US, "%-75s%-35s%10s\n", "KEY", "NAME", "PRICE"), "UTF-8", false);
            for (ShipModule m : this.knownShipModules) {
                FileUtils.write(knownShipModulesFile, String.format(Locale.US, "%-75s%-35s%10d\n", m.getKey(), m.getName(), m.getBuyPrice()), "UTF-8", true);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void onShipChanged(ShipLoadout oldShip, ShipLoadout newShip) {
        // Do nothing
    }

}
