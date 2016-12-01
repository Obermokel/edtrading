package borg.edtrading.sidepanel;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;

/**
 * StoredModules
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class StoredModules implements Serializable {

    private static final long serialVersionUID = 5880646961937450540L;

    static final Logger logger = LogManager.getLogger(StoredModules.class);

    private Map<String, List<ShipModule>> storedModules = new TreeMap<>();

    public Map<String, List<ShipModule>> getStoredModules() {
        return this.storedModules;
    }

    public void setStoredModules(Map<String, List<ShipModule>> storedModules) {
        this.storedModules = storedModules;
    }

    public void store(String stationName, ShipModule module) {
        if (stationName != null && module != null) {
            List<ShipModule> modules = this.storedModules.getOrDefault(stationName, new ArrayList<>());
            modules.add(module);
            this.storedModules.put(stationName, modules);
        }
    }

    public ShipModule retrieve(String stationName, String item, String itemLocalized) {
        ShipModule module = new ShipModule(item, itemLocalized, null);
        if (stationName != null && item != null && this.storedModules.get(stationName) != null) {
            List<ShipModule> modules = this.storedModules.get(stationName);
            ListIterator<ShipModule> it = modules.listIterator();
            while (it.hasNext()) {
                ShipModule m = it.next();
                if (item.equals(m.getKey())) {
                    module = m;
                    it.remove();
                    break;
                }
            }
            this.storedModules.put(stationName, modules);
        }
        return module;
    }

    public void sellRemote(String item, Integer sellPrice) {
        for (String stationName : this.storedModules.keySet()) {
            List<ShipModule> modules = this.storedModules.get(stationName);
            ListIterator<ShipModule> it = modules.listIterator();
            while (it.hasNext()) {
                ShipModule m = it.next();
                if (item.equals(m.getKey())) {
                    logger.trace("Sell remote (" + item + "): Bought for " + m.getBuyPrice() + ", sold for " + sellPrice);
                    it.remove();
                    return;
                }
            }
        }
    }

    public void fetchRemote(String toStationName, String item, Integer transferCost) {
        if (toStationName != null) {
            for (String stationName : this.storedModules.keySet()) {
                if (!stationName.equals(toStationName)) {
                    List<ShipModule> modules = this.storedModules.get(stationName);
                    ListIterator<ShipModule> it = modules.listIterator();
                    while (it.hasNext()) {
                        ShipModule m = it.next();
                        if (item.equals(m.getKey())) {
                            logger.trace("Fetch remote (" + item + "): Bought for " + m.getBuyPrice() + ", transferred for " + transferCost);
                            it.remove();
                            return;
                        }
                    }
                }
            }
        }
    }

}
