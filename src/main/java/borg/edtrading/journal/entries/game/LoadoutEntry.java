package borg.edtrading.journal.entries.game;

import borg.edtrading.journal.Event;
import borg.edtrading.journal.ModuleData;
import borg.edtrading.journal.entries.AbstractJournalEntry;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * LoadoutEntry
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class LoadoutEntry extends AbstractJournalEntry {

    private static final long serialVersionUID = 4409240475722826482L;

    private final String ship;
    private final Integer shipID;
    private final String shipIdent;
    private final String shipName;
    private final List<ModuleData> modules;

    public LoadoutEntry(Date timestamp, Event event, LinkedHashMap<String, Object> data) {
        super(timestamp, event, data);

        this.ship = this.readString(data, "Ship");
        this.shipID = this.readInt(data, "ShipID");
        this.shipIdent = this.readString(data, "ShipIdent");
        this.shipName = this.readString(data, "ShipName");
        this.modules = this.readModules(data, "Modules");
    }

    public String getShip() {
        return this.ship;
    }

    public Integer getShipID() {
        return this.shipID;
    }

    public String getShipIdent() {
        return this.shipIdent;
    }

    public String getShipName() {
        return this.shipName;
    }

    public List<ModuleData> getModules() {
        return this.modules;
    }

}
