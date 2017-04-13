package borg.edtrading.eddn;

import borg.edtrading.data.Coord;
import borg.edtrading.journal.entries.AbstractJournalEntry.Faction;

import java.util.Date;
import java.util.List;

/**
 * EddnListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddnListener {

    void onCommanderLocation(Date timestamp, String commanderName, String systemName, Coord systemCoords, List<Faction> systemFactions);

}
