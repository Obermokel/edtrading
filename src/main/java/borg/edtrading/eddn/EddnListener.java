package borg.edtrading.eddn;

import borg.edtrading.data.Coord;

import java.util.Date;

/**
 * EddnListener
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddnListener {

    void onCommanderLocation(Date timestamp, String commanderName, String systemName, Coord systemCoords);

}
