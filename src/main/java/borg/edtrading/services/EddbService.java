package borg.edtrading.services;

import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * EddbService
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbService {

    EddbSystem searchSystemByName(String name);

    EddbSystem searchClosestSystemByCoord(Coord coord);

    List<EddbBody> searchArrivalNeutronStars();

    List<EddbBody> searchArrivalUnscoopableStars();

    Map<String, Set<EddbBody>> mapStarsBySpectralClass(boolean arrivalOnly);

    List<EddbSystem> loadAllSystems();

}
