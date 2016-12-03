package borg.edtrading.services;

import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;

import java.util.List;
import java.util.Map;

/**
 * EddbService
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbService {

    void setMissingCoords();

    EddbSystem searchSystemByName(String name);

    EddbSystem searchClosestSystemByCoord(Coord coord);

    List<EddbBody> retainStarsOfSpectralClasses(Map<String, List<EddbBody>> starsBySpectralClass, String... spectralClasses);

    List<EddbBody> removeStarsOfSpectralClasses(Map<String, List<EddbBody>> starsBySpectralClass, String... spectralClasses);

    Map<String, List<EddbBody>> mapStarsBySpectralClass(boolean arrivalOnly);

    List<EddbSystem> loadAllSystems();

}
