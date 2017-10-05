package borg.edtrading.services;

import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * EddbService
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbService {

    List<EddbSystem> loadAllSystems();

    // ----

    EddbSystem findSystemByName(String name);

    EddbSystem findNearestSystem(Coord coord);

    List<EddbBody> findBodiesOfSystem(Long systemId);

    // ----

    Map<String, List<EddbBody>> mapStarsBySpectralClass(boolean arrivalOnly);

    List<EddbBody> retainStarsOfSpectralClasses(Map<String, List<EddbBody>> starsBySpectralClass, String... spectralClasses);

    List<EddbBody> removeStarsOfSpectralClasses(Map<String, List<EddbBody>> starsBySpectralClass, String... spectralClasses);

    // ----

    void updateEddbData(boolean forceReindex, boolean deleteOldEntities);

    // ----

    Page<EddbSystem> findSystemsNear(Coord coord, float maxDistance, Pageable pageable);

    Page<EddbSystem> findSystemsWithin(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, Pageable pageable);

    Page<EddbBody> findBodiesNear(Coord coord, float maxDistance, Pageable pageable);

    Page<EddbBody> findBodiesWithin(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, Pageable pageable);

    Page<EddbBody> findStarsNear(Coord coord, float maxDistance, Boolean isMainStar, Collection<String> starClasses, Pageable pageable);

    Page<EddbBody> findStarsWithin(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, Boolean isMainStar, Collection<String> starClasses, Pageable pageable);

    Page<EddbBody> findPlanetsNear(Coord coord, float maxDistance, Boolean isTerraformingCandidate, Collection<Long> types, Pageable pageable);

    Page<EddbBody> findPlanetsWithin(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, Boolean isTerraformingCandidate, Collection<Long> types, Pageable pageable);

}
