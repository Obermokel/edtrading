package borg.edtrading;

import borg.edtrading.aystar.AyStar;
import borg.edtrading.aystar.Path;
import borg.edtrading.data.Body;
import borg.edtrading.data.Galaxy;
import borg.edtrading.data.StarSystem;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * NeutronJumpApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class NeutronJumpApp {

    static final Logger logger = LogManager.getLogger(NeutronJumpApp.class);

    private static final Long TYPE_ID_NEUTRON_STAR = new Long(3);

    public static void main(String[] args) throws IOException {
        Galaxy galaxy = Galaxy.readDataFromFiles();
        logger.debug(galaxy.getStarSystemsById().size() + " star systems");

        StarSystem sourceSystem = galaxy.searchStarSystemByExactName("Altair"); // Altair, Boewnst KS-S c20-959
        logger.debug("From: " + sourceSystem);

        StarSystem targetSystem = galaxy.searchStarSystemByExactName("Colonia"); // Colonia, VY Canis Majoris, Crab Pulsar, Hen 2-23, Skaude AA-A h294
        logger.debug("To: " + targetSystem);

        float directDistanceSourceToTarget = sourceSystem.distanceTo(targetSystem);
        logger.debug("Direct distance: " + String.format("%.0fly", directDistanceSourceToTarget));

        List<Body> arrivalNeutronStars = findArrivalNeutronStars(galaxy.getBodiesById().values());
        Set<StarSystem> starSystemsWithNeutronStars = bodiesToSystems(arrivalNeutronStars);
        starSystemsWithNeutronStars.addAll(findMappingProjectNeutronStars(galaxy));
        Set<StarSystem> starSystemsWithScoopableStars = new HashSet<>(galaxy.getStarSystemsById().values());
        starSystemsWithScoopableStars.removeAll(starSystemsWithNeutronStars);
        starSystemsWithScoopableStars.add(targetSystem);
        logger.debug("Total known neutron stars (EDDB + Mapping Project): " + starSystemsWithNeutronStars.size());

        //mapBodiesByTypeId(galaxy.getBodiesById().values());

        final long start = System.currentTimeMillis();
        AyStar ayStar = new AyStar();
        ayStar.initialize(sourceSystem, targetSystem, starSystemsWithNeutronStars, starSystemsWithScoopableStars, 47.5f, 7);
        Path path = ayStar.findPath();
        if (path == null) {
            logger.warn("No path found");
        } else {
            logger.info("Found path with " + path.getTotalJumps() + " jumps:");
            Path p = path;
            while (p != null) {
                String extraDistanceLy = "";
                if (p.getPrev() != null) {
                    extraDistanceLy = " (+" + String.format("%.0fly", p.getStarSystem().distanceTo(p.getPrev().getStarSystem())) + ")";
                }
                logger.info(String.format("Jump #%-3d\t%-30s\tDistance: %5.0fly", p.getTotalJumps(), p.getStarSystem().getName(), p.getTravelledDistanceLy()) + extraDistanceLy);
                p = p.getPrev();
            }
        }
        final long end = System.currentTimeMillis();
        final long millis = end - start;
        logger.info("Took " + DurationFormatUtils.formatDuration(millis, "H:mm:ss"));
    }

    private static Set<StarSystem> findMappingProjectNeutronStars(Galaxy galaxy) throws IOException {
        File neutronStarNamesFile = new File(Constants.EDTRADING_BASE_DIR, "neutron stars.txt");
        File neutronStarIdsFile = new File(Constants.EDTRADING_BASE_DIR, "neutron stars.dat");
        Set<StarSystem> neutronStars = null;
        if (!neutronStarIdsFile.exists() || neutronStarNamesFile.lastModified() > neutronStarIdsFile.lastModified()) {
            neutronStars = findStarSystemsWithName(galaxy.getStarSystemsById().values(), FileUtils.readLines(neutronStarNamesFile, "UTF-8"));
            FileUtils.write(neutronStarIdsFile, neutronStars.stream().map(ss -> String.valueOf(ss.getId())).collect(Collectors.joining("\n")), "UTF-8", false);
        } else {
            neutronStars = findStarSystemsWithId(galaxy.getStarSystemsById(), FileUtils.readLines(neutronStarIdsFile, "UTF-8"));
        }
        return neutronStars;
    }

    private static Set<StarSystem> findStarSystemsWithId(Map<Long, StarSystem> starSystemsById, List<String> ids) {
        Set<StarSystem> result = new HashSet<>(ids.size());

        List<Long> longIds = ids.stream().map(n -> Long.valueOf(n)).collect(Collectors.toList());
        for (Long id : longIds) {
            StarSystem starSystem = starSystemsById.get(id);
            if (starSystem != null) {
                result.add(starSystem);
            }
        }

        logger.debug("Found " + result.size() + " of all " + ids.size() + " IDs");

        return result;
    }

    private static Set<StarSystem> findStarSystemsWithName(Collection<StarSystem> starSystems, List<String> names) {
        Set<StarSystem> result = new HashSet<>(names.size());

        List<String> lowercaseNames = names.stream().map(n -> n.toLowerCase()).collect(Collectors.toList());
        for (StarSystem starSystem : starSystems) {
            for (String name : lowercaseNames) {
                if (name.startsWith(starSystem.getName().toLowerCase())) {
                    result.add(starSystem);
                }
            }
        }

        logger.debug("Found " + result.size() + " of all " + names.size() + " names");

        return result;
    }

    private static List<Body> findArrivalNeutronStars(Collection<Body> bodies) {
        List<Body> result = new ArrayList<>();

        for (Body body : bodies) {
            if (TYPE_ID_NEUTRON_STAR.equals(body.getTypeId())) {
                if (Boolean.TRUE.equals(body.getIs_main_star())) {
                    result.add(body);
                }
            }
        }

        logger.debug(result.size() + " of all " + bodies.size() + " bodies are arrival neutron stars");

        return result;
    }

    private static Set<StarSystem> bodiesToSystems(Collection<Body> bodies) {
        Set<StarSystem> result = new HashSet<>(bodies.size());

        for (Body body : bodies) {
            if (body.getStarSystem() != null) {
                result.add(body.getStarSystem());
            }
        }

        logger.debug(result.size() + " of all " + bodies.size() + " bodies have a known star system");

        return result;
    }

    private static void mapBodiesByTypeId(Collection<Body> bodies) {
        Map<Long, List<Body>> bodiesByTypeId = bodies.stream().filter(b -> b.getTypeId() != null).collect(Collectors.groupingBy(Body::getTypeId));
        for (Long typeId : bodiesByTypeId.keySet()) {
            Map<String, List<Body>> bodiesBySpectralClass = bodiesByTypeId.get(typeId).stream().filter(b -> b.getSpectral_class() != null).collect(Collectors.groupingBy(Body::getSpectral_class));
            logger.debug(String.format("Type ID %4d has %d bodies with %d different spectral classes: %s", typeId, bodiesByTypeId.get(typeId).size(), bodiesBySpectralClass.size(), bodiesBySpectralClass.keySet().toString()));
        }

        List<String> scoopableSpectralClasses = Arrays.asList("O", "B", "A", "F", "G", "K", "M");
        List<Body> bodiesHavingSpectralClass = bodies.stream().filter(b -> b.getSpectral_class() != null).collect(Collectors.toList());
        List<Body> bodiesHavingScoopableSpectralClass = new ArrayList<>(bodiesHavingSpectralClass.size() / 2);
        Map<String, List<Body>> bodiesBySpectralClass = bodiesHavingSpectralClass.stream().collect(Collectors.groupingBy(Body::getSpectral_class));
        for (String spectralClass : bodiesBySpectralClass.keySet()) {
            float percent = (100f * bodiesBySpectralClass.get(spectralClass).size()) / bodiesHavingSpectralClass.size();
            logger.debug(String.format("Spectral class %-20s: %d bodies (%.1f%%)", spectralClass, bodiesBySpectralClass.get(spectralClass).size(), percent));
            if (scoopableSpectralClasses.contains(spectralClass)) {
                bodiesHavingScoopableSpectralClass.addAll(bodiesBySpectralClass.get(spectralClass));
            }
        }
        logger.debug(String.format("Bodies having a spectral class at all:    %d (%.1f%% of all bodies)", bodiesHavingSpectralClass.size(), (100f * bodiesHavingSpectralClass.size()) / bodies.size()));
        logger.debug(String.format("Bodies having a scoopable spectral class: %d (%.1f%% of bodies having spectral class)", bodiesHavingScoopableSpectralClass.size(), (100f * bodiesHavingScoopableSpectralClass.size()) / bodiesHavingSpectralClass.size()));
    }

}
