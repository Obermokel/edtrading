package borg.edtrading;

import borg.edtrading.aystar.AyStar;
import borg.edtrading.aystar.Path;
import borg.edtrading.data.Body;
import borg.edtrading.data.Galaxy;
import borg.edtrading.data.StarSystem;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * NeutronJumpApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class NeutronJumpApp {

    static final Logger logger = LogManager.getLogger(NeutronJumpApp.class);

    public static void main(String[] args) throws IOException {
        Galaxy galaxy = Galaxy.readDataFromFiles();
        logger.debug(galaxy.getStarSystemsById().size() + " star systems");

        StarSystem sourceSystem = galaxy.searchStarSystemByExactName("Altair");
        logger.debug("From: " + sourceSystem);

        StarSystem targetSystem = galaxy.searchStarSystemByExactName("Colonia"); // Eol Prou RS-T d3-94
        logger.debug("To: " + targetSystem);

        double directDistanceSourceToTarget = sourceSystem.distanceTo(targetSystem);
        logger.debug("Direct distance: " + String.format("%.0fly", directDistanceSourceToTarget));

        Long typeIdNeutronStar = new Long(3);
        List<String> scoopableSpectralClasses = Arrays.asList("O", "B", "A", "F", "G", "K", "M");
        List<Body> arrivalNeutronStars = new ArrayList<>();
        List<Body> distantNeutronStars = new ArrayList<>();
        List<Body> unknownNeutronStars = new ArrayList<>();
        List<Body> suspiciousNeutronStars = new ArrayList<>();
        List<Body> arrivalScoopableStars = new ArrayList<>();
        for (Body body : galaxy.getBodiesById().values()) {
            if (typeIdNeutronStar.equals(body.getTypeId())) {
                //logger.debug(String.format("%-30s\t%-15s\t%8d\t%s\t%6.0fly", body.getName(), body.getTypeName(), body.getDistance_to_arrival(), body.getIs_main_star(), body.getStarSystem() == null ? null : body.getStarSystem().distanceTo(systemAltair)));
                if (Boolean.TRUE.equals(body.getIs_main_star())) {
                    arrivalNeutronStars.add(body);
                } else if (Boolean.FALSE.equals(body.getIs_main_star())) {
                    distantNeutronStars.add(body);
                } else {
                    unknownNeutronStars.add(body);
                }
            } else if (body.getTypeName() != null && body.getTypeName().toLowerCase().contains("neutron")) {
                //logger.warn(body);
                suspiciousNeutronStars.add(body);
            } else {
                if (scoopableSpectralClasses.contains(body.getSpectral_class()) && Boolean.TRUE.equals(body.getIs_main_star())) {
                    arrivalScoopableStars.add(body);
                }
            }
        }
        List<Body> allNeutronStars = new ArrayList<>();
        allNeutronStars.addAll(arrivalNeutronStars);
        allNeutronStars.addAll(distantNeutronStars);
        allNeutronStars.addAll(unknownNeutronStars);
        allNeutronStars.addAll(suspiciousNeutronStars);
        logger.debug("arrival:     " + arrivalNeutronStars.size());
        logger.debug("distant:     " + distantNeutronStars.size());
        logger.debug("unknown:     " + unknownNeutronStars.size());
        logger.debug("suspicious:  " + suspiciousNeutronStars.size());
        logger.debug("all:         " + allNeutronStars.size());

        //final double maxTotalDistance = 1.10 * directDistanceSourceToTarget;
        final double tenPercentDirectDistance = 0.1 * directDistanceSourceToTarget;
        List<Body> usableNeutronStars = new ArrayList<>();
        for (Body body : arrivalNeutronStars) {
            if (body.getStarSystem() != null) {
                double wayPercent = body.getStarSystem().distanceTo(sourceSystem) / directDistanceSourceToTarget; // 0 .. 1
                wayPercent -= 0.5; // -0.5 .. 0.0 .. +0.5
                wayPercent = -1 * Math.abs(wayPercent); // -0.5 .. 0.0 .. -0.5
                wayPercent += 0.5; // 0.0 .. 0.5 .. 0.0
                wayPercent *= 2; // 0.0 .. 1.0 .. 0.0
                double maxTotalDistance = directDistanceSourceToTarget + 10 + wayPercent * tenPercentDirectDistance; // Always +10ly and the closer to halfway the more of a 10% extra

                double fromSource = body.getStarSystem().distanceTo(sourceSystem);
                double toTarget = body.getStarSystem().distanceTo(targetSystem);
                if (fromSource + toTarget <= maxTotalDistance) {
                    usableNeutronStars.add(body);
                }
            }
        }
        logger.debug("usable:      " + usableNeutronStars.size());
        Set<StarSystem> starSystemsWithNeutronStars = usableNeutronStars.stream().map(b -> b.getStarSystem()).collect(Collectors.toSet());
        List<Body> usableScoopableStars = new ArrayList<>();
        for (Body body : arrivalScoopableStars) {
            if (body.getStarSystem() != null) {
                double wayPercent = body.getStarSystem().distanceTo(sourceSystem) / directDistanceSourceToTarget; // 0 .. 1
                wayPercent -= 0.5; // -0.5 .. 0.0 .. +0.5
                wayPercent = -1 * Math.abs(wayPercent); // -0.5 .. 0.0 .. -0.5
                wayPercent += 0.5; // 0.0 .. 0.5 .. 0.0
                wayPercent *= 2; // 0.0 .. 1.0 .. 0.0
                double maxTotalDistance = directDistanceSourceToTarget + 10 + wayPercent * tenPercentDirectDistance; // Always +10ly and the closer to halfway the more of a 10% extra

                double fromSource = body.getStarSystem().distanceTo(sourceSystem);
                double toTarget = body.getStarSystem().distanceTo(targetSystem);
                if (fromSource + toTarget <= maxTotalDistance) {
                    usableScoopableStars.add(body);
                }
            }
        }
        logger.debug("scoopable:   " + arrivalScoopableStars.size());
        Set<StarSystem> starSystemsWithScoopableStars = usableScoopableStars.stream().map(b -> b.getStarSystem()).collect(Collectors.toSet());

        // >>>> start dirty >>>>
        starSystemsWithScoopableStars = new HashSet<>();
        for (StarSystem system : galaxy.getStarSystemsById().values()) {
            if (!starSystemsWithNeutronStars.contains(system)) {
                double wayPercent = system.distanceTo(sourceSystem) / directDistanceSourceToTarget; // 0 .. 1
                wayPercent -= 0.5; // -0.5 .. 0.0 .. +0.5
                wayPercent = -1 * Math.abs(wayPercent); // -0.5 .. 0.0 .. -0.5
                wayPercent += 0.5; // 0.0 .. 0.5 .. 0.0
                wayPercent *= 2; // 0.0 .. 1.0 .. 0.0
                double maxTotalDistance = directDistanceSourceToTarget + 10 + wayPercent * tenPercentDirectDistance; // Always +10ly and the closer to halfway the more of a 10% extra

                double fromSource = system.distanceTo(sourceSystem);
                double toTarget = system.distanceTo(targetSystem);
                if (fromSource + toTarget <= maxTotalDistance) {
                    starSystemsWithScoopableStars.add(system);
                }
            }
        }
        logger.debug("scoopable:   " + starSystemsWithScoopableStars.size());
        // <<<< end dirty <<<<

        starSystemsWithScoopableStars.add(targetSystem); // Do not care

        AyStar ayStar = new AyStar();
        ayStar.initialize(sourceSystem, targetSystem, starSystemsWithNeutronStars, starSystemsWithScoopableStars, 47.5, 7);
        Path path = ayStar.findPath();
        if (path == null) {
            logger.warn("No path found");
        } else {
            logger.info("Found path with " + path.getTotalHops() + " jumps:");
            Path p = path;
            while (p != null) {
                String extraDistanceLy = "";
                if (p.getPrev() != null) {
                    extraDistanceLy = " (+" + String.format("%.0fly", p.getStarSystem().distanceTo(p.getPrev().getStarSystem())) + ")";
                }
                logger.info(String.format("Jump #%-3d\t%-24s\tDistance: %5.0fly", p.getTotalHops(), p.getStarSystem().getName(), p.getTotalDistanceLy()) + extraDistanceLy);
                p = p.getPrev();
            }
        }
    }

}
