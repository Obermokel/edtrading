package borg.edtrading;

import borg.edtrading.aystar.AyStar;
import borg.edtrading.aystar.Path;
import borg.edtrading.data.Body;
import borg.edtrading.data.Coord;
import borg.edtrading.data.Galaxy;
import borg.edtrading.data.StarSystem;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalReader;
import borg.edtrading.util.FuelAndJumpRangeLookup;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.common.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringEscapeUtils.*;

/**
 * NeutronJumpApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class NeutronJumpApp {

    static final Logger logger = LogManager.getLogger(NeutronJumpApp.class);

    private static final Long TYPE_ID_NEUTRON_STAR = new Long(3);
    private static final File ROUTES_DIR = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\Routes");

    // Sol, Altair, Shinrarta Dezhra
    // Colonia, VY Canis Majoris, Crab Pulsar, Hen 2-23, Skaude AA-A h294, Sagittarius A*, Choomuia UI-K d8-4692

    public static void main(String[] args) throws IOException {
        final String fromName = "Colonia";
        final String toName = "Sol";

        final int maxFuelTons = 88;
        final float maxFuelPerJump = 8.32f;
        final float jumpRangeFuelFull = 48.30f;
        final float jumpRangeFuelOpt = 54.53f;

        final FuelAndJumpRangeLookup fuelJumpLUT = new FuelAndJumpRangeLookup(maxFuelTons, maxFuelPerJump, jumpRangeFuelFull, jumpRangeFuelOpt);

        // Lookup source and destination
        Galaxy galaxy = Galaxy.readDataFromFiles();
        StarSystem fromSystem = galaxy.searchStarSystemByName(fromName);
        StarSystem toSystem = galaxy.searchStarSystemByName(toName);
        logger.debug(String.format("%s → %s: %.0f Ly", fromSystem.toString(), toSystem.toString(), fromSystem.distanceTo(toSystem)));

        // Write a simple waypoints file
        final String baseFilename = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss").format(new Date()) + " " + fromSystem.getName().replaceAll("[^\\w\\s\\-\\+\\.]", "_") + " to " + toSystem.getName().replaceAll("[^\\w\\s\\-\\+\\.]", "_");
        writeWaypointsFile(fromSystem, toSystem, galaxy);

        // Try to find a route
        List<Body> arrivalNeutronStars = findArrivalNeutronStars(galaxy.getBodiesById().values());
        Set<StarSystem> starSystemsWithNeutronStars = bodiesToSystems(arrivalNeutronStars);
        starSystemsWithNeutronStars.addAll(findMappingProjectNeutronStars(galaxy));
        Set<StarSystem> starSystemsWithScoopableStars = new HashSet<>(galaxy.getStarSystemsById().values());
        starSystemsWithScoopableStars.removeAll(starSystemsWithNeutronStars);
        starSystemsWithScoopableStars.add(toSystem);
        Map<String, Set<StarSystem>> systemsBySpectralClass = mapSystemsBySpectralClass(galaxy.getBodiesById().values());
        logger.debug("Total known neutron stars (EDDB + Mapping Project): " + starSystemsWithNeutronStars.size());

        AyStar ayStar = new AyStar();
        ayStar.initialize(fromSystem, toSystem, starSystemsWithNeutronStars, starSystemsWithScoopableStars, fuelJumpLUT);
        final long start = System.currentTimeMillis();
        Path path = ayStar.findPath();
        final long end = System.currentTimeMillis();
        logger.info("Took " + DurationFormatUtils.formatDuration(end - start, "H:mm:ss"));
        if (path == null) {
            logger.warn("No path found");
            return;
        } else {
            logger.info("Found path with " + path.getTotalJumps() + " jumps");
        }
        List<Path> sortedPaths = path.toSortedList();

        // Read the journal to see which systems/bodies have already been discovered
        Journal journal = new Journal(JournalReader.readEntireJournal(Constants.JOURNAL_DIR));

        // Write route as VoiceAttack TXT file
        String route = routeToVoiceAttackTxt(sortedPaths, starSystemsWithNeutronStars, systemsBySpectralClass, galaxy, fuelJumpLUT);
        FileUtils.write(new File(ROUTES_DIR, baseFilename + " Route.txt"), route, "UTF-8");

        // Write route as human readable HTML file
        String html = routeToHumanReadableHtml(sortedPaths, starSystemsWithNeutronStars, systemsBySpectralClass, galaxy, fuelJumpLUT, journal);
        FileUtils.write(new File(ROUTES_DIR, baseFilename + " Route.html"), html, "UTF-8");
    }

    private static void writeWaypointsFile(StarSystem fromSystem, StarSystem toSystem, Galaxy galaxy) throws IOException {
        float directDistance = fromSystem.distanceTo(toSystem);
        if (directDistance > 999) {
            File waypointsFile = new File(ROUTES_DIR,
                    new SimpleDateFormat("yyyy-MM-dd HH-mm-ss").format(new Date()) + " Waypoints " + fromSystem.getName().replaceAll("[^\\w\\s\\-\\+\\.]", "_") + " to " + toSystem.getName().replaceAll("[^\\w\\s\\-\\+\\.]", "_") + ".txt");
            int waypointsNeeded = (int) (directDistance / 888) + 1;
            float waypointSeparation = directDistance / waypointsNeeded;
            FileUtils.write(waypointsFile, String.format(Locale.US, "Direct distance: %.0f Ly\nWaypoints needed: %d\nWaypoint separation: %.0f Ly\n\n", directDistance, waypointsNeeded, waypointSeparation), "UTF-8", false);
            Coord fromCoord = fromSystem.getCoord();
            Coord toCoord = toSystem.getCoord();
            float stepX = (toCoord.getX() - fromCoord.getX()) / waypointsNeeded;
            float stepY = (toCoord.getY() - fromCoord.getY()) / waypointsNeeded;
            float stepZ = (toCoord.getZ() - fromCoord.getZ()) / waypointsNeeded;
            for (int wp = 1; wp <= waypointsNeeded; wp++) {
                Coord coord = new Coord(fromCoord.getX() + wp * stepX, fromCoord.getY() + wp * stepY, fromCoord.getZ() + wp * stepZ);
                StarSystem closestSystem = galaxy.searchClosestStarSystemByName(coord);
                FileUtils.write(waypointsFile, String.format(Locale.US, "Waypoint %2d: %s\n", wp, closestSystem.getName()), "UTF-8", true);
            }
        }
    }

    private static String routeToVoiceAttackTxt(List<Path> sortedPaths, Set<StarSystem> starSystemsWithNeutronStars, Map<String, Set<StarSystem>> systemsBySpectralClass, Galaxy galaxy, FuelAndJumpRangeLookup fuelJumpLUT) {
        StringBuilder route = new StringBuilder();

        int jumpNo = 0;
        Path prevPath = null;
        for (Path currPath : sortedPaths) {
            jumpNo++;
            StarSystem prevSystem = prevPath == null ? null : prevPath.getStarSystem();
            StarSystem currSystem = currPath.getStarSystem();
            if (prevPath != null) {
                route.append("\n");
            }
            String name = currSystem.getName();
            float distance = prevPath != null ? currSystem.distanceTo(prevSystem) : 0;
            String spectralClass = lookupSpectralClass(currSystem, starSystemsWithNeutronStars, systemsBySpectralClass);
            String flags = "";
            if (name.replaceAll("[^\\-]", "").length() < 2) {
                flags += "N"; // Pron name
            }
            if (jumpNo == sortedPaths.size() / 2) {
                flags += "H"; // Halfway
            }
            if (findValuableBodies(currSystem, galaxy).size() > 0) {
                flags += "P"; // Planets
            }
            if (currPath.getFuelLevel() <= (fuelJumpLUT.getMaxFuelPerJump() + 2)) {
                flags += "F"; // Fuel
            }
            // TODO S
            route.append(String.format(Locale.US, "%-50s%5.0f%10s%10s", name.replace("'", " "), distance, spectralClass, flags));
            prevPath = currPath;
        }

        return route.toString();
    }

    private static String routeToHumanReadableHtml(List<Path> sortedPaths, Set<StarSystem> starSystemsWithNeutronStars, Map<String, Set<StarSystem>> systemsBySpectralClass, Galaxy galaxy, FuelAndJumpRangeLookup fuelJumpLUT, Journal journal) {
        StringBuilder html = new StringBuilder();

        Date eddbDumpDate = new Date(Constants.BODIES_FILE.lastModified());
        StarSystem fromSystem = sortedPaths.get(0).getStarSystem();
        StarSystem toSystem = sortedPaths.get(sortedPaths.size() - 1).getStarSystem();
        String fromName = fromSystem.getName();
        String toName = toSystem.getName();
        float directDistance = fromSystem.distanceTo(toSystem);
        float routeDistance = sortedPaths.get(sortedPaths.size() - 1).getTravelledDistanceLy();
        int jumpsUsingHighway = sortedPaths.size();
        int jumpsTraditional = Math.round(directDistance / fuelJumpLUT.getJumpRangeFuelFull());
        int jumpsSaved = jumpsTraditional - jumpsUsingHighway;
        float jumpsSavedPercent = 100f * jumpsSaved / jumpsTraditional;

        String title = escapeHtml4(String.format(Locale.US, "%s → %s (%.0f Ly, %d jumps)", fromName, toName, directDistance, jumpsUsingHighway));
        String h2 = String.format(Locale.US, "EDDB data from %td-%tb-%tY, %d known systems with neutron stars", eddbDumpDate, eddbDumpDate, eddbDumpDate, starSystemsWithNeutronStars.size());
        String h3 = String.format(Locale.US, "Jump range: %.1f to %.1f Ly | Fuel usage: Max %.2f of %d tons | Jumps saved: %d of %d (%.0f%%)", fuelJumpLUT.getJumpRangeFuelFull(), fuelJumpLUT.getJumpRangeFuelOpt(), fuelJumpLUT.getMaxFuelPerJump(),
                fuelJumpLUT.getMaxFuelTons(), jumpsSaved, jumpsTraditional, jumpsSavedPercent);
        html.append("<html>\n");
        html.append("<head>\n");
        html.append("<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n");
        html.append("<link href=\"route.css\" rel=\"stylesheet\" type=\"text/css\" />\n");
        html.append("<title>").append(title).append("</title>\n");
        html.append("</head>\n");
        html.append("<body>\n");
        html.append("<h1>").append(title).append("</h1>\n");
        html.append("<h2>").append(h2).append("</h2>\n");
        html.append("<h3>").append(h3).append("</h3>\n");
        html.append("<table id=\"jumpTable\">\n");
        html.append("<tr>");
        html.append("<th class=\"numeric jumpNo\">#</th>");
        html.append("<th class=\"starName\">From</th>");
        html.append("<th class=\"starClass\">Class</th>");
        html.append("<th class=\"numeric jumpDistance\">Jump</th>");
        html.append("<th class=\"starClass\">Class</th>");
        html.append("<th class=\"starName\">To</th>");
        html.append("<th class=\"notes\">Notes</th>");
        html.append("<th class=\"numeric distance\">Dist</th>");
        html.append("</tr>\n");
        int jumpNo = 0;
        float travelledLy = 0;
        boolean halfwayPassed = false;
        Path prevPath = null;
        for (Path currPath : sortedPaths) {
            if (prevPath != null) {
                jumpNo++;
                StarSystem prevSystem = prevPath.getStarSystem();
                StarSystem currSystem = currPath.getStarSystem();
                float jumpDistance = currSystem.distanceTo(prevSystem);
                travelledLy += jumpDistance;
                String neutronJumpCss = starSystemsWithNeutronStars.contains(prevSystem) ? "neutronJump" : "normalJump";
                String evenOddCss = jumpNo % 2 == 0 ? "even" : "odd";
                String prevName = prevSystem.getName();
                String prevClass = lookupSpectralClass(prevSystem, starSystemsWithNeutronStars, systemsBySpectralClass);
                String prevKnownCss = journal.getVisitedSystems().contains(prevName) ? " known" : "";
                String currName = currSystem.getName();
                String currClass = lookupSpectralClass(currSystem, starSystemsWithNeutronStars, systemsBySpectralClass);
                String currKnownCss = journal.getVisitedSystems().contains(currName) ? " known" : "";
                String flags = "";
                String notes = "";
                if (currName.replaceAll("[^\\-]", "").length() < 2) {
                    flags += "N"; // Pron name
                }
                if (travelledLy >= routeDistance / 2 && !halfwayPassed) {
                    flags += "H"; // Halfway
                    halfwayPassed = true;
                }
                if (currPath.getFuelLevel() <= (fuelJumpLUT.getMaxFuelPerJump() + 2)) {
                    flags += "F";
                    notes += "<span class=\"fuelWarning\">" + String.format(Locale.US, "%.1ft", currPath.getFuelLevel()) + "</span>";
                }
                if (findValuableBodies(currSystem, galaxy).size() > 0) {
                    flags += "P"; // Planets
                    for (Body body : findValuableBodies(currSystem, galaxy)) {
                        String knownCss = journal.getScannedBodies().contains(body.getName()) ? "known" : "";
                        String typeCss = body.getTypeName().toLowerCase().replaceAll("\\W", "-");
                        notes += "<span class=\"valuablePlanet " + typeCss + " " + knownCss + "\">" + escapeHtml4(body.getName()) + "</span>";
                    }
                }
                // TODO S
                //float routePercent = 100f * jumpNo / (sortedSystems.size() - 1);
                html.append("<tr class=\"" + evenOddCss + " " + neutronJumpCss + "\">");
                html.append("<td class=\"numeric jumpNo\">" + jumpNo + "</td>");
                html.append("<td class=\"starName spectralClass-" + prevClass + prevKnownCss + "\">" + escapeHtml4(prevName) + "</td>");
                html.append("<td class=\"starClass spectralClass-" + prevClass + "\">" + prevClass + "</td>");
                html.append("<td class=\"numeric jumpDistance\">" + String.format(Locale.US, "%.1f Ly", jumpDistance) + "</td>");
                html.append("<td class=\"starClass spectralClass-" + currClass + "\">" + currClass + "</td>");
                html.append("<td class=\"starName spectralClass-" + currClass + currKnownCss + "\">" + escapeHtml4(currName) + "</td>");
                html.append("<td class=\"notes\">" + "[" + flags + "] " + notes + "</td>");
                html.append("<td class=\"numeric distance\">" + String.format(Locale.US, "%.0f Ly", travelledLy) + "</td>");
                html.append("</tr>\n");
            }
            prevPath = currPath;
        }
        html.append("</table>\n");
        html.append("</body>\n");
        html.append("</html>");

        return html.toString();
    }

    private static String lookupSpectralClass(StarSystem system, Set<StarSystem> starSystemsWithNeutronStars, Map<String, Set<StarSystem>> systemsBySpectralClass) {
        if (starSystemsWithNeutronStars.contains(system)) {
            return "NS";
        } else {
            for (String spectralClass : systemsBySpectralClass.keySet()) {
                if (systemsBySpectralClass.get(spectralClass).contains(system)) {
                    return spectralClass;
                }
            }
            return "?";
        }
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

        logger.trace("Found " + result.size() + " of all " + ids.size() + " IDs");

        return result;
    }

    private static Set<StarSystem> findStarSystemsWithName(Collection<StarSystem> starSystems, List<String> names) {
        Set<StarSystem> result = new HashSet<>(names.size());

        List<String> lowercaseNames = names.stream().map(n -> n.toLowerCase()).collect(Collectors.toList());
        for (StarSystem starSystem : starSystems) {
            for (String name : lowercaseNames) {
                if (name.startsWith(starSystem.getName().toLowerCase())) {
                    if (StringUtils.getLevenshteinDistance(name, starSystem.getName().toLowerCase()) <= 4) {
                        result.add(starSystem);
                    }
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

        logger.trace(result.size() + " of all " + bodies.size() + " bodies are arrival neutron stars");

        return result;
    }

    private static Set<StarSystem> bodiesToSystems(Collection<Body> bodies) {
        Set<StarSystem> result = new HashSet<>(bodies.size());

        for (Body body : bodies) {
            if (body.getStarSystem() != null) {
                result.add(body.getStarSystem());
            }
        }

        logger.trace(result.size() + " of all " + bodies.size() + " bodies have a known star system");

        return result;
    }

    private static Map<String, Set<StarSystem>> mapSystemsBySpectralClass(Collection<Body> bodies) {
        List<Body> bodiesHavingSpectralClass = bodies.stream().filter(b -> b.getSpectral_class() != null).collect(Collectors.toList());
        List<Body> arrivalBodiesHavingSpectralClass = bodiesHavingSpectralClass.stream().filter(b -> Boolean.TRUE.equals(b.getIs_main_star())).collect(Collectors.toList());
        List<Body> scoopableArrivalStars = new ArrayList<>(arrivalBodiesHavingSpectralClass.size() / 2);

        Map<String, List<Body>> bodiesBySpectralClass = arrivalBodiesHavingSpectralClass.stream().collect(Collectors.groupingBy(Body::getSpectral_class));
        for (String spectralClass : bodiesBySpectralClass.keySet()) {
            float spectralClassPercentOfAllArrivalStars = (100f * bodiesBySpectralClass.get(spectralClass).size()) / arrivalBodiesHavingSpectralClass.size();
            logger.trace(String.format("Spectral class %-20s: %d bodies (%.1f%%)", spectralClass, bodiesBySpectralClass.get(spectralClass).size(), spectralClassPercentOfAllArrivalStars));
            if (Constants.SCOOPABLE_SPECTRAL_CLASSES.contains(spectralClass)) {
                scoopableArrivalStars.addAll(bodiesBySpectralClass.get(spectralClass));
            }
        }
        logger.debug(String.format(Locale.US, "Having a spectral class:  %9d (%.1f%% of all bodies)", bodiesHavingSpectralClass.size(), (100f * bodiesHavingSpectralClass.size()) / bodies.size()));
        logger.debug(String.format(Locale.US, "Being arrival star:       %9d (%.1f%% of having spectral class)", arrivalBodiesHavingSpectralClass.size(), (100f * arrivalBodiesHavingSpectralClass.size()) / bodiesHavingSpectralClass.size()));
        logger.debug(String.format(Locale.US, "Scoopable:                %9d (%.1f%% of being arrival)", scoopableArrivalStars.size(), (100f * scoopableArrivalStars.size()) / arrivalBodiesHavingSpectralClass.size()));

        Map<String, Set<StarSystem>> result = new LinkedHashMap<>();
        for (String spectralClass : bodiesBySpectralClass.keySet()) {
            result.put(spectralClass, bodiesBySpectralClass.get(spectralClass).stream().filter(b -> b.getStarSystem() != null).map(Body::getStarSystem).collect(Collectors.toSet()));
        }
        return result;
    }

    static List<Body> findValuableBodies(StarSystem system, Galaxy galaxy) {
        List<Body> result = new ArrayList<>(0);
        for (Body body : galaxy.searchBodiesOfStarSystem(system.getId())) {
            if (StringUtils.isNotEmpty(body.getTerraforming_state_name()) && !"Not terraformable".equals(body.getTerraforming_state_name())) {
                result.add(body);
            } else if (StringUtils.isNotEmpty(body.getTypeName())) {
                if ("Ammonia world".equals(body.getTypeName()) || "Earth-like world".equals(body.getTypeName()) || "Water giant".equals(body.getTypeName()) || "Water world".equals(body.getTypeName())) {
                    result.add(body);
                } else if (body.getTypeName().endsWith("life")) {
                    result.add(body);
                }
            }
        }
        return result;
    }

}
