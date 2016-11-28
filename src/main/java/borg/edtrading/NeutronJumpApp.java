package borg.edtrading;

import borg.edtrading.aystar.AyStar;
import borg.edtrading.aystar.Path;
import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddb.repositories.EddbBodyRepository;
import borg.edtrading.eddb.repositories.EddbSystemRepository;
import borg.edtrading.gui.PathViewPanel;
import borg.edtrading.gui.RouteViewPanel;
import borg.edtrading.journal.Journal;
import borg.edtrading.journal.JournalReader;
import borg.edtrading.services.EddbService;
import borg.edtrading.util.FuelAndJumpRangeLookup;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.data.domain.PageRequest;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.swing.JFrame;

import static org.apache.commons.lang3.StringEscapeUtils.*;

/**
 * NeutronJumpApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class NeutronJumpApp {

    static final Logger logger = LogManager.getLogger(NeutronJumpApp.class);

    private static final File ROUTES_DIR = new File(System.getProperty("user.home"), "Google Drive\\Elite Dangerous\\Routes");

    private static final AnnotationConfigApplicationContext APPCTX = new AnnotationConfigApplicationContext(Config.class);

    // Sol, Altair, Shinrarta Dezhra
    // Colonia, VY Canis Majoris, Crab Pulsar, Hen 2-23, Skaude AA-A h294, Sagittarius A*, Choomuia UI-K d8-4692

    public static void main(String[] args) throws IOException {
        final String fromName = "Eafots BP-I d9-35";
        final String toName = "Altair";
        //        final String fromName = "Sol";
        //        final String toName = "Sagittarius A*";

        // Anaconda
        final int maxFuelTons = 32;
        final float maxFuelPerJump = 8.32f;
        final float jumpRangeFuelFull = 46.06f;
        final float jumpRangeFuelOpt = 47.60f;
        //        // Beluga
        //        final int maxFuelTons = 64;
        //        final float maxFuelPerJump = 8.32f;
        //        final float jumpRangeFuelFull = 47.22f;
        //        final float jumpRangeFuelOpt = 51.22f;

        final FuelAndJumpRangeLookup fuelJumpLUT = new FuelAndJumpRangeLookup(maxFuelTons, maxFuelPerJump, jumpRangeFuelFull, jumpRangeFuelOpt);

        EddbService eddbService = APPCTX.getBean(EddbService.class);
        EddbSystemRepository eddbSystemRepository = APPCTX.getBean(EddbSystemRepository.class);
        EddbBodyRepository eddbBodyRepository = APPCTX.getBean(EddbBodyRepository.class);

        // Lookup source and destination
        EddbSystem fromSystem = eddbService.searchSystemByName(fromName);
        EddbSystem toSystem = eddbService.searchSystemByName(toName);
        logger.debug(String.format("%s → %s: %.0f Ly", fromSystem.toString(), toSystem.toString(), fromSystem.distanceTo(toSystem)));

        // Write a simple waypoints file
        final String baseFilename = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss").format(new Date()) + " " + fromSystem.getName().replaceAll("[^\\w\\s\\-\\+\\.]", "_") + " to " + toSystem.getName().replaceAll("[^\\w\\s\\-\\+\\.]", "_");
        writeWaypointsFile(fromSystem, toSystem, eddbService);

        // Try to find a route
        Map<String, Set<EddbBody>> arrivalStarsBySpectralClass = eddbService.mapStarsBySpectralClass(/* arrivalOnly = */ true);
        Set<EddbSystem> starSystemsWithNeutronStars = eddbService.searchArrivalNeutronStars().stream().map(b -> eddbSystemRepository.findOne(b.getSystemId())).collect(Collectors.toSet());
        Set<EddbSystem> starSystemsWithUnscoopableStars = eddbService.searchArrivalUnscoopableStars().stream().map(b -> eddbSystemRepository.findOne(b.getSystemId())).collect(Collectors.toSet());
        Set<EddbSystem> starSystemsWithScoopableStars = new HashSet<>(eddbService.loadAllSystems());
        starSystemsWithScoopableStars.removeAll(starSystemsWithUnscoopableStars);
        starSystemsWithScoopableStars.add(fromSystem);
        starSystemsWithScoopableStars.add(toSystem);
        logger.debug("Total known neutron stars (EDDB + Mapping Project): " + starSystemsWithNeutronStars.size());

        JFrame frame = new JFrame("Route");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLayout(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        RouteViewPanel routeViewPanel = new RouteViewPanel(eddbSystemRepository, fromSystem, toSystem);
        c.gridx = 0;
        c.gridy = 0;
        c.gridwidth = 1;
        c.gridheight = 1;
        frame.add(routeViewPanel, c);
        PathViewPanel topViewPanel = new PathViewPanel("Top view", eddbService, fromSystem, toSystem);
        c.gridx = 1;
        c.gridy = 0;
        c.gridwidth = 1;
        c.gridheight = 1;
        frame.add(topViewPanel, c);
        PathViewPanel leftViewPanel = new PathViewPanel("Left view", eddbService, fromSystem, toSystem);
        c.gridx = 0;
        c.gridy = 1;
        c.gridwidth = 2;
        c.gridheight = 1;
        frame.add(leftViewPanel, c);
        PathViewPanel frontViewPanel = new PathViewPanel("Front view", eddbService, fromSystem, toSystem);
        c.gridx = 0;
        c.gridy = 2;
        c.gridwidth = 2;
        c.gridheight = 1;
        frame.add(frontViewPanel, c);
        frame.setVisible(true);
        frame.pack();

        AyStar ayStar = new AyStar();
        ayStar.initialize(fromSystem, toSystem, starSystemsWithNeutronStars, starSystemsWithScoopableStars, fuelJumpLUT);
        final long start = System.currentTimeMillis();
        Path path = null;
        while ((path = ayStar.findPath()) != null && !path.getMinimizedStarSystem().getId().equals(toSystem.getId())) {
            routeViewPanel.updatePath(path);
            topViewPanel.updatePath(path);
            leftViewPanel.updatePath(path);
            frontViewPanel.updatePath(path);
        }
        final long end = System.currentTimeMillis();
        logger.info("Took " + DurationFormatUtils.formatDuration(end - start, "H:mm:ss"));
        if (path == null) {
            logger.warn("No path found");
            return;
        } else {
            routeViewPanel.updatePath(path);
            topViewPanel.updatePath(path);
            leftViewPanel.updatePath(path);
            frontViewPanel.updatePath(path);
            logger.info("Found path with " + path.getTotalJumps() + " jumps");
        }
        List<Path> sortedPaths = path.toSortedList();

        // Read the journal to see which systems/bodies have already been discovered
        Journal journal = new Journal(new JournalReader().readEntireJournal(Constants.JOURNAL_DIR));

        // Write route as VoiceAttack TXT file
        String route = routeToVoiceAttackTxt(sortedPaths, starSystemsWithNeutronStars, arrivalStarsBySpectralClass, eddbSystemRepository, eddbBodyRepository, fuelJumpLUT, journal);
        FileUtils.write(new File(ROUTES_DIR, baseFilename + " Route.txt"), route, "UTF-8");

        // Write route as human readable HTML file
        String html = routeToHumanReadableHtml(sortedPaths, starSystemsWithNeutronStars, arrivalStarsBySpectralClass, eddbSystemRepository, eddbBodyRepository, fuelJumpLUT, journal);
        FileUtils.write(new File(ROUTES_DIR, baseFilename + " Route.html"), html, "UTF-8");
    }

    private static void writeWaypointsFile(EddbSystem fromSystem, EddbSystem toSystem, EddbService eddbService) throws IOException {
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
                EddbSystem closestSystem = eddbService.searchClosestSystemByCoord(coord);
                FileUtils.write(waypointsFile, String.format(Locale.US, "Waypoint %2d: %s\n", wp, closestSystem.getName()), "UTF-8", true);
            }
        }
    }

    private static String routeToVoiceAttackTxt(List<Path> sortedPaths, Set<EddbSystem> starSystemsWithNeutronStars, Map<String, Set<EddbBody>> arrivalStarsBySpectralClass, EddbSystemRepository repo, EddbBodyRepository bodyrepo,
            FuelAndJumpRangeLookup fuelJumpLUT, Journal journal) {
        Date eddbDumpDate = new Date(Constants.BODIES_FILE.lastModified());
        EddbSystem fromSystem = sortedPaths.get(0).getStarSystem(repo);
        EddbSystem toSystem = sortedPaths.get(sortedPaths.size() - 1).getStarSystem(repo);
        String fromName = fromSystem.getName();
        String toName = toSystem.getName();
        float directDistance = fromSystem.distanceTo(toSystem);
        float routeDistance = sortedPaths.get(sortedPaths.size() - 1).getTravelledDistanceLy();
        int jumpsUsingHighway = sortedPaths.size() - 1;
        int jumpsTraditional = Math.round(directDistance / fuelJumpLUT.getJumpRangeFuelFull());
        int jumpsSaved = jumpsTraditional - jumpsUsingHighway;
        float jumpsSavedPercent = 100f * jumpsSaved / jumpsTraditional;

        int jumpNo = 0;
        int unboostedJumps = 0;
        float travelledLy = 0;
        boolean halfwayPassed = false;
        Path prevPath = null;
        LinkedList<String> lines = new LinkedList<>();
        for (int i = 0; i < sortedPaths.size(); i++) {
            Path currPath = sortedPaths.get(i);
            EddbSystem currSystem = currPath.getStarSystem(repo);
            String currName = currSystem.getName();
            String currClass = lookupSpectralClass(currSystem.getId(), arrivalStarsBySpectralClass);
            String currKnownCss = journal.getVisitedSystems().contains(currName) ? " known" : "";
            float jumpDistance = 0;
            String flags = "";
            if (prevPath != null) {
                jumpNo++;
                EddbSystem prevSystem = prevPath.getStarSystem(repo);
                jumpDistance = currSystem.distanceTo(prevSystem);
                travelledLy += jumpDistance;
                String neutronJumpCss = starSystemsWithNeutronStars.contains(prevSystem) ? "neutronJump" : "normalJump";
                boolean plotRoute = false;
                if (starSystemsWithNeutronStars.contains(prevSystem)) {
                    unboostedJumps = 0;
                } else {
                    unboostedJumps++;
                    if (unboostedJumps >= 2 && (starSystemsWithNeutronStars.contains(currSystem) || i == sortedPaths.size() - 1)) {
                        for (int r = 1; r < unboostedJumps; r++) {
                            lines.removeLast();
                        }
                        jumpDistance = 999;
                        plotRoute = true;
                    }
                }
                String evenOddCss = jumpNo % 2 == 0 ? "even" : "odd";
                String prevName = prevSystem.getName();
                String prevClass = lookupSpectralClass(prevSystem.getId(), arrivalStarsBySpectralClass);
                String prevKnownCss = journal.getVisitedSystems().contains(prevName) ? " known" : "";
                String notes = "";
                if (plotRoute) {
                    flags += "R"; // Route
                }
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
                List<EddbBody> valuableBodies = findValuableBodies(currSystem, bodyrepo);
                if (valuableBodies.size() > 0) {
                    int unknown = 0;
                    for (EddbBody body : valuableBodies) {
                        if (!journal.getScannedBodies().contains(body.getName())) {
                            unknown++;
                        }
                        String knownCss = journal.getScannedBodies().contains(body.getName()) ? "known" : "";
                        String typeCss = body.getTypeName().toLowerCase().replaceAll("\\W", "-");
                        notes += "<span class=\"valuablePlanet " + typeCss + " " + knownCss + "\">" + escapeHtml4(body.getName().replace(currName + " ", "")) + "</span>";
                    }
                    if (unknown > 0) {
                        flags += "P"; // Planets
                    }
                }
                // TODO S
            }
            lines.add(String.format(Locale.US, "%-50s%5.0f%10s%10s", currName.replace("'", " "), jumpDistance, currClass, flags));
            prevPath = currPath;
        }

        return lines.stream().collect(Collectors.joining("\n"));
    }

    private static String routeToHumanReadableHtml(List<Path> sortedPaths, Set<EddbSystem> starSystemsWithNeutronStars, Map<String, Set<EddbBody>> arrivalStarsBySpectralClass, EddbSystemRepository repo, EddbBodyRepository bodyrepo,
            FuelAndJumpRangeLookup fuelJumpLUT, Journal journal) {
        StringBuilder html = new StringBuilder();

        Date eddbDumpDate = new Date(Constants.BODIES_FILE.lastModified());
        EddbSystem fromSystem = sortedPaths.get(0).getStarSystem(repo);
        EddbSystem toSystem = sortedPaths.get(sortedPaths.size() - 1).getStarSystem(repo);
        String fromName = fromSystem.getName();
        String toName = toSystem.getName();
        float directDistance = fromSystem.distanceTo(toSystem);
        float routeDistance = sortedPaths.get(sortedPaths.size() - 1).getTravelledDistanceLy();
        int jumpsUsingHighway = sortedPaths.size() - 1;
        int jumpsTraditional = Math.round(directDistance / fuelJumpLUT.getJumpRangeFuelFull());
        int jumpsSaved = jumpsTraditional - jumpsUsingHighway;
        float jumpsSavedPercent = 100f * jumpsSaved / jumpsTraditional;

        String title = escapeHtml4(String.format(Locale.US, "%s → %s (%.0f Ly, %d jumps)", fromName, toName, directDistance, jumpsUsingHighway));
        String h2 = String.format(Locale.US, "EDDB data from %td-%tb-%tY, %d known neutron star systems", eddbDumpDate, eddbDumpDate, eddbDumpDate, starSystemsWithNeutronStars.size());
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
        int unboostedJumps = 0;
        float travelledLy = 0;
        boolean halfwayPassed = false;
        Path prevPath = null;
        for (Path currPath : sortedPaths) {
            if (prevPath != null) {
                jumpNo++;
                EddbSystem prevSystem = prevPath.getStarSystem(repo);
                EddbSystem currSystem = currPath.getStarSystem(repo);
                float jumpDistance = currSystem.distanceTo(prevSystem);
                travelledLy += jumpDistance;
                String neutronJumpCss = starSystemsWithNeutronStars.contains(prevSystem) ? "neutronJump" : "normalJump";
                boolean plotRoute = false;
                if (starSystemsWithNeutronStars.contains(prevSystem)) {
                    unboostedJumps = 0;
                } else {
                    unboostedJumps++;
                    if (unboostedJumps >= 2 && starSystemsWithNeutronStars.contains(currSystem)) {
                        plotRoute = true;
                    }
                }
                String evenOddCss = jumpNo % 2 == 0 ? "even" : "odd";
                String prevName = prevSystem.getName();
                String prevClass = lookupSpectralClass(prevSystem.getId(), arrivalStarsBySpectralClass);
                String prevKnownCss = journal.getVisitedSystems().contains(prevName) ? " known" : "";
                String currName = currSystem.getName();
                String currClass = lookupSpectralClass(currSystem.getId(), arrivalStarsBySpectralClass);
                String currKnownCss = journal.getVisitedSystems().contains(currName) ? " known" : "";
                String flags = "";
                String notes = "";
                if (plotRoute) {
                    flags += "R";
                }
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
                List<EddbBody> valuableBodies = findValuableBodies(currSystem, bodyrepo);
                if (valuableBodies.size() > 0) {
                    int unknown = 0;
                    for (EddbBody body : valuableBodies) {
                        if (!journal.getScannedBodies().contains(body.getName())) {
                            unknown++;
                        }
                        String knownCss = journal.getScannedBodies().contains(body.getName()) ? "known" : "";
                        String typeCss = body.getTypeName().toLowerCase().replaceAll("\\W", "-");
                        notes += "<span class=\"valuablePlanet " + typeCss + " " + knownCss + "\">" + escapeHtml4(body.getName().replace(currName + " ", "")) + "</span>";
                    }
                    if (unknown > 0) {
                        flags += "P"; // Planets
                    }
                }
                // TODO S
                //float routePercent = 100f * jumpNo / (sortedSystems.size() - 1);
                html.append("<tr class=\"" + evenOddCss + " " + neutronJumpCss + "\">");
                html.append("<td class=\"numeric jumpNo\">" + jumpNo + "</td>");
                html.append("<td class=\"starName " + prevKnownCss + "\">" + escapeHtml4(prevName) + "</td>");
                html.append("<td class=\"starClass spectralClass-" + prevClass + "\">" + prevClass + "</td>");
                html.append("<td class=\"numeric jumpDistance\">" + String.format(Locale.US, "%.1f Ly", jumpDistance) + "</td>");
                html.append("<td class=\"starClass spectralClass-" + currClass + "\">" + currClass + "</td>");
                html.append("<td class=\"starName " + currKnownCss + "\">" + escapeHtml4(currName) + "</td>");
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

    private static String lookupSpectralClass(Long systemId, Map<String, Set<EddbBody>> arrivalStarsBySpectralClass) {
        if (systemId != null) {
            for (String spectralClass : arrivalStarsBySpectralClass.keySet()) {
                for (EddbBody star : arrivalStarsBySpectralClass.get(spectralClass)) {
                    if (systemId.equals(star.getSystemId())) {
                        return spectralClass;
                    }
                }
            }
        }
        return "?";
    }

    static List<EddbBody> findValuableBodies(EddbSystem system, EddbBodyRepository repo) {
        List<EddbBody> result = new ArrayList<>(0);
        for (EddbBody body : repo.findBySystemId(system.getId(), new PageRequest(0, 100)).getContent()) {
            if (StringUtils.isNotEmpty(body.getTerraformingStateName()) && !"Not terraformable".equals(body.getTerraformingStateName())) {
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
