package borg.edtrading;

import borg.edtrading.aystar.AyStar;
import borg.edtrading.aystar.Path;
import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddb.reader.EddbReader;
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
import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
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
        final String fromName = "Sol";
        final String toName = "Colonia";
        //        final String fromName = "Sol";
        //        final String toName = "Sagittarius A*";

        //        // Anaconda
        //        final int maxFuelTons = 32;
        //        final float maxFuelPerJump = 8.32f;
        //        final float jumpRangeFuelFull = 46.06f;
        //        final float jumpRangeFuelOpt = 47.60f;
        // Beluga
        final int maxFuelTons = 128;
        final float maxFuelPerJump = 13.33f;
        final float jumpRangeFuelFull = 30.15f;
        final float jumpRangeFuelOpt = 32.29f;

        final FuelAndJumpRangeLookup fuelJumpLUT = new FuelAndJumpRangeLookup(maxFuelTons, maxFuelPerJump, jumpRangeFuelFull, jumpRangeFuelOpt);

        APPCTX.getBean(EddbReader.class).loadEddbDataIntoElasticsearch();
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
        Map<String, List<EddbBody>> arrivalStarsBySpectralClass = eddbService.mapStarsBySpectralClass(/* arrivalOnly = */ true);
        List<EddbSystem> starSystemsWithNeutronStars = eddbService.retainStarsOfSpectralClasses(arrivalStarsBySpectralClass, "NS").parallelStream().map(b -> eddbSystemRepository.findOne(b.getSystemId())).collect(Collectors.toList());
        Set<EddbSystem> starSystemsWithUnscoopableStars = eddbService.removeStarsOfSpectralClasses(arrivalStarsBySpectralClass, Constants.SCOOPABLE_SPECTRAL_CLASSES.toArray(new String[0])).parallelStream()
                .map(b -> eddbSystemRepository.findOne(b.getSystemId())).collect(Collectors.toSet());
        Set<EddbSystem> starSystemsWithScoopableStars = new HashSet<>(eddbService.loadAllSystems());
        starSystemsWithScoopableStars.removeAll(starSystemsWithUnscoopableStars);
        //        List<EddbSystem> starSystemsWithScoopableStars = eddbService.retainStarsOfSpectralClasses(arrivalStarsBySpectralClass, Constants.SCOOPABLE_SPECTRAL_CLASSES.toArray(new String[0])).parallelStream()
        //                .map(b -> eddbSystemRepository.findOne(b.getSystemId())).collect(Collectors.toList());
        starSystemsWithScoopableStars.add(fromSystem);
        starSystemsWithScoopableStars.add(toSystem);
        logger.debug("Total known neutron stars: " + starSystemsWithNeutronStars.size());

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

        Route route = Route.fromPath(sortedPaths, fuelJumpLUT, journal, eddbSystemRepository, eddbBodyRepository);

        // Write route as VoiceAttack TXT file
        FileUtils.write(new File(ROUTES_DIR, baseFilename + " Route.txt"), route.toVoiceAttackTxt(), "UTF-8");

        // Write route as human readable HTML file
        Date eddbDumpDate = new Date(new File(System.getProperty("user.home"), ".eddbdata/systems.csv").lastModified());
        int nKnownArrivalNeutronStars = starSystemsWithNeutronStars.size();
        FileUtils.write(new File(ROUTES_DIR, baseFilename + " Route.html"), route.toHumanReadableHtml(eddbDumpDate, nKnownArrivalNeutronStars), "UTF-8");
    }

    public static class Route implements Serializable {

        private static final long serialVersionUID = 8127296548648983823L;

        private final List<RouteElement> elements = new ArrayList<>();
        private final FuelAndJumpRangeLookup fuelJumpLUT;
        private final Journal journal;

        public static Route fromPath(List<Path> sortedPaths, FuelAndJumpRangeLookup fuelJumpLUT, Journal journal, EddbSystemRepository systemRepo, EddbBodyRepository bodyRepo) {
            Route route = new Route(fuelJumpLUT, journal);

            Path prevPath = null;
            for (Path currPath : sortedPaths) {
                if (prevPath != null) {
                    EddbSystem fromSystem = systemRepo.findOne(prevPath.getMinimizedStarSystem().getId());
                    List<EddbBody> fromBodies = bodyRepo.findBySystemId(prevPath.getMinimizedStarSystem().getId(), new PageRequest(0, 250)).getContent();

                    EddbSystem toSystem = systemRepo.findOne(currPath.getMinimizedStarSystem().getId());
                    List<EddbBody> toBodies = bodyRepo.findBySystemId(currPath.getMinimizedStarSystem().getId(), new PageRequest(0, 250)).getContent();

                    route.add(new RouteElement(route, currPath.getTotalJumps(), fromSystem, fromBodies, toSystem, toBodies, currPath.getFuelLevel(), currPath.getTravelledDistanceLy(), currPath.getRemainingDistanceLy()));
                }
                prevPath = currPath;
            }

            return route.markDryPeriods();
        }

        private void add(RouteElement e) {
            this.elements.add(e);
        }

        public Route(FuelAndJumpRangeLookup fuelJumpLUT, Journal journal) {
            this.fuelJumpLUT = fuelJumpLUT;
            this.journal = journal;
        }

        private Route markDryPeriods() {
            int nUnboostedJumps = 0;
            for (int index = 0; index < this.getElements().size(); index++) {
                RouteElement e = this.getElements().get(index);
                if (!"NS".equals(e.getFromSpectralClass())) {
                    nUnboostedJumps++;
                } else {
                    if (nUnboostedJumps >= 3) {
                        // Example for 3 unboosted jumps:
                        // index    = NS -> *
                        // index-1  = !NS -> NS: keep (jump to ns, exact fuel usage)
                        // index-2  = !NS -> !NS: keep (last refuel before jump to ns)
                        // index-3  = !NS -> !NS: skip
                        // index-4  = NS -> !NS: keep (make use of boost)
                        this.getElements().get(index - 2).setDryPeriod(DryPeriod.END);
                        for (int skip = 3; skip <= nUnboostedJumps; skip++) {
                            this.getElements().get(index - skip).setDryPeriod(DryPeriod.PART_OF);
                        }
                        if (index >= (nUnboostedJumps + 1)) {
                            this.getElements().get(index - (nUnboostedJumps + 1)).setDryPeriod(DryPeriod.START);
                        }
                    }
                    nUnboostedJumps = 0;
                }
            }
            return this;
        }

        public List<RouteElement> getElements() {
            return this.elements;
        }

        public FuelAndJumpRangeLookup getFuelJumpLUT() {
            return this.fuelJumpLUT;
        }

        public Journal getJournal() {
            return this.journal;
        }

        public String toVoiceAttackTxt() {
            StringBuilder txt = new StringBuilder();

            RouteElement last = null;
            for (RouteElement curr : this.getElements()) {
                if (curr.getDryPeriod() != DryPeriod.PART_OF) {
                    txt.append(curr.toVoiceAttackRow(last));
                    last = curr;
                }
            }

            return txt.toString();
        }

        public String toHumanReadableHtml(Date eddbDumpDate, int nKnownArrivalNeutronStars) {
            StringBuilder html = new StringBuilder();

            EddbSystem fromSystem = this.getElements().get(0).getFromSystem();
            EddbSystem toSystem = this.getElements().get(this.getElements().size() - 1).getToSystem();
            String fromName = fromSystem.getName();
            String toName = toSystem.getName();
            float directDistance = fromSystem.distanceTo(toSystem);
            int jumpsUsingHighway = this.getElements().get(this.getElements().size() - 1).getJumpNo();
            int jumpsTraditional = Math.round(directDistance / fuelJumpLUT.getJumpRangeFuelFull());
            int jumpsSaved = jumpsTraditional - jumpsUsingHighway;
            float jumpsSavedPercent = 100f * jumpsSaved / jumpsTraditional;

            String title = escapeHtml4(String.format(Locale.US, "%s → %s (%.0f Ly, %d jumps)", fromName, toName, directDistance, jumpsUsingHighway));
            String h2 = String.format(Locale.US, "EDDB data from %td-%tb-%tY, %d known neutron star systems", eddbDumpDate, eddbDumpDate, eddbDumpDate, nKnownArrivalNeutronStars);
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
            html.append(RouteElement.toHtmlTableHeadline());
            for (RouteElement e : this.getElements()) {
                html.append(e.toHtmlTableRow());
            }
            html.append("</table>\n");
            html.append("</body>\n");
            html.append("</html>");

            return html.toString();
        }

    }

    public static class RouteElement implements Serializable {

        private static final long serialVersionUID = -2544933509448009207L;

        private final Route route;

        private final int jumpNo;

        private final EddbSystem fromSystem;
        private final List<EddbBody> fromBodies;
        private final EddbBody fromStar;
        private final String fromSpectralClass;

        private final EddbSystem toSystem;
        private final List<EddbBody> toBodies;
        private final EddbBody toStar;
        private final String toSpectralClass;

        private final float fuelLevelOnArrival;
        private final float travelledLy;
        private final float remainingLy;

        private DryPeriod dryPeriod = DryPeriod.NOT_PART_OF;

        public RouteElement(Route route, int jumpNo, EddbSystem fromSystem, List<EddbBody> fromBodies, EddbSystem toSystem, List<EddbBody> toBodies, float fuelLevelOnArrival, float travelledLy, float remainingLy) {
            this.route = route;

            this.jumpNo = jumpNo;

            this.fromSystem = fromSystem;
            this.fromBodies = Collections.unmodifiableList(fromBodies);
            Optional<EddbBody> optionalFrom = fromBodies.stream().filter(b -> Boolean.TRUE.equals(b.getIsMainStar())).findFirst();
            if (optionalFrom.isPresent()) {
                this.fromStar = optionalFrom.get();
                this.fromSpectralClass = this.fromStar.toStarClass();
            } else {
                this.fromStar = null;
                this.fromSpectralClass = "?";
            }

            this.toSystem = toSystem;
            this.toBodies = Collections.unmodifiableList(toBodies);
            Optional<EddbBody> optionalTo = toBodies.stream().filter(b -> Boolean.TRUE.equals(b.getIsMainStar())).findFirst();
            if (optionalTo.isPresent()) {
                this.toStar = optionalTo.get();
                this.toSpectralClass = this.toStar.toStarClass();
            } else {
                this.toStar = null;
                this.toSpectralClass = "?";
            }

            this.fuelLevelOnArrival = fuelLevelOnArrival;
            this.travelledLy = travelledLy;
            this.remainingLy = remainingLy;
        }

        public String toVoiceAttackRow(RouteElement last) {
            float jumpDistance = last != null ? last.getToSystem().distanceTo(this.getToSystem()) : this.getFromSystem().distanceTo(this.getToSystem());
            String flags = "";
            if (this.getDryPeriod() == DryPeriod.END) {
                flags += "R";
            }
            if (this.getToSystem().getName().replaceAll("[^\\-]", "").length() < 2) {
                flags += "N"; // Pron name
            }
            if (this.getFuelLevelOnArrival() <= (this.getRoute().getFuelJumpLUT().getMaxFuelPerJump() * 1.25f)) {
                flags += "F";
            }
            List<EddbBody> valuableBodies = findValuableBodies(this.getToBodies());
            if (valuableBodies.size() > 0) {
                for (EddbBody body : valuableBodies) {
                    if (!this.getRoute().getJournal().getScannedBodies().contains(body.getName())) {
                        flags += "P"; // Planets
                        break;
                    }
                }
            }
            return String.format(Locale.US, "%-50s%5.0f%10s%10s\n", this.getToSystem().getName().replace("'", " "), jumpDistance, this.getToSpectralClass(), flags);
        }

        public static String toHtmlTableHeadline() {
            StringBuilder html = new StringBuilder();

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

            return html.toString();
        }

        public String toHtmlTableRow() {
            StringBuilder html = new StringBuilder();

            String evenOddCss = this.getJumpNo() % 2 == 0 ? "even" : "odd";
            String neutronJumpCss = "NS".equals(this.getFromSpectralClass()) ? "neutronJump" : "normalJump";
            String prevKnownCss = this.getRoute().getJournal().getVisitedSystems().contains(this.getFromSystem().getName()) ? " known" : "";
            String currKnownCss = this.getRoute().getJournal().getVisitedSystems().contains(this.getToSystem().getName()) ? " known" : "";
            String flags = "";
            String notes = "";
            if (this.getDryPeriod() == DryPeriod.END) {
                flags += "R";
                notes += "&lt;Route END&gt;&nbsp;";
            } else if (this.getDryPeriod() == DryPeriod.START) {
                //flags += "R";
                notes += "&lt;Route START&gt;&nbsp;";
            } else if (this.getDryPeriod() == DryPeriod.PART_OF) {
                //flags += "R";
                notes += "&lt;Route PART&gt;&nbsp;";
            }
            if (this.getToSystem().getName().replaceAll("[^\\-]", "").length() < 2) {
                flags += "N"; // Pron name
            }
            if (this.getFuelLevelOnArrival() <= (this.getRoute().getFuelJumpLUT().getMaxFuelPerJump() * 1.25f)) {
                flags += "F";
                notes += "<span class=\"fuelWarning\">" + String.format(Locale.US, "%.1ft", this.getFuelLevelOnArrival()) + "</span>";
            }
            List<EddbBody> valuableBodies = findValuableBodies(this.getToBodies());
            if (valuableBodies.size() > 0) {
                int unknown = 0;
                for (EddbBody body : valuableBodies) {
                    if (!this.getRoute().getJournal().getScannedBodies().contains(body.getName())) {
                        unknown++;
                    }
                    String knownCss = this.getRoute().getJournal().getScannedBodies().contains(body.getName()) ? "known" : "";
                    String typeCss = body.getTypeName().toLowerCase().replaceAll("\\W", "-");
                    notes += "<span class=\"valuablePlanet " + typeCss + " " + knownCss + "\">" + escapeHtml4(body.getName().replace(this.getToSystem().getName() + " ", "")) + "</span>";
                }
                if (unknown > 0) {
                    flags += "P"; // Planets
                }
            }

            html.append("<tr class=\"" + evenOddCss + " " + neutronJumpCss + "\">");
            html.append("<td class=\"numeric jumpNo\">" + this.getJumpNo() + "</td>");
            html.append("<td class=\"starName " + prevKnownCss + "\">" + escapeHtml4(this.getFromSystem().getName()) + "</td>");
            html.append("<td class=\"starClass spectralClass-" + this.getFromSpectralClass() + "\">" + this.getFromSpectralClass() + "</td>");
            html.append("<td class=\"numeric jumpDistance\">" + String.format(Locale.US, "%.1f Ly", this.getFromSystem().distanceTo(this.getToSystem())) + "</td>");
            html.append("<td class=\"starClass spectralClass-" + this.getToSpectralClass() + "\">" + this.getToSpectralClass() + "</td>");
            html.append("<td class=\"starName " + currKnownCss + "\">" + escapeHtml4(this.getToSystem().getName()) + "</td>");
            html.append("<td class=\"notes\">" + "[" + flags + "] " + notes + "</td>");
            html.append("<td class=\"numeric distance\">" + String.format(Locale.US, "%.0f Ly", this.getTravelledLy()) + "</td>");
            html.append("</tr>\n");

            return html.toString();
        }

        public Route getRoute() {
            return this.route;
        }

        public int getJumpNo() {
            return this.jumpNo;
        }

        public EddbSystem getFromSystem() {
            return this.fromSystem;
        }

        public List<EddbBody> getFromBodies() {
            return this.fromBodies;
        }

        public EddbBody getFromStar() {
            return this.fromStar;
        }

        public String getFromSpectralClass() {
            return this.fromSpectralClass;
        }

        public EddbSystem getToSystem() {
            return this.toSystem;
        }

        public List<EddbBody> getToBodies() {
            return this.toBodies;
        }

        public EddbBody getToStar() {
            return this.toStar;
        }

        public String getToSpectralClass() {
            return this.toSpectralClass;
        }

        public float getFuelLevelOnArrival() {
            return this.fuelLevelOnArrival;
        }

        public float getTravelledLy() {
            return this.travelledLy;
        }

        public float getRemainingLy() {
            return this.remainingLy;
        }

        public DryPeriod getDryPeriod() {
            return this.dryPeriod;
        }

        public void setDryPeriod(DryPeriod dryPeriod) {
            this.dryPeriod = dryPeriod;
        }

    }

    public static enum DryPeriod {
        NOT_PART_OF,
        /** This is the last planned jump (NS -> ?) */
        START,
        /** This is part of the dry period (!NS -> !NS) */
        PART_OF,
        /** This is the first planned jump after the dry period (!NS -> scoopable before NS) */
        END;
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

    //    private static String routeToVoiceAttackTxt(List<Path> sortedPaths, List<EddbSystem> starSystemsWithNeutronStars, Map<String, List<EddbBody>> arrivalStarsBySpectralClass, EddbSystemRepository repo, EddbBodyRepository bodyrepo,
    //            FuelAndJumpRangeLookup fuelJumpLUT, Journal journal) {
    //        float routeDistance = sortedPaths.get(sortedPaths.size() - 1).getTravelledDistanceLy();
    //
    //        float travelledLy = 0;
    //        boolean halfwayPassed = false;
    //        Path prevPath = null;
    //        LinkedList<VoiceAttackLine> lines = new LinkedList<>();
    //        for (int i = 0; i < sortedPaths.size(); i++) {
    //            Path currPath = sortedPaths.get(i);
    //            EddbSystem currSystem = currPath.getStarSystem(repo);
    //            String currName = currSystem.getName();
    //            String currClass = lookupSpectralClass(currSystem.getId(), arrivalStarsBySpectralClass);
    //            float jumpDistance = 0;
    //            String flags = "";
    //            if (prevPath != null) {
    //                EddbSystem prevSystem = prevPath.getStarSystem(repo);
    //                jumpDistance = currSystem.distanceTo(prevSystem);
    //                travelledLy += jumpDistance;
    //                if (currName.replaceAll("[^\\-]", "").length() < 2) {
    //                    flags += "N"; // Pron name
    //                }
    //                if (travelledLy >= routeDistance / 2 && !halfwayPassed) {
    //                    flags += "H"; // Halfway
    //                    halfwayPassed = true;
    //                }
    //                if (currPath.getFuelLevel() <= (fuelJumpLUT.getMaxFuelPerJump() + 2)) {
    //                    flags += "F";
    //                }
    //                List<EddbBody> valuableBodies = findValuableBodies(currSystem, bodyrepo);
    //                if (valuableBodies.size() > 0) {
    //                    int unknown = 0;
    //                    for (EddbBody body : valuableBodies) {
    //                        if (!journal.getScannedBodies().contains(body.getName())) {
    //                            unknown++;
    //                        }
    //                    }
    //                    if (unknown > 0) {
    //                        flags += "P"; // Planets
    //                    }
    //                }
    //            }
    //            lines.add(new VoiceAttackLine(currName, jumpDistance, currClass, flags));
    //            prevPath = currPath;
    //        }
    //
    //        int nUnboostedJumps = 0;
    //        for (int index = 1; index < lines.size(); index++) {
    //            VoiceAttackLine line = lines.get(index);
    //            if (!"NS".equals(line.jumpToClass)) {
    //                nUnboostedJumps++;
    //            } else {
    //                if (nUnboostedJumps >= 3) {
    //                    for (int setnull = 2; setnull <= nUnboostedJumps; setnull++) {
    //                        lines.set(index - setnull, null);
    //                    }
    //                    EddbSystem routeFrom = sortedPaths.get((index - nUnboostedJumps) - 1).getStarSystem(repo);
    //                    EddbSystem routeTo = sortedPaths.get(index - 1).getStarSystem(repo);
    //
    //                    lines.get(index - 1).flags += "R";
    //                    lines.get(index - 1).jumpDistance += routeFrom.distanceTo(routeTo);
    //                }
    //                nUnboostedJumps = 0;
    //            }
    //        }
    //
    //        return lines.stream().filter(l -> l != null).map(l -> l.format()).collect(Collectors.joining("\n"));
    //    }
    //
    //    public static class VoiceAttackLine {
    //        public String jumpToName = null;
    //        public float jumpDistance = 0f;
    //        public String jumpToClass = null;
    //        public String flags = null;
    //
    //        public VoiceAttackLine(String jumpToName, float jumpDistance, String jumpToClass, String flags) {
    //            this.jumpToName = jumpToName;
    //            this.jumpDistance = jumpDistance;
    //            this.jumpToClass = jumpToClass;
    //            this.flags = flags;
    //        }
    //
    //        public String format() {
    //            return String.format(Locale.US, "%-50s%5.0f%10s%10s", jumpToName.replace("'", " "), jumpDistance, jumpToClass, flags);
    //        }
    //    }
    //
    //    private static String routeToHumanReadableHtml(List<Path> sortedPaths, List<EddbSystem> starSystemsWithNeutronStars, Map<String, List<EddbBody>> arrivalStarsBySpectralClass, EddbSystemRepository repo, EddbBodyRepository bodyrepo,
    //            FuelAndJumpRangeLookup fuelJumpLUT, Journal journal) {
    //        StringBuilder html = new StringBuilder();
    //
    //        Date eddbDumpDate = new Date(new File(System.getProperty("user.home"), ".eddbdata/systems.csv").lastModified());
    //        EddbSystem fromSystem = sortedPaths.get(0).getStarSystem(repo);
    //        EddbSystem toSystem = sortedPaths.get(sortedPaths.size() - 1).getStarSystem(repo);
    //        String fromName = fromSystem.getName();
    //        String toName = toSystem.getName();
    //        float directDistance = fromSystem.distanceTo(toSystem);
    //        float routeDistance = sortedPaths.get(sortedPaths.size() - 1).getTravelledDistanceLy();
    //        int jumpsUsingHighway = sortedPaths.size() - 1;
    //        int jumpsTraditional = Math.round(directDistance / fuelJumpLUT.getJumpRangeFuelFull());
    //        int jumpsSaved = jumpsTraditional - jumpsUsingHighway;
    //        float jumpsSavedPercent = 100f * jumpsSaved / jumpsTraditional;
    //
    //        String title = escapeHtml4(String.format(Locale.US, "%s → %s (%.0f Ly, %d jumps)", fromName, toName, directDistance, jumpsUsingHighway));
    //        String h2 = String.format(Locale.US, "EDDB data from %td-%tb-%tY, %d known neutron star systems", eddbDumpDate, eddbDumpDate, eddbDumpDate, starSystemsWithNeutronStars.size());
    //        String h3 = String.format(Locale.US, "Jump range: %.1f to %.1f Ly | Fuel usage: Max %.2f of %d tons | Jumps saved: %d of %d (%.0f%%)", fuelJumpLUT.getJumpRangeFuelFull(), fuelJumpLUT.getJumpRangeFuelOpt(), fuelJumpLUT.getMaxFuelPerJump(),
    //                fuelJumpLUT.getMaxFuelTons(), jumpsSaved, jumpsTraditional, jumpsSavedPercent);
    //        html.append("<html>\n");
    //        html.append("<head>\n");
    //        html.append("<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n");
    //        html.append("<link href=\"route.css\" rel=\"stylesheet\" type=\"text/css\" />\n");
    //        html.append("<title>").append(title).append("</title>\n");
    //        html.append("</head>\n");
    //        html.append("<body>\n");
    //        html.append("<h1>").append(title).append("</h1>\n");
    //        html.append("<h2>").append(h2).append("</h2>\n");
    //        html.append("<h3>").append(h3).append("</h3>\n");
    //        html.append("<table id=\"jumpTable\">\n");
    //        html.append("<tr>");
    //        html.append("<th class=\"numeric jumpNo\">#</th>");
    //        html.append("<th class=\"starName\">From</th>");
    //        html.append("<th class=\"starClass\">Class</th>");
    //        html.append("<th class=\"numeric jumpDistance\">Jump</th>");
    //        html.append("<th class=\"starClass\">Class</th>");
    //        html.append("<th class=\"starName\">To</th>");
    //        html.append("<th class=\"notes\">Notes</th>");
    //        html.append("<th class=\"numeric distance\">Dist</th>");
    //        html.append("</tr>\n");
    //        int jumpNo = 0;
    //        int unboostedJumps = 0;
    //        float travelledLy = 0;
    //        boolean halfwayPassed = false;
    //        Path prevPath = null;
    //        for (Path currPath : sortedPaths) {
    //            if (prevPath != null) {
    //                jumpNo++;
    //                EddbSystem prevSystem = prevPath.getStarSystem(repo);
    //                EddbSystem currSystem = currPath.getStarSystem(repo);
    //                float jumpDistance = currSystem.distanceTo(prevSystem);
    //                travelledLy += jumpDistance;
    //                String neutronJumpCss = starSystemsWithNeutronStars.contains(prevSystem) ? "neutronJump" : "normalJump";
    //                boolean plotRoute = false;
    //                if (starSystemsWithNeutronStars.contains(prevSystem)) {
    //                    unboostedJumps = 0;
    //                } else {
    //                    unboostedJumps++;
    //                    if (unboostedJumps >= 2 && starSystemsWithNeutronStars.contains(currSystem)) {
    //                        plotRoute = true;
    //                    }
    //                }
    //                String evenOddCss = jumpNo % 2 == 0 ? "even" : "odd";
    //                String prevName = prevSystem.getName();
    //                String prevClass = lookupSpectralClass(prevSystem.getId(), arrivalStarsBySpectralClass);
    //                String prevKnownCss = journal.getVisitedSystems().contains(prevName) ? " known" : "";
    //                String currName = currSystem.getName();
    //                String currClass = lookupSpectralClass(currSystem.getId(), arrivalStarsBySpectralClass);
    //                String currKnownCss = journal.getVisitedSystems().contains(currName) ? " known" : "";
    //                String flags = "";
    //                String notes = "";
    //                if (plotRoute) {
    //                    flags += "R";
    //                }
    //                if (currName.replaceAll("[^\\-]", "").length() < 2) {
    //                    flags += "N"; // Pron name
    //                }
    //                if (travelledLy >= routeDistance / 2 && !halfwayPassed) {
    //                    flags += "H"; // Halfway
    //                    halfwayPassed = true;
    //                }
    //                if (currPath.getFuelLevel() <= (fuelJumpLUT.getMaxFuelPerJump() + 2)) {
    //                    flags += "F";
    //                    notes += "<span class=\"fuelWarning\">" + String.format(Locale.US, "%.1ft", currPath.getFuelLevel()) + "</span>";
    //                }
    //                List<EddbBody> valuableBodies = findValuableBodies(currSystem, bodyrepo);
    //                if (valuableBodies.size() > 0) {
    //                    int unknown = 0;
    //                    for (EddbBody body : valuableBodies) {
    //                        if (!journal.getScannedBodies().contains(body.getName())) {
    //                            unknown++;
    //                        }
    //                        String knownCss = journal.getScannedBodies().contains(body.getName()) ? "known" : "";
    //                        String typeCss = body.getTypeName().toLowerCase().replaceAll("\\W", "-");
    //                        notes += "<span class=\"valuablePlanet " + typeCss + " " + knownCss + "\">" + escapeHtml4(body.getName().replace(currName + " ", "")) + "</span>";
    //                    }
    //                    if (unknown > 0) {
    //                        flags += "P"; // Planets
    //                    }
    //                }
    //                //float routePercent = 100f * jumpNo / (sortedSystems.size() - 1);
    //                html.append("<tr class=\"" + evenOddCss + " " + neutronJumpCss + "\">");
    //                html.append("<td class=\"numeric jumpNo\">" + jumpNo + "</td>");
    //                html.append("<td class=\"starName " + prevKnownCss + "\">" + escapeHtml4(prevName) + "</td>");
    //                html.append("<td class=\"starClass spectralClass-" + prevClass + "\">" + prevClass + "</td>");
    //                html.append("<td class=\"numeric jumpDistance\">" + String.format(Locale.US, "%.1f Ly", jumpDistance) + "</td>");
    //                html.append("<td class=\"starClass spectralClass-" + currClass + "\">" + currClass + "</td>");
    //                html.append("<td class=\"starName " + currKnownCss + "\">" + escapeHtml4(currName) + "</td>");
    //                html.append("<td class=\"notes\">" + "[" + flags + "] " + notes + "</td>");
    //                html.append("<td class=\"numeric distance\">" + String.format(Locale.US, "%.0f Ly", travelledLy) + "</td>");
    //                html.append("</tr>\n");
    //            }
    //            prevPath = currPath;
    //        }
    //        html.append("</table>\n");
    //        html.append("</body>\n");
    //        html.append("</html>");
    //
    //        return html.toString();
    //    }
    //
    //    private static String lookupSpectralClass(Long systemId, Map<String, List<EddbBody>> arrivalStarsBySpectralClass) {
    //        if (systemId != null) {
    //            for (String spectralClass : arrivalStarsBySpectralClass.keySet()) {
    //                for (EddbBody star : arrivalStarsBySpectralClass.get(spectralClass)) {
    //                    if (systemId.equals(star.getSystemId())) {
    //                        return spectralClass;
    //                    }
    //                }
    //            }
    //        }
    //        return "?";
    //    }

    static List<EddbBody> findValuableBodies(List<EddbBody> systemBodies) {
        List<EddbBody> result = new ArrayList<>(0);
        for (EddbBody body : systemBodies) {
            if (StringUtils.isNotEmpty(body.getTerraformingStateName()) && !"Not terraformable".equals(body.getTerraformingStateName())) {
                result.add(body);
            } else if (StringUtils.isNotEmpty(body.getTypeName()) && (body.getTypeName().toLowerCase().contains("world") || body.getTypeName().toLowerCase().contains("earth"))) {
                result.add(body);
            } else if (EddbBody.TYPE_ID_SUPERMASSIVE_BLACK_HOLE.equals(body.getTypeId()) && !Boolean.TRUE.equals(body.getIsMainStar())) {
                result.add(body);
            } else if (EddbBody.TYPE_ID_BLACK_HOLE.equals(body.getTypeId()) && !Boolean.TRUE.equals(body.getIsMainStar())) {
                result.add(body);
            } else if (EddbBody.TYPE_ID_NEUTRON_STAR.equals(body.getTypeId()) && !Boolean.TRUE.equals(body.getIsMainStar())) {
                result.add(body);
            } else if ("DA".equals(body.getSpectralClass()) || "DB".equals(body.getSpectralClass()) || "DC".equals(body.getSpectralClass())) {
                result.add(body);
            }
        }
        return result;
    }

}
