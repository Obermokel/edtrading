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
import org.elasticsearch.common.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
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

    public static void main(String[] args) throws IOException {
        float ladenAndFueledBaseJumpRange = 47.3f;
        float emptyTankBaseJumpRange = 50.5f;
        int maxJumpsWithoutScooping = 7;

        Galaxy galaxy = Galaxy.readDataFromFiles();
        logger.debug(galaxy.getStarSystemsById().size() + " star systems");

        StarSystem sourceSystem = galaxy.searchStarSystemByName("Sol"); // Altair, Shinrarta Dezhra, Boewnst KS-S c20-959
        logger.debug("From: " + sourceSystem);

        StarSystem targetSystem = galaxy.searchStarSystemByName("Colonia"); // Colonia, VY Canis Majoris, Crab Pulsar, Hen 2-23, Skaude AA-A h294, Sagittarius A*
        logger.debug("To: " + targetSystem);

        float directDistanceSourceToTarget = sourceSystem.distanceTo(targetSystem);
        logger.debug("Direct distance: " + String.format("%.0fly", directDistanceSourceToTarget));

        StarSystem ftemp = galaxy.searchStarSystemByName("Sol");
        StarSystem ttemp = galaxy.searchStarSystemByName("Nova Aquila No 3");
        logger.debug(ftemp + " -> " + ttemp + " = " + ftemp.distanceTo(ttemp));
        ftemp = galaxy.searchStarSystemByName("Swoiwns SG-C b46-4");
        ttemp = galaxy.searchStarSystemByName("Swoiwns RF-D d13-7");
        logger.debug(ftemp + " -> " + ttemp + " = " + ftemp.distanceTo(ttemp));

        List<Body> arrivalNeutronStars = findArrivalNeutronStars(galaxy.getBodiesById().values());
        Set<StarSystem> starSystemsWithNeutronStars = bodiesToSystems(arrivalNeutronStars);
        starSystemsWithNeutronStars.addAll(findMappingProjectNeutronStars(galaxy));
        Set<StarSystem> starSystemsWithScoopableStars = new HashSet<>(galaxy.getStarSystemsById().values());
        starSystemsWithScoopableStars.removeAll(starSystemsWithNeutronStars);
        starSystemsWithScoopableStars.add(targetSystem);
        logger.debug("Total known neutron stars (EDDB + Mapping Project): " + starSystemsWithNeutronStars.size());

        Map<String, Set<StarSystem>> systemsBySpectralClass = mapSystemsBySpectralClass(galaxy.getBodiesById().values());

        final long start = System.currentTimeMillis();
        AyStar ayStar = new AyStar();
        ayStar.initialize(sourceSystem, targetSystem, starSystemsWithNeutronStars, starSystemsWithScoopableStars, ladenAndFueledBaseJumpRange, emptyTankBaseJumpRange, maxJumpsWithoutScooping);
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

        int traditionalJumps = Math.round(directDistanceSourceToTarget / ladenAndFueledBaseJumpRange);
        int jumpsSaved = traditionalJumps - path.getTotalJumps();
        float jumpsSavedPercent = 100f * jumpsSaved / traditionalJumps;
        String h2 = String.format(Locale.US, "Direct distance: %.0f Ly | Jumps until empty: %d | Jump range min/max: %.1f Ly / %.1f Ly | Jumps saved: %d of %d (%.0f%%)", directDistanceSourceToTarget, maxJumpsWithoutScooping + 1,
                ladenAndFueledBaseJumpRange, emptyTankBaseJumpRange, jumpsSaved, traditionalJumps, jumpsSavedPercent);
        String html = pathToHtml(path, starSystemsWithNeutronStars, systemsBySpectralClass, h2);
        String filename = "Route " + sourceSystem.getName().replaceAll("[^\\w\\s\\-\\+\\.]", "_") + " to " + targetSystem.getName().replaceAll("[^\\w\\s\\-\\+\\.]", "_") + " " + new SimpleDateFormat("yyyy-MM-dd").format(new Date()) + ".html";
        FileUtils.write(new File(Constants.TEMP_DIR, filename), html, "UTF-8");

        String route = pathToRoute(path, starSystemsWithNeutronStars, systemsBySpectralClass, maxJumpsWithoutScooping, galaxy);
        FileUtils.write(new File(Constants.TEMP_DIR, filename.replace(".html", ".txt")), route, "UTF-8");
    }

    private static String pathToRoute(Path path, Set<StarSystem> starSystemsWithNeutronStars, Map<String, Set<StarSystem>> systemsBySpectralClass, int maxJumpsWithoutScooping, Galaxy galaxy) {
        StringBuilder route = new StringBuilder();

        List<StarSystem> sortedSystems = new ArrayList<>();
        Path p = path;
        while (p != null) {
            sortedSystems.add(p.getStarSystem());
            p = p.getPrev();
        }
        Collections.reverse(sortedSystems);

        final List<String> scoopableSpectralClasses = Arrays.asList("O", "B", "A", "F", "G", "K", "M");
        int jumpNo = 0;
        int jumpsWithoutScooping = 1;
        StarSystem prevSystem = null;
        for (StarSystem currSystem : sortedSystems) {
            jumpNo++;
            if (prevSystem != null) {
                route.append("\n");
            }
            String name = currSystem.getName();
            float distance = prevSystem != null ? currSystem.distanceTo(prevSystem) : 0;
            String spectralClass = lookupSpectralClass(currSystem, starSystemsWithNeutronStars, systemsBySpectralClass);
            if (!scoopableSpectralClasses.contains(spectralClass)) {
                jumpsWithoutScooping = 1;
            } else {
                jumpsWithoutScooping++;
            }
            String flags = "";
            if (name.replaceAll("[^\\-]", "").length() < 2) {
                flags += "N"; // Pron name
            }
            if (jumpNo == sortedSystems.size() / 2) {
                flags += "H"; // Halfway
            }
            if (hasValuableBodies(currSystem, galaxy)) {
                flags += "P"; // Planets
            }
            if (jumpsWithoutScooping >= maxJumpsWithoutScooping) {
                flags += "F"; // Fuel
            }
            // TODO S
            route.append(String.format(Locale.US, "%-50s%5.0f%10s%10s", name.replace("'", " "), distance, spectralClass, flags));
            prevSystem = currSystem;
        }

        return route.toString();
    }

    private static boolean hasValuableBodies(StarSystem system, Galaxy galaxy) {
        List<Body> bodies = galaxy.searchBodiesOfStarSystem(system.getId());
        for (Body body : bodies) {
            if (StringUtils.isNotEmpty(body.getTerraforming_state_name()) && !"Not terraformable".equals(body.getTerraforming_state_name())) {
                return true;
            } else if (StringUtils.isNotEmpty(body.getTypeName()) && body.getTypeName().contains("life")) {
                return true;
            }
        }
        return false;
    }

    private static String pathToHtml(Path path, Set<StarSystem> starSystemsWithNeutronStars, Map<String, Set<StarSystem>> systemsBySpectralClass, String h2) {
        StringBuilder html = new StringBuilder();

        List<StarSystem> sortedSystems = new ArrayList<>();
        Path p = path;
        while (p != null) {
            sortedSystems.add(p.getStarSystem());
            p = p.getPrev();
        }
        Collections.reverse(sortedSystems);

        String title = escapeHtml4(sortedSystems.get(0).getName()) + " → " + escapeHtml4(sortedSystems.get(sortedSystems.size() - 1).getName()) + " (" + new SimpleDateFormat("dd.MM.yyyy").format(new Date()) + ")";
        html.append("<html>\n");
        html.append("<head>\n");
        html.append("<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n");
        html.append("<link href=\"route.css\" rel=\"stylesheet\" type=\"text/css\" />\n");
        html.append("<title>").append(title).append("</title>\n");
        html.append("</head>\n");
        html.append("<body>\n");
        html.append("<h1>").append(title).append("</h1>\n");
        if (StringUtils.isNotEmpty(h2)) {
            html.append("<h2>").append(h2).append("</h2>\n");
        }
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
        StarSystem prevSystem = null;
        for (StarSystem currSystem : sortedSystems) {
            if (prevSystem != null) {
                String neutronJumpCss = starSystemsWithNeutronStars.contains(prevSystem) ? "neutronJump" : "normalJump";
                jumpNo++;
                travelledLy += currSystem.distanceTo(prevSystem);
                String evenOddCss = jumpNo % 2 == 0 ? "even" : "odd";
                String fromName = prevSystem.getName();
                String fromClass = lookupSpectralClass(prevSystem, starSystemsWithNeutronStars, systemsBySpectralClass);
                float jumpDistance = currSystem.distanceTo(prevSystem);
                String toName = currSystem.getName();
                String toClass = lookupSpectralClass(currSystem, starSystemsWithNeutronStars, systemsBySpectralClass);
                //float routePercent = 100f * jumpNo / (sortedSystems.size() - 1);
                html.append("<tr class=\"" + evenOddCss + " " + neutronJumpCss + "\">");
                html.append("<td class=\"numeric jumpNo\">" + jumpNo + "</td>");
                html.append("<td class=\"starName spectralClass-" + fromClass + "\">" + escapeHtml4(fromName) + "</td>");
                html.append("<td class=\"starClass spectralClass-" + fromClass + "\">" + fromClass + "</td>");
                html.append("<td class=\"numeric jumpDistance\">" + String.format(Locale.US, "%.1f Ly", jumpDistance) + "</td>");
                html.append("<td class=\"starClass spectralClass-" + toClass + "\">" + toClass + "</td>");
                html.append("<td class=\"starName spectralClass-" + toClass + "\">" + escapeHtml4(toName) + "</td>");
                html.append("<td class=\"notes\">&nbsp;</td>");
                html.append("<td class=\"numeric distance\">" + String.format(Locale.US, "%.0f Ly", travelledLy) + "</td>");
                html.append("</tr>\n");
            }
            prevSystem = currSystem;
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
        final List<String> scoopableSpectralClasses = Arrays.asList("O", "B", "A", "F", "G", "K", "M");

        List<Body> bodiesHavingSpectralClass = bodies.stream().filter(b -> b.getSpectral_class() != null).collect(Collectors.toList());
        List<Body> arrivalBodiesHavingSpectralClass = bodiesHavingSpectralClass.stream().filter(b -> Boolean.TRUE.equals(b.getIs_main_star())).collect(Collectors.toList());
        List<Body> scoopableArrivalStars = new ArrayList<>(arrivalBodiesHavingSpectralClass.size() / 2);

        Map<String, List<Body>> bodiesBySpectralClass = arrivalBodiesHavingSpectralClass.stream().collect(Collectors.groupingBy(Body::getSpectral_class));
        for (String spectralClass : bodiesBySpectralClass.keySet()) {
            float spectralClassPercentOfAllArrivalStars = (100f * bodiesBySpectralClass.get(spectralClass).size()) / arrivalBodiesHavingSpectralClass.size();
            logger.trace(String.format("Spectral class %-20s: %d bodies (%.1f%%)", spectralClass, bodiesBySpectralClass.get(spectralClass).size(), spectralClassPercentOfAllArrivalStars));
            if (scoopableSpectralClasses.contains(spectralClass)) {
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

}
