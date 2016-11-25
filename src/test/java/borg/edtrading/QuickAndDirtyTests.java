package borg.edtrading;

import borg.edtrading.data.Faction;
import borg.edtrading.data.Galaxy;
import borg.edtrading.data.StarSystem;
import borg.edtrading.data.Station;
import org.apache.commons.collections.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * QuickAndDirtyTests
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class QuickAndDirtyTests {

    static final Logger logger = LogManager.getLogger(QuickAndDirtyTests.class);

    public static void main(String[] args) throws Exception {
        Galaxy galaxy = Galaxy.readDataFromFiles();
        StarSystem sol = galaxy.searchStarSystemByName("Sol");
        Map<Long, List<Station>> stationsBySystem = galaxy.getStationsById().values().stream().collect(Collectors.groupingBy(Station::getStarSystemId));
        List<Faction> nonPlayerFactions = galaxy.getFactionsById().values().stream().filter(f -> !f.isPlayerFaction()).collect(Collectors.toList());

        Map<String, List<Faction>> factionsByState = nonPlayerFactions.stream().filter(f -> f.getState() != null).collect(Collectors.groupingBy(Faction::getState));
        for (String key : factionsByState.keySet()) {
            System.out.println(String.format(Locale.US, "%6dx %s", factionsByState.get(key).size(), key));
        }
        System.out.println("--------");

        Map<String, List<Faction>> factionsByAllegiance = nonPlayerFactions.stream().filter(f -> f.getAllegiance() != null).collect(Collectors.groupingBy(Faction::getAllegiance));
        for (String key : factionsByAllegiance.keySet()) {
            System.out.println(String.format(Locale.US, "%6dx %s", factionsByAllegiance.get(key).size(), key));
        }
        System.out.println("--------");

        List<Faction> factionsAtWar = factionsByState.get("Civil War");
        List<Faction> empireFactions = factionsByAllegiance.get("Empire");
        Collection<Faction> empireFactionsAtWar = CollectionUtils.intersection(factionsAtWar, empireFactions);
        System.out.println(empireFactionsAtWar.size() + " empire factions at war");
        System.out.println("--------");

        empireFactionsAtWar.stream().filter(f -> f.getHomeSystem() != null).sorted((f1, f2) -> new Float(f1.getHomeSystem().distanceTo(sol)).compareTo(new Float(f2.getHomeSystem().distanceTo(sol)))).forEach(faction -> {
            StarSystem homeSystem = faction.getHomeSystem();
            float distanceFromSol = homeSystem.distanceTo(sol);
            List<Station> stationsInThatSystem = stationsBySystem.get(homeSystem.getId());
            if (stationsInThatSystem != null) {
                List<Station> controlledStations = stationsInThatSystem.stream().filter(s -> faction.equals(s.getControllingMinorFaction())).sorted((s1, s2) -> new Float(s1.getDistanceFromStarInLs()).compareTo(new Float(s2.getDistanceFromStarInLs())))
                        .collect(Collectors.toList());
                if (!controlledStations.isEmpty()) {
                    List<Faction> factionsInThatSystem = stationsInThatSystem.stream().filter(s -> s.getControllingMinorFaction() != null).map(s -> s.getControllingMinorFaction()).distinct()
                            .sorted((f1, f2) -> f1.getName().toLowerCase().compareTo(f2.getName().toLowerCase())).collect(Collectors.toList());
                    factionsInThatSystem.remove(faction); // Remove home faction
                    factionsInThatSystem.retainAll(factionsAtWar); // Keep only those also at war
                    if (factionsInThatSystem.size() > 0) {
                        System.out.println(String.format(Locale.US, "%.0f Ly from Sol: \"%s\" with %d station(s) controlled by \"%s\"", distanceFromSol, homeSystem.getName(), controlledStations.size(), faction.getName()));
                        for (Faction f : factionsInThatSystem) {
                            System.out.println(String.format(Locale.US, "- %-48s\t%-16s\t%-16s", f.getName(), f.getAllegiance(), f.getState()));
                        }
                        System.out.println();
                    }
                }
            }
        });
    }

}
