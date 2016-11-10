package borg.edtrading.aystar;

import borg.edtrading.data.Coord;
import borg.edtrading.data.StarSystem;
import borg.edtrading.util.FuelAndJumpRangeLookup;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * AyStar
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class AyStar {

    static final Logger logger = LogManager.getLogger(AyStar.class);

    private static final int SECTOR_SIZE = 50; // ly

    private PriorityQueue<Path> open = null;
    private Set<StarSystem> closed = null;
    private StarSystem goal = null;
    private Set<StarSystem> starSystemsWithNeutronStars = null;
    private Map<Coord, List<StarSystem>> starSystemsWithScoopableStarsBySector = null;
    private FuelAndJumpRangeLookup fuelJumpLUT = null;
    private float maxTotalDistanceLy = 0;
    private Path closestToGoalSoFar = null;

    public void initialize(StarSystem source, StarSystem goal, Set<StarSystem> starSystemsWithNeutronStars, Set<StarSystem> starSystemsWithScoopableStars, FuelAndJumpRangeLookup fuelJumpLUT) {
        if (!starSystemsWithNeutronStars.contains(goal) && !starSystemsWithScoopableStars.contains(goal)) {
            throw new IllegalArgumentException("goal not in useable star systems");
        } else {
            this.fuelJumpLUT = fuelJumpLUT;
            this.open = new PriorityQueue<>(new LeastJumpsComparator(source.distanceTo(goal), fuelJumpLUT.getJumpRangeFuelOpt()));
            this.closed = new HashSet<>();
            this.goal = goal;
            this.starSystemsWithNeutronStars = starSystemsWithNeutronStars;
            this.starSystemsWithScoopableStarsBySector = mapBySector(starSystemsWithScoopableStars);
            this.maxTotalDistanceLy = 1.5f * source.distanceTo(goal);
            this.closestToGoalSoFar = null;

            this.open.add(new Path(source, source.distanceTo(goal), fuelJumpLUT.getMaxFuelTons()));
        }
    }

    public Path findPath() {
        while (this.open.size() > 0) {
            Path path = this.open.poll();

            if (this.closed.contains(path.getStarSystem())) {
                // We already found a better path
                continue;
            } else {
                // Because we always poll the best path so far, the current path is
                // the best path to this system
                this.closed.add(path.getStarSystem());
            }

            if (this.closestToGoalSoFar == null || path.getRemainingDistanceLy() < this.closestToGoalSoFar.getRemainingDistanceLy()) {
                this.closestToGoalSoFar = path;
            }

            if (this.closed.size() % 10000 == 0 && logger.isTraceEnabled()) {
                logger.trace(String.format("Open: %,15d / Closed: %,15d || Closest so far: %s with %d jump(s), %.0fly travelled, %.0fly remaining", this.open.size(), this.closed.size(), this.closestToGoalSoFar.getStarSystem().toString(),
                        this.closestToGoalSoFar.getTotalJumps(), this.closestToGoalSoFar.getTravelledDistanceLy(), this.closestToGoalSoFar.getRemainingDistanceLy()));
            }

            if (path.getStarSystem().equals(this.goal)) {
                // We reached our destination
                return path;
            }

            List<StarSystem> neighbours = this.findNeighbours(path);

            final float boostValue = this.starSystemsWithNeutronStars.contains(path.getStarSystem()) ? 4.0f : 1.0f;
            for (StarSystem neighbour : neighbours) {
                if (!this.closed.contains(neighbour)) {
                    float remainingDistanceLy = neighbour.distanceTo(this.goal);
                    float extraTravelledDistanceLy = path.getStarSystem().distanceTo(neighbour);
                    float fuelLevel = this.fuelJumpLUT.getMaxFuelTons(); // Scoop until full by default
                    if (this.starSystemsWithNeutronStars.contains(neighbour)) {
                        fuelLevel = path.getFuelLevel() - this.fuelJumpLUT.lookupFuelUsage(extraTravelledDistanceLy / boostValue, path.getFuelLevel()); // Subtract from prev
                    }
                    Path newPath = new Path(path, neighbour, remainingDistanceLy, extraTravelledDistanceLy, fuelLevel);
                    if (newPath.getTravelledDistanceLy() + newPath.getRemainingDistanceLy() <= this.maxTotalDistanceLy) {
                        this.open.offer(newPath);
                    }
                }
            }
        }

        return null;
    }

    private List<StarSystem> findNeighbours(Path path) {
        final StarSystem currentStarSystem = path.getStarSystem();
        final Coord currentCoord = currentStarSystem.getCoord();
        float safeFuelLevel = path.getFuelLevel(); // This is what the calculation says, but as we don't know the formula we should add some safety
        if (safeFuelLevel > this.fuelJumpLUT.getMaxFuelPerJump()) {
            safeFuelLevel = Math.min(this.fuelJumpLUT.getMaxFuelTons(), safeFuelLevel + 2.0f); // Add 2 extra tons to reduce the calculated jump distance
        } else {
            safeFuelLevel = Math.max(0.1f, safeFuelLevel - 2.0f); // Subtract 2 tons to reduce the calculated jump distance
        }
        final float currentUnboostedJumpRange = this.fuelJumpLUT.lookupMaxJumpRange(safeFuelLevel);

        // Do we have an overcharged FSD?
        final boolean haveSuperchargedFsd = this.starSystemsWithNeutronStars.contains(currentStarSystem) ? true : false;

        // Do we need to scoop?
        boolean mustScoop = path.getFuelLevel() <= fuelJumpLUT.getMaxFuelPerJump();

        // Extra jump range because of empty tank?
        final float currentJumpRange = haveSuperchargedFsd ? 4 * currentUnboostedJumpRange : currentUnboostedJumpRange;

        // Find reachable systems
        List<StarSystem> systemsInRange = new ArrayList<>();
        if (!mustScoop) {
            List<StarSystem> neutronInRange = this.starSystemsWithNeutronStars.stream().filter(st -> st.distanceTo(currentStarSystem) <= currentJumpRange).collect(Collectors.toList());
            //logger.debug("Considering " + neutronInRange.size() + " neutrons in range: " + neutronInRange);
            systemsInRange.addAll(neutronInRange);
        }
        List<StarSystem> scoopableSystemsInCloseSectors = findSystemsBySector(this.starSystemsWithScoopableStarsBySector, currentCoord, currentJumpRange);
        systemsInRange.addAll(scoopableSystemsInCloseSectors.stream().filter(st -> st.distanceTo(currentStarSystem) <= currentJumpRange /*&& st.distanceTo(goal) < currentDistanceToGoal*/).collect(Collectors.toList()));

        // Finished
        return systemsInRange;
    }

    private static List<StarSystem> findSystemsBySector(Map<Coord, List<StarSystem>> systemsBySector, Coord currentCoord, float currentJumpRange) {
        List<StarSystem> result = new ArrayList<>();

        Coord currentSector = coordToSector(currentCoord);
        int nSideSectors = (int) Math.ceil(currentJumpRange / SECTOR_SIZE);
        for (int x = -nSideSectors; x <= nSideSectors; x++) {
            for (int y = -nSideSectors; y <= nSideSectors; y++) {
                for (int z = -nSideSectors; z <= nSideSectors; z++) {
                    Coord sector = new Coord(currentSector.getX() + x * SECTOR_SIZE, currentSector.getY() + y * SECTOR_SIZE, currentSector.getZ() + z * SECTOR_SIZE);
                    List<StarSystem> systems = systemsBySector.get(sector);
                    if (systems != null) {
                        result.addAll(systems);
                    }
                }
            }
        }
        //logger.debug("Found " + result.size() + " systems around sector " + currentSector + " with jump range = " + currentJumpRange);

        return result;
    }

    private Map<Coord, List<StarSystem>> mapBySector(Collection<StarSystem> starSystems) {
        Map<Coord, List<StarSystem>> result = new HashMap<>();
        for (StarSystem starSystem : starSystems) {
            Coord sector = coordToSector(starSystem.getCoord());
            List<StarSystem> systemsInThisSector = result.get(sector);
            if (systemsInThisSector == null) {
                systemsInThisSector = new ArrayList<>();
                result.put(sector, systemsInThisSector);
            }
            systemsInThisSector.add(starSystem);
        }
        logger.debug(String.format(Locale.US, "Mapped %,d systems into %,d sectors", starSystems.size(), result.size()));
        return result;
    }

    private static Coord coordToSector(Coord coord) {
        return new Coord(((int) coord.getX() / SECTOR_SIZE) * SECTOR_SIZE, ((int) coord.getY() / SECTOR_SIZE) * SECTOR_SIZE, ((int) coord.getZ() / SECTOR_SIZE) * SECTOR_SIZE);
    }

}
