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
    private Set<MinimizedStarSystem> closed = null;
    private MinimizedStarSystem goal = null;
    private Set<MinimizedStarSystem> starSystemsWithNeutronStars = null;
    private Map<Coord, List<MinimizedStarSystem>> starSystemsWithScoopableStarsBySector = null;
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
            this.goal = new MinimizedStarSystem(goal);
            this.starSystemsWithNeutronStars = starSystemsWithNeutronStars.stream().map(ss -> new MinimizedStarSystem(ss)).collect(Collectors.toSet());
            this.starSystemsWithScoopableStarsBySector = mapBySector(starSystemsWithScoopableStars.stream().map(ss -> new MinimizedStarSystem(ss)).collect(Collectors.toSet()));
            this.maxTotalDistanceLy = 1.5f * source.distanceTo(goal);
            this.closestToGoalSoFar = null;

            this.open.add(new Path(new MinimizedStarSystem(source), source.distanceTo(goal), fuelJumpLUT.getMaxFuelTons()));
        }
    }

    public Path findPath() {
        while (this.open.size() > 0) {
            Path path = this.open.poll();

            if (this.closed.contains(path.getMinimizedStarSystem())) {
                // We already found a better path
                continue;
            } else {
                // Because we always poll the best path so far, the current path is
                // the best path to this system
                this.closed.add(path.getMinimizedStarSystem());
            }

            if (this.closestToGoalSoFar == null || path.getRemainingDistanceLy() < this.closestToGoalSoFar.getRemainingDistanceLy()) {
                this.closestToGoalSoFar = path;
            }

            if (this.closed.size() % 10000 == 0 && logger.isTraceEnabled()) {
                logger.trace(String.format("Open: %,15d / Closed: %,15d || Closest so far: %s with %d jump(s), %.0fly travelled, %.0fly remaining", this.open.size(), this.closed.size(), this.closestToGoalSoFar.getMinimizedStarSystem().toString(),
                        this.closestToGoalSoFar.getTotalJumps(), this.closestToGoalSoFar.getTravelledDistanceLy(), this.closestToGoalSoFar.getRemainingDistanceLy()));
            }

            if (path.getMinimizedStarSystem().equals(this.goal)) {
                // We reached our destination
                return path;
            }

            List<MinimizedStarSystem> neighbours = this.findNeighbours(path);

            final float boostValue = this.starSystemsWithNeutronStars.contains(path.getMinimizedStarSystem()) ? 4.0f : 1.0f;
            for (MinimizedStarSystem neighbour : neighbours) {
                if (!this.closed.contains(neighbour)) {
                    float remainingDistanceLy = neighbour.distanceTo(this.goal);
                    float extraTravelledDistanceLy = path.getMinimizedStarSystem().distanceTo(neighbour);
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

    private List<MinimizedStarSystem> findNeighbours(Path path) {
        final MinimizedStarSystem currentStarSystem = path.getMinimizedStarSystem();
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
        List<MinimizedStarSystem> scoopableSystemsInCloseSectors = findSystemsBySector(this.starSystemsWithScoopableStarsBySector, currentCoord, currentJumpRange);
        List<MinimizedStarSystem> systemsInRange = new ArrayList<>(scoopableSystemsInCloseSectors.size());
        for (MinimizedStarSystem s : scoopableSystemsInCloseSectors) {
            if (s.distanceTo(currentStarSystem) <= currentJumpRange) {
                systemsInRange.add(s);
            }
        }
        if (!mustScoop) {
            for (MinimizedStarSystem s : this.starSystemsWithNeutronStars) {
                if (s.distanceTo(currentStarSystem) <= currentJumpRange) {
                    systemsInRange.add(s);
                }
            }
        }

        // Finished
        return systemsInRange;
    }

    private static List<MinimizedStarSystem> findSystemsBySector(Map<Coord, List<MinimizedStarSystem>> systemsBySector, Coord currentCoord, float currentJumpRange) {
        List<MinimizedStarSystem> result = new ArrayList<>();

        Coord currentSector = coordToSector(currentCoord);
        int nSideSectors = (int) Math.ceil(currentJumpRange / SECTOR_SIZE);
        for (int x = -nSideSectors; x <= nSideSectors; x++) {
            for (int y = -nSideSectors; y <= nSideSectors; y++) {
                for (int z = -nSideSectors; z <= nSideSectors; z++) {
                    Coord sector = new Coord(currentSector.getX() + x * SECTOR_SIZE, currentSector.getY() + y * SECTOR_SIZE, currentSector.getZ() + z * SECTOR_SIZE);
                    List<MinimizedStarSystem> systems = systemsBySector.get(sector);
                    if (systems != null) {
                        result.addAll(systems);
                    }
                }
            }
        }
        //logger.debug("Found " + result.size() + " systems around sector " + currentSector + " with jump range = " + currentJumpRange);

        return result;
    }

    private Map<Coord, List<MinimizedStarSystem>> mapBySector(Collection<MinimizedStarSystem> starSystems) {
        Map<Coord, List<MinimizedStarSystem>> result = new HashMap<>();
        for (MinimizedStarSystem starSystem : starSystems) {
            Coord sector = coordToSector(starSystem.getCoord());
            List<MinimizedStarSystem> systemsInThisSector = result.get(sector);
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
