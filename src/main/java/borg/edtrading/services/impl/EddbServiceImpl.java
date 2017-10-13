package borg.edtrading.services.impl;

import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddb.reader.EddbReader;
import borg.edtrading.eddb.repositories.EddbBodyRepository;
import borg.edtrading.eddb.repositories.EddbSystemRepository;
import borg.edtrading.services.EddbService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.SearchQuery;
import org.springframework.data.util.CloseableIterator;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * EddbServiceImpl
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
@Service
public class EddbServiceImpl implements EddbService {

    static final Logger logger = LogManager.getLogger(EddbServiceImpl.class);

    @Autowired
    private EddbReader eddbReader = null;

    @Autowired
    private EddbSystemRepository systemRepository = null;

    @Autowired
    private EddbBodyRepository bodyRepository = null;

    @Autowired
    private ElasticsearchTemplate elasticsearchTemplate = null;

    @Override
    public List<EddbSystem> loadAllSystems() {
        List<EddbSystem> result = new ArrayList<>();

        logger.debug("Loading all systems");

        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(QueryBuilders.matchAllQuery()).withIndices("eddbsystem").withPageable(new PageRequest(0, 1000)).build();
        try (CloseableIterator<EddbSystem> stream = this.elasticsearchTemplate.stream(searchQuery, EddbSystem.class)) {
            while (stream.hasNext()) {
                result.add(stream.next());
            }
        }

        return result;
    }

    @Override
    public EddbSystem findSystemByName(String name) {
        Page<EddbSystem> page = this.systemRepository.findByName(name, new PageRequest(0, 10));
        if (page.getTotalElements() == 0) {
            return null;
        } else if (page.getTotalElements() == 1) {
            return page.getContent().get(0);
        } else {
            throw new RuntimeException("Found " + page.getTotalElements() + " systems for name '" + name + "': " + page.getContent());
        }
    }

    @Override
    public EddbSystem findNearestSystem(Coord coord) {
        logger.debug("Searching closest system to " + coord);

        EddbSystem nearestSystem = null;
        Float nearestSystemDistance = null;
        for (float range = 2; range <= 16384 && nearestSystem == null; range *= 2) {
            Page<EddbSystem> page = this.findSystemsNear(coord, range, new PageRequest(0, 1000));
            while (page != null) {
                List<EddbSystem> systems = page.getContent();
                for (EddbSystem system : systems) {
                    float distance = system.getCoord().distanceTo(coord);
                    if (nearestSystemDistance == null || distance < nearestSystemDistance) {
                        nearestSystemDistance = distance;
                        nearestSystem = system;
                    }
                }
                if (page.hasNext()) {
                    page = this.findSystemsNear(coord, range, page.nextPageable());
                } else {
                    page = null;
                }
            }
        }

        return nearestSystem;
    }

    @Override
    public List<EddbBody> findBodiesOfSystem(Long systemId) {
        return this.bodyRepository.findBySystemId(systemId, new PageRequest(0, 1000)).getContent();
    }

    @Override
    public Map<String, List<EddbBody>> mapStarsBySpectralClass(boolean arrivalOnly) {
        Map<String, List<EddbBody>> result = new TreeMap<>();

        BoolQueryBuilder combinedQuery = QueryBuilders.boolQuery();
        if (arrivalOnly) {
            combinedQuery.must(QueryBuilders.termQuery("isMainStar", true));
        }
        BoolQueryBuilder isStarQuery = QueryBuilders.boolQuery();
        isStarQuery.should(QueryBuilders.existsQuery("spectralClass"));
        isStarQuery.should(QueryBuilders.rangeQuery("typeId").gte(1L).lte(3L)); // FIXME Relies on BH, SMBH and NS to be exactly those IDs...
        combinedQuery.must(isStarQuery);

        logger.debug("Mapping" + (arrivalOnly ? " arrival" : "") + " stars by spectral class");

        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(combinedQuery).withIndices("eddbbody").withTypes("eddbbody").withPageable(new PageRequest(0, 1000)).build();
        try (CloseableIterator<EddbBody> stream = this.elasticsearchTemplate.stream(searchQuery, EddbBody.class)) {
            while (stream.hasNext()) {
                EddbBody body = stream.next();

                String spectralClass = body.getSpectralClass();
                if (spectralClass == null) {
                    if (EddbBody.TYPE_ID_BLACK_HOLE.equals(body.getTypeId())) {
                        spectralClass = "BH";
                    } else if (EddbBody.TYPE_ID_SUPERMASSIVE_BLACK_HOLE.equals(body.getTypeId())) {
                        spectralClass = "SMBH";
                    } else if (EddbBody.TYPE_ID_NEUTRON_STAR.equals(body.getTypeId())) {
                        spectralClass = "NS";
                    }
                }
                List<EddbBody> stars = result.get(spectralClass);
                if (stars == null) {
                    stars = new ArrayList<>();
                    result.put(spectralClass, stars);
                }
                stars.add(body);
            }
        }

        if (logger.isDebugEnabled()) {
            for (String spectralClass : result.keySet()) {
                logger.debug(String.format(Locale.US, "%,11dx %s", result.get(spectralClass).size(), spectralClass));
            }
        }

        return result;
    }

    @Override
    public List<EddbBody> retainStarsOfSpectralClasses(Map<String, List<EddbBody>> starsBySpectralClass, String... spectralClasses) {
        List<EddbBody> result = new ArrayList<>();

        for (String spectralClass : spectralClasses) {
            List<EddbBody> stars = starsBySpectralClass.get(spectralClass);
            if (stars != null) {
                result.addAll(stars);
            }
        }

        return result;
    }

    @Override
    public List<EddbBody> removeStarsOfSpectralClasses(Map<String, List<EddbBody>> starsBySpectralClass, String... spectralClasses) {
        List<EddbBody> result = new ArrayList<>();

        Set<String> remove = new HashSet<>(Arrays.asList(spectralClasses));
        for (String spectralClass : starsBySpectralClass.keySet()) {
            if (!remove.contains(spectralClass)) {
                result.addAll(starsBySpectralClass.get(spectralClass));
            }
        }

        return result;
    }

    @Override
    public void updateEddbData(boolean forceReindex, boolean deleteOldEntities) {
        try {
            this.eddbReader.loadEddbDataIntoElasticsearch(forceReindex, deleteOldEntities);
        } catch (IOException e) {
            throw new RuntimeException("Failed to update EDDB data", e);
        }
    }

    private Set<EddbBody> findMappingProjectNeutronStars() {
        Set<EddbBody> neutronStars = new HashSet<>();

        //        File neutronStarNamesFile = new File(Constants.EDTRADING_BASE_DIR, "neutron stars.txt");
        //        File neutronStarIdsFile = new File(Constants.EDTRADING_BASE_DIR, "neutron stars.dat");
        //
        //        if (!neutronStarIdsFile.exists() || neutronStarNamesFile.lastModified() > neutronStarIdsFile.lastModified()) {
        //            if (name.startsWith(starSystem.getName().toLowerCase())) {
        //                if (StringUtils.getLevenshteinDistance(name, starSystem.getName().toLowerCase()) <= 4) {
        //                    result.add(starSystem);
        //                }
        //            }
        //
        //            FileUtils.write(neutronStarIdsFile, neutronStars.stream().map(ss -> String.valueOf(ss.getId())).collect(Collectors.joining("\n")), "UTF-8", false);
        //        } else {
        //            for (String line : FileUtils.readLines(neutronStarIdsFile, "UTF-8")) {
        //                neutronStars.add(this.bodyRepository.findOne(Long.valueOf(line)));
        //            }
        //        }

        return neutronStars;
    }

    @Override
    public Page<EddbSystem> findSystemsNear(Coord coord, float maxDistance, Pageable pageable) {
        return this.findSystemsWithin(coord.getX() - maxDistance, coord.getX() + maxDistance, coord.getY() - maxDistance, coord.getY() + maxDistance, coord.getZ() - maxDistance, coord.getZ() + maxDistance, pageable);
    }

    @Override
    public Page<EddbSystem> findSystemsWithin(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, Pageable pageable) {
        return this.systemRepository.findByCoordWithin(xmin, xmax, ymin, ymax, zmin, zmax, pageable);
    }

    @Override
    public Page<EddbBody> findBodiesNear(Coord coord, float maxDistance, Pageable pageable) {
        return this.findBodiesWithin(coord.getX() - maxDistance, coord.getX() + maxDistance, coord.getY() - maxDistance, coord.getY() + maxDistance, coord.getZ() - maxDistance, coord.getZ() + maxDistance, pageable);
    }

    @Override
    public Page<EddbBody> findBodiesWithin(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, Pageable pageable) {
        return this.bodyRepository.findByCoordWithin(xmin, xmax, ymin, ymax, zmin, zmax, pageable);
    }

    @Override
    public Page<EddbBody> findStarsNear(Coord coord, float maxDistance, Boolean isMainStar, Collection<String> starClasses, Pageable pageable) {
        return this.findStarsWithin(coord.getX() - maxDistance, coord.getX() + maxDistance, coord.getY() - maxDistance, coord.getY() + maxDistance, coord.getZ() - maxDistance, coord.getZ() + maxDistance, isMainStar, starClasses, pageable);
    }

    @Override
    public Page<EddbBody> findStarsWithin(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, Boolean isMainStar, Collection<String> starClasses, Pageable pageable) {
        BoolQueryBuilder qb = QueryBuilders.boolQuery();
        qb.must(QueryBuilders.boolQuery().should(QueryBuilders.termQuery("groupId", 1L)).should(QueryBuilders.termQuery("groupId", 2L))); // Compact star or star
        qb.must(QueryBuilders.rangeQuery("coord.x").gte(xmin).lte(xmax));
        qb.must(QueryBuilders.rangeQuery("coord.y").gte(ymin).lte(ymax));
        qb.must(QueryBuilders.rangeQuery("coord.z").gte(zmin).lte(zmax));
        if (Boolean.TRUE.equals(isMainStar)) {
            qb.must(QueryBuilders.termQuery("isMainStar", true));
        } else if (Boolean.FALSE.equals(isMainStar)) {
            qb.must(QueryBuilders.termQuery("isMainStar", false));
        }
        if (starClasses != null && !starClasses.isEmpty()) {
            BoolQueryBuilder starClassIn = QueryBuilders.boolQuery();
            for (String starClass : starClasses) {
                starClassIn.should(QueryBuilders.termQuery("starClass", starClass));
            }
            qb.must(starClassIn);
        }
        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(qb).withIndices("eddbbody").withPageable(pageable).build();
        return this.elasticsearchTemplate.queryForPage(searchQuery, EddbBody.class);
    }

    @Override
    public Page<EddbBody> findPlanetsNear(Coord coord, float maxDistance, Boolean isTerraformingCandidate, Collection<Long> types, Pageable pageable) {
        return this.findPlanetsWithin(coord.getX() - maxDistance, coord.getX() + maxDistance, coord.getY() - maxDistance, coord.getY() + maxDistance, coord.getZ() - maxDistance, coord.getZ() + maxDistance, isTerraformingCandidate, types, pageable);
    }

    @Override
    public Page<EddbBody> findPlanetsWithin(float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, Boolean isTerraformingCandidate, Collection<Long> types, Pageable pageable) {
        BoolQueryBuilder qb = QueryBuilders.boolQuery();
        qb.must(QueryBuilders.termQuery("groupId", 6L)); // Planet
        qb.must(QueryBuilders.rangeQuery("coord.x").gte(xmin).lte(xmax));
        qb.must(QueryBuilders.rangeQuery("coord.y").gte(ymin).lte(ymax));
        qb.must(QueryBuilders.rangeQuery("coord.z").gte(zmin).lte(zmax));
        if (Boolean.TRUE.equals(isTerraformingCandidate)) {
            qb.must(QueryBuilders.termQuery("terraformingStateId", EddbBody.TERRAFORMING_STATE_ID_CANDIDATE_FOR_TERRAFORMING));
        } else if (Boolean.FALSE.equals(isTerraformingCandidate)) {
            qb.mustNot(QueryBuilders.termQuery("terraformingStateId", EddbBody.TERRAFORMING_STATE_ID_CANDIDATE_FOR_TERRAFORMING));
        }
        if (types != null && !types.isEmpty()) {
            BoolQueryBuilder typeIn = QueryBuilders.boolQuery();
            for (Long type : types) {
                typeIn.should(QueryBuilders.termQuery("typeId", type));
            }
            qb.must(typeIn);
        }
        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(qb).withIndices("eddbbody").withPageable(pageable).build();
        return this.elasticsearchTemplate.queryForPage(searchQuery, EddbBody.class);
    }

}
