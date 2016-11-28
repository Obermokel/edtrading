package borg.edtrading.services.impl;

import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddb.repositories.EddbBodyRepository;
import borg.edtrading.eddb.repositories.EddbSystemRepository;
import borg.edtrading.services.EddbService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.index.query.RangeQueryBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.SearchQuery;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
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
    private EddbSystemRepository systemRepository = null;

    @Autowired
    private EddbBodyRepository bodyRepository = null;

    @Autowired
    private ElasticsearchTemplate elasticsearchTemplate = null;

    @Override
    public EddbSystem searchSystemByName(String name) {
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
    public EddbSystem searchClosestSystemByCoord(Coord coord) {
        EddbSystem result = null;
        float closest = 999999f;

        logger.debug("Searching closest system to " + coord);

        double lookaround = 250.0;
        RangeQueryBuilder rangeX = QueryBuilders.rangeQuery("x").from(coord.getX() - lookaround).to(coord.getX() + lookaround);
        RangeQueryBuilder rangeY = QueryBuilders.rangeQuery("y").from(coord.getY() - lookaround).to(coord.getY() + lookaround);
        RangeQueryBuilder rangeZ = QueryBuilders.rangeQuery("z").from(coord.getZ() - lookaround).to(coord.getZ() + lookaround);
        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(QueryBuilders.boolQuery().must(rangeX).must(rangeY).must(rangeZ)).withIndices("eddb").withTypes("system").withPageable(new PageRequest(0, 1000)).build();
        String scrollId = this.elasticsearchTemplate.scan(searchQuery, 1000, false);
        boolean hasRecords = true;
        while (hasRecords) {
            Page<EddbSystem> page = this.elasticsearchTemplate.scroll(scrollId, 5000, EddbSystem.class);
            if (page.hasContent()) {
                for (EddbSystem s : page.getContent()) {
                    float dist = s.getCoord().distanceTo(coord);
                    if (dist < closest) {
                        closest = dist;
                        result = s;
                    }
                }
            } else {
                hasRecords = false;
            }
        }
        this.elasticsearchTemplate.clearScroll(scrollId);

        return result;
    }

    @Override
    public List<EddbBody> retainStarsOfSpectralClasses(Map<String, Set<EddbBody>> starsBySpectralClass, String... spectralClasses) {
        List<EddbBody> result = new ArrayList<>();

        for (String spectralClass : spectralClasses) {
            Set<EddbBody> stars = starsBySpectralClass.get(spectralClass);
            if (stars != null) {
                result.addAll(stars);
            }
        }

        return result;
    }

    @Override
    public List<EddbBody> removeStarsOfSpectralClasses(Map<String, Set<EddbBody>> starsBySpectralClass, String... spectralClasses) {
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
    public Map<String, Set<EddbBody>> mapStarsBySpectralClass(boolean arrivalOnly) {
        Map<String, Set<EddbBody>> result = new TreeMap<>();

        BoolQueryBuilder combinedQuery = QueryBuilders.boolQuery();
        if (arrivalOnly) {
            combinedQuery.must(QueryBuilders.termQuery("isMainStar", true));
        }
        BoolQueryBuilder isStarQuery = QueryBuilders.boolQuery();
        isStarQuery.should(QueryBuilders.existsQuery("spectralClass"));
        isStarQuery.should(QueryBuilders.rangeQuery("typeId").gte(1L).lte(3L)); // FIXME Relies on BH, SMBH and NS to be exactly those IDs...
        combinedQuery.must(isStarQuery);

        logger.debug("Mapping" + (arrivalOnly ? " arrival" : "") + " stars by spectral class");

        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(combinedQuery).withIndices("eddb").withTypes("body").withPageable(new PageRequest(0, 1000)).build();
        String scrollId = this.elasticsearchTemplate.scan(searchQuery, 1000, false);
        boolean hasRecords = true;
        while (hasRecords) {
            Page<EddbBody> page = this.elasticsearchTemplate.scroll(scrollId, 5000, EddbBody.class);
            if (page.hasContent()) {
                for (EddbBody body : page.getContent()) {
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
                    Set<EddbBody> stars = result.get(spectralClass);
                    if (stars == null) {
                        stars = new HashSet<>();
                        result.put(spectralClass, stars);
                    }
                    stars.add(body);
                }
            } else {
                hasRecords = false;
            }
        }
        this.elasticsearchTemplate.clearScroll(scrollId);

        if (logger.isDebugEnabled()) {
            for (String spectralClass : result.keySet()) {
                logger.debug(String.format(Locale.US, "%,11dx %s", result.get(spectralClass).size(), spectralClass));
            }
        }

        return result;
    }

    @Override
    public List<EddbSystem> loadAllSystems() {
        List<EddbSystem> result = new ArrayList<>();

        logger.debug("Loading all systems");

        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(QueryBuilders.matchAllQuery()).withIndices("eddb").withTypes("system").withPageable(new PageRequest(0, 1000)).build();
        String scrollId = this.elasticsearchTemplate.scan(searchQuery, 1000, false);
        boolean hasRecords = true;
        while (hasRecords) {
            Page<EddbSystem> page = this.elasticsearchTemplate.scroll(scrollId, 5000, EddbSystem.class);
            if (page.hasContent()) {
                result.addAll(page.getContent());
            } else {
                hasRecords = false;
            }
        }
        this.elasticsearchTemplate.clearScroll(scrollId);

        return result;
    }

    private Set<EddbBody> findMappingProjectNeutronStars() {
        Set<EddbBody> neutronStars = new HashSet<>();

        // TODO
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

}
