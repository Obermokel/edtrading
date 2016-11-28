package borg.edtrading.services.impl;

import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.eddb.repositories.EddbBodyRepository;
import borg.edtrading.eddb.repositories.EddbSystemRepository;
import borg.edtrading.services.EddbService;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.SearchQuery;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
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

        // TODO Optimize searchQuery to some 100 Ly
        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(QueryBuilders.matchAllQuery()).withIndices("eddb").withTypes("system").withPageable(new PageRequest(0, 1000)).build();
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
    public List<EddbBody> searchArrivalNeutronStars() {
        Map<String, Set<EddbBody>> arrivalStarsBySpectralClass = this.mapStarsBySpectralClass(true);
        Set<EddbBody> arrivalNeutronStars = arrivalStarsBySpectralClass.get("NS");
        arrivalNeutronStars.addAll(this.findMappingProjectNeutronStars());
        return new ArrayList<>(arrivalNeutronStars);
    }

    @Override
    public List<EddbBody> searchArrivalUnscoopableStars() {
        Map<String, Set<EddbBody>> arrivalStarsBySpectralClass = this.mapStarsBySpectralClass(true);
        arrivalStarsBySpectralClass.remove("O");
        arrivalStarsBySpectralClass.remove("B");
        arrivalStarsBySpectralClass.remove("A");
        arrivalStarsBySpectralClass.remove("F");
        arrivalStarsBySpectralClass.remove("G");
        arrivalStarsBySpectralClass.remove("K");
        arrivalStarsBySpectralClass.remove("K_RedGiant");
        arrivalStarsBySpectralClass.remove("K_OrangeGiant");
        arrivalStarsBySpectralClass.remove("M");
        arrivalStarsBySpectralClass.remove("M_RedGiant");
        arrivalStarsBySpectralClass.remove("M_OrangeGiant");

        List<EddbBody> result = new ArrayList<>();
        for (String unscoopableClass : arrivalStarsBySpectralClass.keySet()) {
            result.addAll(arrivalStarsBySpectralClass.get(unscoopableClass));
        }
        return result;
    }

    @Override
    public Map<String, Set<EddbBody>> mapStarsBySpectralClass(boolean arrivalOnly) {
        Map<String, Set<EddbBody>> result = new TreeMap<>();

        SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(QueryBuilders.matchAllQuery()).withIndices("eddb").withTypes("body").withPageable(new PageRequest(0, 1000)).build();
        String scrollId = this.elasticsearchTemplate.scan(searchQuery, 1000, false);
        boolean hasRecords = true;
        while (hasRecords) {
            Page<EddbBody> page = this.elasticsearchTemplate.scroll(scrollId, 5000, EddbBody.class);
            if (page.hasContent()) {
                for (EddbBody body : page.getContent()) {
                    if (arrivalOnly && !Boolean.TRUE.equals(body.getIsMainStar())) {
                        // Skip
                    } else {
                        String spectralClass = body.getSpectralClass();
                        if (StringUtils.isEmpty(spectralClass)) {
                            if (EddbBody.TYPE_ID_BLACK_HOLE.equals(body.getTypeId())) {
                                spectralClass = "BH";
                            } else if (EddbBody.TYPE_ID_SUPERMASSIVE_BLACK_HOLE.equals(body.getTypeId())) {
                                spectralClass = "SMBH";
                            } else if (EddbBody.TYPE_ID_NEUTRON_STAR.equals(body.getTypeId())) {
                                spectralClass = "NS";
                            }
                        }
                        if (StringUtils.isNotEmpty(spectralClass)) {
                            Set<EddbBody> stars = result.getOrDefault(spectralClass, new HashSet<>());
                            stars.add(body);
                            result.put(spectralClass, stars);
                        }
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
    public List<EddbSystem> loadAllSystems() {
        List<EddbSystem> result = new ArrayList<>();

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
