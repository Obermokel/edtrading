package borg.edtrading;

import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbBody.MaterialShare;
import borg.edtrading.util.MiscUtil;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.SearchQuery;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.TreeMap;
import java.util.stream.Collectors;

/**
 * QuickAndDirtyTests
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class QuickAndDirtyTests {

    static final Logger logger = LogManager.getLogger(QuickAndDirtyTests.class);

    public static void main(String[] args) throws Exception {
        AnnotationConfigApplicationContext appctx = new AnnotationConfigApplicationContext(Config.class);
        try {
            final MutableInt total = new MutableInt(0);
            LinkedHashMap<String, Integer> nByClass = new LinkedHashMap<>();
            TreeMap<String, List<Float>> sharesByMat = new TreeMap<>();
            ElasticsearchTemplate elasticsearchTemplate = appctx.getBean(ElasticsearchTemplate.class);
            SearchQuery searchQuery = new NativeSearchQueryBuilder().withQuery(QueryBuilders.matchAllQuery()).withIndices("eddbbody").withTypes("eddbbody").withPageable(new PageRequest(0, 1000)).build();
            String scrollId = elasticsearchTemplate.scan(searchQuery, 1000, false);
            boolean hasRecords = true;
            while (hasRecords) {
                Page<EddbBody> page = elasticsearchTemplate.scroll(scrollId, 5000, EddbBody.class);
                if (page.hasContent()) {
                    page.forEach(b -> {
                        if (StringUtils.isNotEmpty(b.getSpectralClass())) {
                            total.increment();
                            nByClass.put(b.getSpectralClass(), nByClass.getOrDefault(b.getSpectralClass(), 0) + 1);
                        }
                        if (b.getMaterials() != null) {
                            for (MaterialShare matshare : b.getMaterials()) {
                                if (StringUtils.isNotEmpty(matshare.getName()) && matshare.getShare() != null) {
                                    List<Float> shares = sharesByMat.get(matshare.getName());
                                    if (shares == null) {
                                        shares = new ArrayList<>();
                                        sharesByMat.put(matshare.getName(), shares);
                                    }
                                    shares.add(matshare.getShare());
                                }
                            }
                        }
                    });
                } else {
                    hasRecords = false;
                }
            }
            elasticsearchTemplate.clearScroll(scrollId);
            MiscUtil.sortMapByValue(nByClass);
            for (String spectralClass : nByClass.keySet()) {
                System.out.println(String.format(Locale.US, "%,11dx %s", nByClass.get(spectralClass), spectralClass));
            }
            System.out.println(String.format(Locale.US, "%,11dx total", total.intValue()));
            for (String mat : sharesByMat.keySet()) {
                List<Float> shares = sharesByMat.get(mat);
                Collections.sort(shares);
                int n = shares.size();
                float median = shares.get((shares.size() * 5) / 10);
                float top80 = shares.get((shares.size() * 8) / 10);
                float top90 = shares.get((shares.size() * 9) / 10);
                float top99 = shares.get((shares.size() * 99) / 100);
                double avg = shares.stream().collect(Collectors.averagingDouble(s -> (double) s));
                System.out.println(String.format(Locale.US, "%,11dx %-20s AVG=%4.1f%%    MED=%4.1f%%    T80=%4.1f%%    T90=%4.1f%%    T99=%4.1f%%", n, mat, avg, median, top80, top90, top99));
            }
            File csvFile = new File(Constants.TEMP_DIR, "mats.csv");
            FileUtils.write(csvFile, "Percentage;" + sharesByMat.keySet().stream().collect(Collectors.joining(";")) + "\r\n", "ISO-8859-1", false);
            for (float percentage = 0.1f; percentage <= 50.0f; percentage += 0.1f) {
                FileUtils.write(csvFile, String.format(Locale.GERMANY, "%.1f", percentage), "ISO-8859-1", true);
                for (String mat : sharesByMat.keySet()) {
                    int n = 0;
                    for (float share : sharesByMat.get(mat)) {
                        if (Math.abs(share - percentage) <= 0.01f) {
                            n++;
                        }
                    }
                    FileUtils.write(csvFile, String.format(Locale.GERMANY, ";%d", n), "ISO-8859-1", true);
                }
                FileUtils.write(csvFile, "\r\n", "ISO-8859-1", true);
            }
        } finally {
            appctx.close();
        }
        //        Galaxy galaxy = Galaxy.readDataFromFiles();
        //        StarSystem sol = galaxy.searchStarSystemByName("Sol");
        //        Map<Long, List<Station>> stationsBySystem = galaxy.getStationsById().values().stream().collect(Collectors.groupingBy(Station::getStarSystemId));
        //        List<Faction> nonPlayerFactions = galaxy.getFactionsById().values().stream().filter(f -> !f.isPlayerFaction()).collect(Collectors.toList());
        //
        //        Map<String, List<Faction>> factionsByState = nonPlayerFactions.stream().filter(f -> f.getState() != null).collect(Collectors.groupingBy(Faction::getState));
        //        for (String key : factionsByState.keySet()) {
        //            System.out.println(String.format(Locale.US, "%6dx %s", factionsByState.get(key).size(), key));
        //        }
        //        System.out.println("--------");
        //
        //        Map<String, List<Faction>> factionsByAllegiance = nonPlayerFactions.stream().filter(f -> f.getAllegiance() != null).collect(Collectors.groupingBy(Faction::getAllegiance));
        //        for (String key : factionsByAllegiance.keySet()) {
        //            System.out.println(String.format(Locale.US, "%6dx %s", factionsByAllegiance.get(key).size(), key));
        //        }
        //        System.out.println("--------");
        //
        //        List<Faction> factionsAtWar = factionsByState.get("Civil War");
        //        List<Faction> empireFactions = factionsByAllegiance.get("Empire");
        //        Collection<Faction> empireFactionsAtWar = CollectionUtils.intersection(factionsAtWar, empireFactions);
        //        System.out.println(empireFactionsAtWar.size() + " empire factions at war");
        //        System.out.println("--------");
        //
        //        empireFactionsAtWar.stream().filter(f -> f.getHomeSystem() != null).sorted((f1, f2) -> new Float(f1.getHomeSystem().distanceTo(sol)).compareTo(new Float(f2.getHomeSystem().distanceTo(sol)))).forEach(faction -> {
        //            StarSystem homeSystem = faction.getHomeSystem();
        //            float distanceFromSol = homeSystem.distanceTo(sol);
        //            List<Station> stationsInThatSystem = stationsBySystem.get(homeSystem.getId());
        //            if (stationsInThatSystem != null) {
        //                List<Station> controlledStations = stationsInThatSystem.stream().filter(s -> faction.equals(s.getControllingMinorFaction())).sorted((s1, s2) -> new Float(s1.getDistanceFromStarInLs()).compareTo(new Float(s2.getDistanceFromStarInLs())))
        //                        .collect(Collectors.toList());
        //                if (!controlledStations.isEmpty()) {
        //                    List<Faction> factionsInThatSystem = stationsInThatSystem.stream().filter(s -> s.getControllingMinorFaction() != null).map(s -> s.getControllingMinorFaction()).distinct()
        //                            .sorted((f1, f2) -> f1.getName().toLowerCase().compareTo(f2.getName().toLowerCase())).collect(Collectors.toList());
        //                    factionsInThatSystem.remove(faction); // Remove home faction
        //                    factionsInThatSystem.retainAll(factionsAtWar); // Keep only those also at war
        //                    if (factionsInThatSystem.size() > 0) {
        //                        System.out.println(String.format(Locale.US, "%.0f Ly from Sol: \"%s\" with %d station(s) controlled by \"%s\"", distanceFromSol, homeSystem.getName(), controlledStations.size(), faction.getName()));
        //                        for (Faction f : factionsInThatSystem) {
        //                            System.out.println(String.format(Locale.US, "- %-48s\t%-16s\t%-16s", f.getName(), f.getAllegiance(), f.getState()));
        //                        }
        //                        System.out.println();
        //                    }
        //                }
        //            }
        //        });
    }

}
