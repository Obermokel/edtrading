package borg.edtrading;

import borg.edtrading.cfg.Config;
import borg.edtrading.cfg.Constants;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.eddb.data.EddbSystem;
import borg.edtrading.gui.MapCreator;
import borg.edtrading.gui.MapCreator.MapView;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.SortBuilder;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.SearchQuery;

import java.awt.image.BufferedImage;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

/**
 * GalmapApp
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class GalmapAnimationApp {

    static final Logger logger = LogManager.getLogger(GalmapAnimationApp.class);

    private static final AnnotationConfigApplicationContext APPCTX = new AnnotationConfigApplicationContext(Config.class);

    private static final SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");

    public static void main(String[] args) throws Exception {
        //Date yesterday = DateUtils.addDays(DateUtils.truncate(new Date(), Calendar.DATE), -1);
        Date yesterday = df.parse("2016-01-01");
        //Date date = df.parse("2016-09-18"); // Where EDDB starts
        Date date = df.parse("2014-12-06");
        ElasticsearchTemplate elasticsearchTemplate = APPCTX.getBean(ElasticsearchTemplate.class);
        Map<Long, EddbBody> mainStarsBySystem = findMainStarsBySystem(elasticsearchTemplate);

        while (date.before(yesterday)) {
            System.out.print(df.format(date) + ": ");

            MapCreator mapCreator = new MapCreator(-43000, 41000, -3333, 3333, -18000, 66000, MapView.TOP, 4096);
            mapCreator.prepare();

            BoolQueryBuilder qb = QueryBuilders.boolQuery();
            try {
                long maxSystemId = estimateNumSystems(date);
                qb.must(QueryBuilders.rangeQuery("id").lte(maxSystemId));
                qb.must(QueryBuilders.rangeQuery("coord.y").from(-3333.0).to(3333.0));
            } catch (Exception e1) {
                qb.must(QueryBuilders.rangeQuery("updatedAt").lte(date.getTime()));
                qb.must(QueryBuilders.rangeQuery("coord.y").from(-3333.0).to(3333.0));
            }
            SortBuilder sb = SortBuilders.fieldSort("coord.y").order(SortOrder.ASC);
            SearchQuery searchQuery = new NativeSearchQueryBuilder().withIndices("eddbsystem").withQuery(qb).withSort(sb).withPageable(new PageRequest(0, 1000)).build();
            String scrollId = elasticsearchTemplate.scan(searchQuery, 1000, false);
            boolean hasRecords = true;
            while (hasRecords) {
                Page<EddbSystem> page = elasticsearchTemplate.scroll(scrollId, 5000, EddbSystem.class);
                if (page.hasContent()) {
                    System.out.print(".");
                    for (EddbSystem system : page.getContent()) {
                        String starClass = null;
                        try {
                            //                            CriteriaQuery cq = new CriteriaQuery(Criteria.where("systemId").is(system.getId()).and(Criteria.where("createdAt").lessThanEqual(date.getTime())).and(Criteria.where("isMainStar").is(true)));
                            //                            EddbBody mainStar = elasticsearchTemplate.queryForObject(cq, EddbBody.class);
                            //                            starClass = mainStar == null ? null : mainStar.getStarClass();

                            //                            List<EddbBody> bodies = bodyRepository.findBySystemId(system.getId(), new PageRequest(0, 999)).getContent();
                            //                            for (EddbBody body : bodies) {
                            //                                if (Boolean.TRUE.equals(body.getIsMainStar()) && body.getCreatedAt() != null && body.getCreatedAt().compareTo(date) <= 0) {
                            //                                    starClass = body.getStarClass();
                            //                                    break;
                            //                                }
                            //                            }

                            EddbBody mainStar = mainStarsBySystem.get(system.getId());
                            if (mainStar != null && mainStar.getCreatedAt() != null && mainStar.getCreatedAt().compareTo(date) <= 0) {
                                starClass = mainStar.getStarClass();
                            }
                        } catch (Exception e) {
                            // Ignore
                        }

                        mapCreator.drawStar(system.getCoord(), starClass, null);
                    }
                } else {
                    hasRecords = false;
                }
            }
            elasticsearchTemplate.clearScroll(scrollId);

            BufferedImage mapImage = mapCreator.finish();
            //            Graphics2D g = mapImage.createGraphics();
            //            g.setFont(new Font("Consolas", Font.BOLD, 256));
            //            g.setColor(Color.WHITE);
            //            g.drawString(df.format(date), 50, 300);
            //            g.dispose();
            ImageIO.write(mapImage, "png", new File(Constants.TEMP_DIR, "Map " + df.format(date) + ".png"));

            date = DateUtils.addDays(date, 1);

            System.out.println();
        }
    }

    private static long estimateNumSystems(Date at) throws Exception {
        // 2016-01-01: 200,000
        // 2016-09-18: 1,862,019
        Date maxDate = df.parse("2016-09-18"); // ~1,862,000 systems (EDDB start)
        //                       2016-06-29    // ~640,000 systems (Jaques Station discovered)
        Date refDate = df.parse("2016-01-01"); // ~200,000 systems (wild guess!)
        Date minDate = df.parse("2014-12-06"); // Release date for PC
        if (at.after(maxDate)) {
            throw new RuntimeException(at + " is after max date of " + maxDate);
        } else if (at.before(minDate)) {
            throw new RuntimeException(at + " is before min date of " + minDate);
        } else if (at.before(refDate)) {
            long millis = refDate.getTime() - at.getTime();
            long days = millis / DateUtils.MILLIS_PER_DAY;
            double numSystems = 200000.0;
            for (long l = 0; l < days; l++) {
                numSystems /= 1.009748;
            }
            return Math.round(numSystems);
        } else {
            long millis = at.getTime() - refDate.getTime();
            long days = millis / DateUtils.MILLIS_PER_DAY;
            double numSystems = 200000.0;
            for (long l = 0; l < days; l++) {
                numSystems *= 1.009748;
            }
            return Math.round(numSystems);
        }
    }

    private static Map<Long, EddbBody> findMainStarsBySystem(ElasticsearchTemplate elasticsearchTemplate) {
        Map<Long, EddbBody> result = new HashMap<>();

        BoolQueryBuilder qb = QueryBuilders.boolQuery();
        qb.must(QueryBuilders.termQuery("isMainStar", true));
        qb.must(QueryBuilders.existsQuery("starClass"));
        SearchQuery searchQuery = new NativeSearchQueryBuilder().withIndices("eddbbody").withQuery(qb).withPageable(new PageRequest(0, 1000)).build();
        String scrollId = elasticsearchTemplate.scan(searchQuery, 1000, false);
        boolean hasRecords = true;
        while (hasRecords) {
            Page<EddbBody> page = elasticsearchTemplate.scroll(scrollId, 5000, EddbBody.class);
            if (page.hasContent()) {
                for (EddbBody body : page.getContent()) {
                    result.put(body.getSystemId(), body);
                }
            } else {
                hasRecords = false;
            }
        }
        elasticsearchTemplate.clearScroll(scrollId);

        return result;
    }

}
