package borg.edtrading.service;

import java.io.Closeable;
import java.io.IOException;

import borg.edtrading.data.Commodity;
import borg.edtrading.data.TradingData;
import borg.edtrading.eddb.data.StarSystem;
import borg.edtrading.eddb.data.Station;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.action.admin.indices.create.CreateIndexRequestBuilder;
import org.elasticsearch.action.admin.indices.create.CreateIndexResponse;
import org.elasticsearch.action.admin.indices.exists.indices.IndicesExistsRequestBuilder;
import org.elasticsearch.action.admin.indices.exists.indices.IndicesExistsResponse;
import org.elasticsearch.action.index.IndexRequestBuilder;
import org.elasticsearch.action.index.IndexResponse;
import org.elasticsearch.action.search.SearchRequestBuilder;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.transport.TransportClient;
import org.elasticsearch.common.settings.ImmutableSettings;
import org.elasticsearch.common.settings.ImmutableSettings.Builder;
import org.elasticsearch.common.settings.Settings;
import org.elasticsearch.common.transport.InetSocketTransportAddress;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;

/**
 * TradingDao
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TradingDao implements Closeable {

    private static final String ES_HOST = "127.0.0.1";
    private static final int ES_PORT = 9300;
    private static final String ES_CLUSTER = "neuss";
    private static final String ES_INDEX = "edtrading";

    static final Logger logger = LogManager.getLogger(TradingDao.class);

    private static final TradingDao INSTANCE = new TradingDao();

    private final TransportClient client;

    @SuppressWarnings("resource")
    private TradingDao() {
        Builder settingsBuilder = ImmutableSettings.settingsBuilder();
        if (ES_CLUSTER != null) {
            settingsBuilder.put("cluster.name", ES_CLUSTER);
        }
        Settings settings = settingsBuilder.build();
        InetSocketTransportAddress addr = new InetSocketTransportAddress(ES_HOST, ES_PORT);
        client = new TransportClient(settings).addTransportAddress(addr);
        this.ensureIndices();
    }

    @Override
    protected void finalize() throws Throwable {
        this.close();
    }

    public static TradingDao getInstance() {
        return INSTANCE;
    }

    @Override
    public void close() throws IOException {
        client.close();
    }

    private void ensureIndices() {
        // FIXME Do not delete
        client.admin().indices().prepareDelete(ES_INDEX).execute().actionGet();

        // Create index if not exists
        IndicesExistsRequestBuilder ierb = client.admin().indices().prepareExists(ES_INDEX);
        IndicesExistsResponse ier = ierb.execute().actionGet();
        if (!ier.isExists()) {
            CreateIndexRequestBuilder cirb = client.admin().indices().prepareCreate(ES_INDEX);
            CreateIndexResponse cir = cirb.execute().actionGet();
            if (!cir.isAcknowledged()) {
                throw new RuntimeException("Failed to create index " + ES_INDEX);
            }
        }

        // Set settings
        Builder settingsBuilder = ImmutableSettings.settingsBuilder();
        settingsBuilder.put("index.mapper.dynamic", false);
        settingsBuilder.put("query.parse.allow_unmapped_fields", false);
        settingsBuilder.put("index.analysis.analyzer.lowercaseKeyword.tokenizer", "keyword");
        settingsBuilder.put("index.analysis.analyzer.lowercaseKeyword.filter", "lowercase");
        Settings settings = settingsBuilder.build();

        client.admin().indices().prepareClose(ES_INDEX).execute().actionGet();
        client.admin().indices().prepareUpdateSettings(ES_INDEX).setSettings(settings).execute().actionGet();
        client.admin().indices().prepareOpen(ES_INDEX).execute().actionGet();

        // Put mappings
        client.admin().indices()
        .preparePutMapping(ES_INDEX).setType(Commodity.ES_TYPE)
        .setSource(Commodity.createElasticSearchMapping()).setIgnoreConflicts(false)
        .execute().actionGet();
        client.admin().indices()
        .preparePutMapping(ES_INDEX).setType(StarSystem.ES_TYPE)
        .setSource(StarSystem.createElasticSearchMapping()).setIgnoreConflicts(false)
        .execute().actionGet();
        client.admin().indices()
        .preparePutMapping(ES_INDEX).setType(Station.ES_TYPE)
        .setSource(Station.createElasticSearchMapping()).setIgnoreConflicts(false)
        .execute().actionGet();
        client.admin().indices()
        .preparePutMapping(ES_INDEX).setType(TradingData.ES_TYPE)
        .setSource(TradingData.createElasticSearchMapping()).setIgnoreConflicts(false)
        .execute().actionGet();
    }

    public String saveCargo(Commodity commodity) {
        IndexRequestBuilder irb = client.prepareIndex(ES_INDEX, Commodity.ES_TYPE);
        irb.setId(commodity.getElasticSearchId());
        irb.setSource(commodity.toElasticSearchSource());
        IndexResponse ir = irb.execute().actionGet();

        return ir.getId();
    }

    public String saveStarSystem(StarSystem starSystem) {
        IndexRequestBuilder irb = client.prepareIndex(ES_INDEX, StarSystem.ES_TYPE);
        irb.setId(starSystem.getElasticSearchId());
        irb.setSource(starSystem.toElasticSearchSource());
        IndexResponse ir = irb.execute().actionGet();

        return ir.getId();
    }

    public String saveStation(Station station) {
        IndexRequestBuilder irb = client.prepareIndex(ES_INDEX, Station.ES_TYPE);
        irb.setId(station.getElasticSearchId());
        irb.setSource(station.toElasticSearchSource());
        IndexResponse ir = irb.execute().actionGet();

        return ir.getId();
    }

    public String saveTradingData(TradingData tradingData) {
        IndexRequestBuilder irb = client.prepareIndex(ES_INDEX, TradingData.ES_TYPE);
        irb.setId(tradingData.getElasticSearchId());
        irb.setSource(tradingData.toElasticSearchSource());
        IndexResponse ir = irb.execute().actionGet();

        return ir.getId();
    }

    public Commodity loadCargoByName(String name) {
        SearchRequestBuilder srb = client.prepareSearch(ES_INDEX);
        QueryBuilder qb = QueryBuilders.matchQuery("name", name);
        srb.setTypes(Commodity.ES_TYPE).setQuery(qb);
        SearchResponse sr = srb.execute().actionGet();

        if (sr.getHits().getTotalHits() <= 0) {
            logger.debug("Cargo '" + name + "' not found");
            return null;
        } else if (sr.getHits().getTotalHits() >= 2) {
            throw new RuntimeException("Found more than one cargo with name '" + name + "'");
        } else {
            return Commodity.fromElasticSearchSource(sr.getHits().getHits()[0].getSource());
        }
    }

}
