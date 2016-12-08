package borg.edtrading.cfg;

import borg.edtrading.eddb.reader.EddbReader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.elasticsearch.client.Client;
import org.elasticsearch.client.transport.TransportClient;
import org.elasticsearch.common.settings.Settings;
import org.elasticsearch.common.transport.InetSocketTransportAddress;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.data.elasticsearch.repository.config.EnableElasticsearchRepositories;

import java.net.InetSocketAddress;

/**
 * Config
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
@Configuration
@EnableElasticsearchRepositories(basePackages = "borg.edtrading.eddb.repositories")
@ComponentScan(basePackages = { "borg.edtrading.services.impl" })
public class Config {

    static final Logger logger = LogManager.getLogger(Config.class);

    @Bean
    public ElasticsearchOperations elasticsearchTemplate() {
        return new ElasticsearchTemplate(this.client());
    }

    @Bean
    public Client client() {
        TransportClient client = TransportClient.builder().settings(Settings.settingsBuilder().put("cluster.name", "eddbmirror")).build();
        client.addTransportAddress(new InetSocketTransportAddress(new InetSocketAddress("127.0.0.1", 9300)));
        return client;
    }

    @Bean
    public EddbReader eddbReader() {
        return new EddbReader();
    }

}
