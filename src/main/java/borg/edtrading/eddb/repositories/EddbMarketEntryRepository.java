package borg.edtrading.eddb.repositories;

import borg.edtrading.eddb.data.EddbMarketEntry;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * EddbMarketEntryRepository
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbMarketEntryRepository extends ElasticsearchRepository<EddbMarketEntry, Long> {

    Page<EddbMarketEntry> findByCommodityId(Long commodityId, Pageable pageable);

}
