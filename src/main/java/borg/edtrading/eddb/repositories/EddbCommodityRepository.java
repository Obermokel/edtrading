package borg.edtrading.eddb.repositories;

import borg.edtrading.eddb.data.EddbCommodity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * EddbCommodityRepository
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbCommodityRepository extends ElasticsearchRepository<EddbCommodity, Long> {

    Page<EddbCommodity> findByName(String name, Pageable pageable);

}
