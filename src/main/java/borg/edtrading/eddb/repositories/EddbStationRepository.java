package borg.edtrading.eddb.repositories;

import borg.edtrading.eddb.data.EddbStation;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * EddbStationRepository
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbStationRepository extends ElasticsearchRepository<EddbStation, Long> {

    Page<EddbStation> findByName(String name, Pageable pageable);

}
