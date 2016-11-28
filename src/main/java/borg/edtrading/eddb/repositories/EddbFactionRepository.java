package borg.edtrading.eddb.repositories;

import borg.edtrading.eddb.data.EddbFaction;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * EddbFactionRepository
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbFactionRepository extends ElasticsearchRepository<EddbFaction, Long> {

    Page<EddbFaction> findByName(String name, Pageable pageable);

}
