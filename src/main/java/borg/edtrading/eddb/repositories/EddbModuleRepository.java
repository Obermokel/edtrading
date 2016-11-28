package borg.edtrading.eddb.repositories;

import borg.edtrading.eddb.data.EddbModule;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * EddbModuleRepository
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbModuleRepository extends ElasticsearchRepository<EddbModule, Long> {

    Page<EddbModule> findByName(String name, Pageable pageable);

}
