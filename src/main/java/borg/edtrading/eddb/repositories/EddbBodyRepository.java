package borg.edtrading.eddb.repositories;

import borg.edtrading.eddb.data.EddbBody;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * EddbBodyRepository
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbBodyRepository extends ElasticsearchRepository<EddbBody, Long> {

    Page<EddbBody> findByName(String name, Pageable pageable);

    Page<EddbBody> findBySpectralClass(String spectralClass, Pageable pageable);

}
