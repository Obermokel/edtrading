package borg.edtrading.eddb.repositories;

import borg.edtrading.eddb.data.EddbSystem;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

/**
 * EddbSystemRepository
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public interface EddbSystemRepository extends ElasticsearchRepository<EddbSystem, Long> {

    Page<EddbSystem> findByName(String name, Pageable pageable);

    Page<EddbSystem> findByAllegiance(String allegiance, Pageable pageable);

    Page<EddbSystem> findByCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(float xfrom, float xto, float yfrom, float yto, float zfrom, float zto, Pageable pageable);

    //    @Query("{\"bool\": {\"must\": [{\"match\": {\"name\": \"?0\"}}]}}")
    //    Page<EddbSystem> findBySystemNameUsingCustomQuery(String name, Pageable pageable);

}
