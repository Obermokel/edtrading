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

    Page<EddbBody> findBySystemId(Long systemId, Pageable pageable);

    Page<EddbBody> findByTypeIdAndIsMainStarAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(Long typeId, Boolean isMainStar, float xfrom, float xto, float yfrom, float yto, float zfrom, float zto, Pageable pageable);

    Page<EddbBody> findByTypeIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(Long typeId, float xfrom, float xto, float yfrom, float yto, float zfrom, float zto, Pageable pageable);

    Page<EddbBody> findByTerraformingStateIdAndCoord_xBetweenAndCoord_yBetweenAndCoord_zBetween(Long terraformingStateId, float xfrom, float xto, float yfrom, float yto, float zfrom, float zto, Pageable pageable);

}
