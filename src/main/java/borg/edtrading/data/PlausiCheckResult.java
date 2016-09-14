package borg.edtrading.data;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * PlausiCheckResult
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class PlausiCheckResult {

    private final String testedSubject;
    private final Map<String, String> fixedFields = new LinkedHashMap<>();
    private final Map<String, String> deletedFields = new LinkedHashMap<>();
    private final Map<String, String> errorFields = new LinkedHashMap<>();

    public PlausiCheckResult(String testedSubject) {
        this.testedSubject = testedSubject;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("PlausiCheckResult for " + this.getTestedSubject() + ":\n");
        for (String field : this.getFixedFields().keySet()) {
            String message = this.getFixedFields().get(field);
            sb.append("FIXED: ").append(field).append(": ").append(message).append("\n");
        }
        for (String field : this.getDeletedFields().keySet()) {
            String message = this.getDeletedFields().get(field);
            sb.append("DELETED: ").append(field).append(": ").append(message).append("\n");
        }
        for (String field : this.getErrorFields().keySet()) {
            String message = this.getErrorFields().get(field);
            sb.append("ERROR: ").append(field).append(": ").append(message).append("\n");
        }
        sb.append("TOTAL: ").append(this.getFixedFields().size()).append(" fixed, ").append(this.getDeletedFields().size()).append(" deleted, ").append(this.getErrorFields().size()).append(" error(s) left");
        return sb.toString();
    }

    public boolean hasErrors() {
        return !this.getErrorFields().isEmpty();
    }

    public PlausiCheckResult addFixedField(String field, String message) {
        this.getFixedFields().put(field, message);
        return this;
    }

    public PlausiCheckResult addDeletedField(String field, String message) {
        this.getDeletedFields().put(field, message);
        return this;
    }

    public PlausiCheckResult addErrorField(String field, String message) {
        this.getErrorFields().put(field, message);
        return this;
    }

    public String getTestedSubject() {
        return this.testedSubject;
    }

    public Map<String, String> getFixedFields() {
        return this.fixedFields;
    }

    public Map<String, String> getDeletedFields() {
        return this.deletedFields;
    }

    public Map<String, String> getErrorFields() {
        return this.errorFields;
    }

}
