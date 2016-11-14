package borg.edtrading.journal;

/**
 * UnknownEventException
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class UnknownEventException extends Exception {

    private static final long serialVersionUID = -6385121247330336732L;

    private final String event;

    public UnknownEventException(String event) {
        this.event = event;
    }

    public String getEvent() {
        return this.event;
    }

}
