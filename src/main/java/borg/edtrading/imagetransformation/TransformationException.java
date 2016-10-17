package borg.edtrading.imagetransformation;

/**
 * TransformationException
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class TransformationException extends RuntimeException {

    private static final long serialVersionUID = -2909823858068554738L;

    public TransformationException() {
        super();
    }

    public TransformationException(String message) {
        super(message);
    }

    public TransformationException(Throwable cause) {
        super(cause);
    }

    public TransformationException(String message, Throwable cause) {
        super(message, cause);
    }

}
