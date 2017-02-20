package borg.edtrading.gui;

import borg.edtrading.data.Coord;
import borg.edtrading.util.StarUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

/**
 * MapCreator
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
public class MapCreator {

    static final Logger logger = LogManager.getLogger(MapCreator.class);

    private final float xfrom;
    private final float xto;
    private final float xsize;
    private final float yfrom;
    private final float yto;
    private final float ysize;
    private final float zfrom;
    private final float zto;
    private final float zsize;

    private final MapView view;

    private final int width;
    private final int height;

    private BufferedImage i = null;
    private Graphics2D g = null;

    public MapCreator(float xfrom, float xto, float yfrom, float yto, float zfrom, float zto, MapView view, int maxImageSize) {
        if (xto <= xfrom) {
            throw new IllegalArgumentException("xto <= xfrom");
        } else if (yto <= yfrom) {
            throw new IllegalArgumentException("yto <= yfrom");
        } else if (zto <= zfrom) {
            throw new IllegalArgumentException("zto <= zfrom");
        } else if (view == null) {
            throw new IllegalArgumentException("view == null");
        } else {
            this.xfrom = xfrom;
            this.xto = xto;
            this.xsize = xto - xfrom;
            this.yfrom = yfrom;
            this.yto = yto;
            this.ysize = yto - yfrom;
            this.zfrom = zfrom;
            this.zto = zto;
            this.zsize = zto - zfrom;

            this.view = view;

            if (MapView.TOP.equals(this.view)) {
                if (this.xsize >= this.zsize) {
                    this.width = maxImageSize;
                    this.height = Math.round((this.zsize / this.xsize) * maxImageSize);
                } else {
                    this.width = Math.round((this.xsize / this.zsize) * maxImageSize);
                    this.height = maxImageSize;
                }
            } else if (MapView.LEFT.equals(this.view)) {
                if (this.zsize >= this.ysize) {
                    this.width = maxImageSize;
                    this.height = Math.round((this.ysize / this.zsize) * maxImageSize);
                } else {
                    this.width = Math.round((this.zsize / this.ysize) * maxImageSize);
                    this.height = maxImageSize;
                }
            } else if (MapView.FRONT.equals(this.view)) {
                if (this.xsize >= this.ysize) {
                    this.width = maxImageSize;
                    this.height = Math.round((this.ysize / this.xsize) * maxImageSize);
                } else {
                    this.width = Math.round((this.xsize / this.ysize) * maxImageSize);
                    this.height = maxImageSize;
                }
            } else {
                throw new IllegalArgumentException("Unknown view " + view);
            }
        }
    }

    public void prepare() {
        this.i = new BufferedImage(this.width, this.height, BufferedImage.TYPE_INT_ARGB);

        this.prepare(this.i.createGraphics());
    }

    public void prepare(Graphics2D g) {
        this.g = g;

        // Black background
        g.setColor(new Color(20, 20, 25));
        g.fillRect(0, 0, this.getWidth(), this.getHeight());

        // Turn on AA
        this.g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    }

    public BufferedImage finish() {
        this.g.dispose();

        return this.i;
    }

    public void drawString(Coord coord, String text, Color color, Font font) {
        if (coord != null && StringUtils.isNotEmpty(text) && color != null && font != null) {
            Point p = this.coordToPoint(coord);
            if (p.x >= 0 && p.x < this.width && p.y >= 0 && p.y < this.height) {
                this.g.setColor(color);
                this.g.setFont(font);
                this.g.drawString(text, p.x, p.y + font.getSize() / 2);
            }
        }
    }

    public void drawStar(Coord coord, String starClass, String name) {
        this.drawStar(coord, starClass, name, 3, 128, 127);
    }

    public void drawStar(Coord coord, String starClass, String name, int psize, int alphaRange, int minAlpha) {
        if (coord != null) {
            Point p = this.coordToPoint(coord);
            if (p.x >= 0 && p.x < this.width && p.y >= 0 && p.y < this.height) {
                int a = this.coordToAlpha(coord, alphaRange, minAlpha);
                Color c = StarUtil.spectralClassToColor(starClass);

                this.g.setColor(new Color(c.getRed(), c.getGreen(), c.getBlue(), a));
                this.g.fillOval(p.x - (psize - 1) / 2, p.y - (psize - 1) / 2, psize, psize);

                if (StringUtils.isNotEmpty(name)) {
                    this.g.setColor(Color.WHITE);
                    this.g.setFont(new Font("Sans Serif", Font.PLAIN, 12));
                    this.g.drawString(name, p.x + psize, p.y + (psize - 1) / 2);
                }
            }
        }
    }

    public void drawPath(Coord from, Coord to, Color c) {
        if (from != null && to != null) {
            Point pfrom = this.coordToPoint(from);
            Point pto = this.coordToPoint(to);

            this.g.setColor(c == null ? Color.ORANGE : c);
            this.g.setStroke(new BasicStroke(2));
            this.g.drawLine(pfrom.x, pfrom.y, pto.x, pto.y);
        }
    }

    public int getWidth() {
        return this.width;
    }

    public int getHeight() {
        return this.height;
    }

    public Coord pointToCoord(Point p) {
        float xPercent = (float) p.x / (float) this.width;
        float yPercent = (float) p.y / (float) this.height;

        if (MapView.TOP.equals(this.view)) {
            return new Coord(this.xfrom + xPercent * this.xsize, 0f, this.zto - yPercent * this.zsize);
        } else if (MapView.LEFT.equals(this.view)) {
            return new Coord(0f, this.yto - yPercent * this.ysize, this.zto - xPercent * this.zsize);
        } else if (MapView.FRONT.equals(this.view)) {
            return new Coord(this.xfrom + xPercent * this.xsize, this.yto - yPercent * this.ysize, 0f);
        } else {
            return null;
        }
    }

    private Point coordToPoint(Coord coord) {
        float xPercent = (coord.getX() - this.xfrom) / this.xsize;
        float yPercent = 1.0f - ((coord.getY() - this.yfrom) / this.ysize);
        float zPercent = 1.0f - ((coord.getZ() - this.zfrom) / this.zsize);

        if (MapView.TOP.equals(this.view)) {
            return new Point(Math.round(xPercent * this.getWidth()), Math.round(zPercent * this.getHeight()));
        } else if (MapView.LEFT.equals(this.view)) {
            return new Point(Math.round(zPercent * this.getWidth()), Math.round(yPercent * this.getHeight()));
        } else if (MapView.FRONT.equals(this.view)) {
            return new Point(Math.round(xPercent * this.getWidth()), Math.round(yPercent * this.getHeight()));
        } else {
            return null;
        }
    }

    /**
     * @param coord
     * @param alphaRange
     *      How much alpha can vary. 255 for full range, 128 for half etc.
     * @param minAlpha
     *      Min alpha. 0 means transparent, 128 means half solid/transparent. minAlpha+alphaRange must be <= 255.
     * @return
     */
    private int coordToAlpha(Coord coord, int alphaRange, int minAlpha) {
        // 255=solid
        // 0=transparent
        if (minAlpha + alphaRange > 255) {
            throw new IllegalArgumentException("minAlpha(" + minAlpha + ") + alphaRange(" + alphaRange + ") > 255");
        } else if (alphaRange < 0 || alphaRange > 255) {
            throw new IllegalArgumentException("alphaRange(" + alphaRange + ") must be between 0 and 255");
        } else if (minAlpha < 0 || minAlpha > 255) {
            throw new IllegalArgumentException("minAlpha(" + minAlpha + ") must be between 0 and 255");
        } else {
            if (MapView.TOP.equals(this.view)) {
                float dy = Math.abs(coord.getY() - (this.yfrom + (this.ysize / 2f)));
                return minAlpha - Math.round((dy / (this.ysize / 2f)) * alphaRange);
            } else if (MapView.LEFT.equals(this.view)) {
                float dx = Math.abs(coord.getX() - (this.xfrom + (this.xsize / 2f)));
                return minAlpha - Math.round((dx / (this.xsize / 2f)) * alphaRange);
            } else if (MapView.FRONT.equals(this.view)) {
                float dz = Math.abs(coord.getZ() - (this.zfrom + (this.zsize / 2f)));
                return minAlpha - Math.round((dz / (this.zsize / 2f)) * alphaRange);
            } else {
                return 0;
            }
        }
    }

    public static enum MapView {

        /**
         * image x = galaxy x
         * <br>
         * image y = galaxy z
         */
        TOP,

        /**
         * image x = galaxy z
         * <br>
         * image y = galaxy y
         */
        LEFT,

        /**
         * image x = galaxy x
         * <br>
         * image y = galaxy y
         */
        FRONT;

    }

}
