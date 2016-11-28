package borg.edtrading.gui;

import borg.edtrading.data.Coord;
import borg.edtrading.eddb.data.EddbBody;
import borg.edtrading.services.EddbService;
import com.sun.j3d.utils.applet.MainFrame;
import com.sun.j3d.utils.geometry.Sphere;
import com.sun.j3d.utils.geometry.Text2D;
import com.sun.j3d.utils.universe.SimpleUniverse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.applet.Applet;
import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.GraphicsConfiguration;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.media.j3d.Appearance;
import javax.media.j3d.BranchGroup;
import javax.media.j3d.Canvas3D;
import javax.media.j3d.Material;
import javax.media.j3d.Transform3D;
import javax.media.j3d.TransformGroup;
import javax.vecmath.Color3f;
import javax.vecmath.Vector3f;

/**
 * Universe
 *
 * @author <a href="mailto:b.guenther@xsite.de">Boris Guenther</a>
 */
@SuppressWarnings("restriction")
public class Universe extends Applet implements KeyListener, ActionListener {

    private static final long serialVersionUID = 5085938261454823722L;

    static final Logger logger = LogManager.getLogger(Universe.class);

    private final EddbService eddbService;
    private float xloc = 0.0f;
    private float yloc = 0.0f;
    private TransformGroup objTrans;
    private Transform3D trans = new Transform3D();

    public Universe(EddbService eddbService) {
        this.eddbService = eddbService;

        this.setLayout(new BorderLayout());

        GraphicsConfiguration config = SimpleUniverse.getPreferredConfiguration();
        Canvas3D c = new Canvas3D(config);
        add("Center", c);
        c.addKeyListener(this);

        //timer = new Timer(100, this);
        //timer.start();

        //        Panel p = new Panel();
        //        p.add(go);
        //        add("North", p);
        //        go.addActionListener(this);
        //        go.addKeyListener(this);

        // Create a simple scene and attach it to the virtual universe

        BranchGroup scene = createSceneGraph();
        SimpleUniverse u = new SimpleUniverse(c);
        u.getViewingPlatform().setNominalViewingTransform();
        u.addBranchGraph(scene);
    }

    public static void main(String[] args) throws IOException {
        Universe universe = new Universe(null);
        universe.addKeyListener(universe);
        MainFrame mf = new MainFrame(universe, 256, 256);
    }

    public BranchGroup createSceneGraph() {
        BranchGroup objRoot = new BranchGroup();
        objTrans = new TransformGroup();
        objTrans.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        objRoot.addChild(objTrans);

        List<EddbBody> bodiesHavingSpectralClass = new ArrayList<>(); // TODO
        for (EddbBody star : bodiesHavingSpectralClass) {
            //float radius = star.getSolar_radius() == null ? 1f : star.getSolar_radius().floatValue();
            float radius = 0.05f;
            Coord coord = star.getSystem().getCoord();
            Color3f color = spectralClassToEmissiveColor(star.getSpectralClass());

            TransformGroup tg = new TransformGroup();
            Transform3D transform = new Transform3D();
            transform.setTranslation(new Vector3f((float) coord.getX(), (float) coord.getY(), (float) coord.getZ()));
            tg.setTransform(transform);
            Appearance ap = new Appearance();
            Material mat = new Material();
            mat.setEmissiveColor(color);
            ap.setMaterial(mat);
            Sphere sphere = new Sphere(radius);
            sphere.setAppearance(ap);
            tg.addChild(sphere);
            tg.addChild(new Text2D(star.getName(), color, "Arial", 12, Font.PLAIN));

            objRoot.addChild(tg);
        }

        return objRoot;
    }

    @Override
    public void keyTyped(KeyEvent e) {
        // TODO Auto-generated method stub
    }

    @Override
    public void keyPressed(KeyEvent e) {
        if (e.getKeyChar() == 'a') {
            System.out.println("left");
            xloc = xloc - 1.1f;
        }
        if (e.getKeyChar() == 'd') {
            System.out.println("right");
            xloc = xloc + 1.1f;
        }
        if (e.getKeyChar() == 'w') {
            System.out.println("up");
            yloc = yloc + 1.1f;
        }
        if (e.getKeyChar() == 's') {
            System.out.println("down");
            yloc = yloc - 1.1f;
        }

    }

    @Override
    public void keyReleased(KeyEvent e) {
        // TODO Auto-generated method stub
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        System.out.println("action!");
        trans.setTranslation(new Vector3f(xloc, yloc, 0.0f));
        objTrans.setTransform(trans);
    }

    private static Color3f spectralClassToEmissiveColor(String spectral_class) {
        if ("O".equals(spectral_class)) {
            return new Color3f(0.9f, 0.9f, 1.0f);
        } else if ("B".equals(spectral_class)) {
            return new Color3f(0.8f, 0.8f, 1.0f);
        } else if ("A".equals(spectral_class)) {
            return new Color3f(0.8f, 0.8f, 0.8f);
        } else if ("F".equals(spectral_class)) {
            return new Color3f(0.8f, 0.8f, 0.6f);
        } else if ("G".equals(spectral_class)) {
            return new Color3f(0.8f, 0.8f, 0.4f);
        } else if ("K".equals(spectral_class)) {
            return new Color3f(0.7f, 0.7f, 0.3f);
        } else if ("M".equals(spectral_class)) {
            return new Color3f(0.6f, 0.6f, 0.2f);
        } else {
            return new Color3f(0.3f, 0.3f, 0.0f);
        }
    }

}
