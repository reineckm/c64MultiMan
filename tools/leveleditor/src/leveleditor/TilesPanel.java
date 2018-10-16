package leveleditor;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.JPanel;

public class TilesPanel extends JPanel {

	private BufferedImage image;

	public TilesPanel() {
		URL tilesPic = Main.class.getResource("TILES.png");

		try {
			image = ImageIO.read(tilesPic);
		} catch (IOException ex) {
			System.out.println(ex);
		}
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		g.drawImage(image, 0, 0, this); // see javadoc for more info on the
										// parameters
	}

}
