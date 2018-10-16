package leveleditor;

import java.awt.EventQueue;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JTable;
import javax.swing.border.BevelBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.JPanel;
import javax.imageio.ImageIO;
import javax.swing.JButton;
import java.awt.Canvas;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import javax.swing.JTextPane;

public class Main {

	private JFrame frame;
	private JTable table;
	private static Object[] columnNames = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"};
	private static ImageIcon[][] data = new ImageIcon[9][13];
	private static ImageIcon[] tiles = new ImageIcon[28]; 
	private static int akt = 0;
	
	private static void loadTiles() {
		URL tilesPic = Main.class.getResource( "TILES.png" );
		try {
			BufferedImage image = ImageIO.read(tilesPic);
			for (int i = 0; i < 28; i++) {
				int x = i % 8 * 24;
				int y = i / 8 * 24;			
				tiles[i] = new ImageIcon(image.getSubimage(x, y, 24, 24));
				tiles[i].setDescription(new String().valueOf(i));
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		
		System.out.println(System.getProperty("java.class.path"));	
		loadTiles();
		for (int i = 0; i < 13; i++) {
			for (int j = 0; j < 9; j++) {
				data[j][i] = tiles[1];
			}
		}
		
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					Main window = new Main();
					window.frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public Main() {
		initialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		
		frame = new JFrame();
		frame.setBounds(100, 100, 600, 600);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().setLayout(null);
		
        final DefaultTableModel model = new DefaultTableModel(data, columnNames)
        {
            public Class getColumnClass(int column)
            {
            	if (getValueAt(0, column) == null)
            		return "X".getClass();
                return getValueAt(0, column).getClass();
            }
        };
		
		table = new JTable(model);
		table.addMouseMotionListener(new MouseMotionAdapter() {
			@Override
			public void mouseDragged(MouseEvent arg0) {
				int x = arg0.getX() / 25;
				int y = arg0.getY() / 25;
				model.setValueAt(tiles[akt], y, x);
			}
		});
		table.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent arg0) {
				int x = arg0.getX() / 25;
				int y = arg0.getY() / 25;
				model.setValueAt(tiles[akt], y, x);
			}
		});
		
		table.setSize(325, 225);
		table.setRowHeight(25);
		table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		for (int i = 0; i < table.getColumnCount(); i++) {
			table.getColumnModel().getColumn(i).setPreferredWidth(24);
		}
		frame.getContentPane().add(table);
		
		TilesPanel t = new TilesPanel();
		t.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent arg0) {
				akt = arg0.getX() / 24 + (arg0.getY() / 24 * 8);
				System.out.println(akt);
			}
		});
		t.setBounds(335, 0, 192, 98);
		frame.getContentPane().add(t);
		
		final JTextPane textPane = new JTextPane();
		textPane.setBounds(0, 236, 325, 260);
		frame.getContentPane().add(textPane);
		
		JButton btnExport = new JButton("Export");
		btnExport.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent arg0) {
				int tileAkt = 0, tileNxt = 0, zaehler = 0;
				
				StringBuilder sb = new StringBuilder(1000);
				int r = table.getRowCount();
				int c = table.getColumnCount();
				for (int x = 0; x < r; x++) {
					sb.append("!byte ");
					zaehler = 1;
					for (int y = 0; y < c; y++) {
						tileAkt = Integer.valueOf(((ImageIcon)model.getValueAt(x, y)).getDescription()) + 128;
						if (y < c - 1) 
							tileNxt = Integer.valueOf(((ImageIcon)model.getValueAt(x, y + 1)).getDescription()) + 128;
						else 
							tileNxt = 999;
						if (tileAkt != tileNxt) {
							if (zaehler == 1) {
								sb.append("$" + Integer.toHexString(tileAkt) + ", ");
							} else {
								sb.append(zaehler + ", $" + Integer.toHexString(tileAkt) + ", ");
								zaehler = 1;
							}
						} else {
							zaehler++;
						}
						if (y == c-1) 
							sb.append("$01");
					}
					sb.append("\n");
				}
				sb.append("!byte $ff");
				textPane.setText(sb.toString());
			}
		});
		btnExport.setBounds(434, 202, 89, 23);
		frame.getContentPane().add(btnExport);
		
		JButton btnImport = new JButton("Import");
		btnImport.setBounds(335, 202, 89, 23);
		frame.getContentPane().add(btnImport);
	}
}
