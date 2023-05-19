package Java;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

import jess.JessException;
import jess.QueryResult;
import jess.ValueVector;

public class Gui extends JFrame {

	JTable table = new JTable();
	JTable fTable = new JTable();
	JButton btnClose = new JButton("Close");

	Vector<Vector<Object>> data = new Vector<Vector<Object>>();
	Vector<String> columns = new Vector<String>();

	Vector<Vector<Object>> fData = new Vector<Vector<Object>>();
	Vector<String> fColumns = new Vector<String>();

	JPanel content_panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 30, 30));
	JScrollPane pane = new JScrollPane(table);
	JScrollPane fPane = new JScrollPane(fTable);

	
	String[] fire_column_strings = { "No.", "Name", "Damage", "Defense",
			"Level", "Burn Damage", "Price" };
	String[] water_column_strings = { "No.", "Name", "Damage", "Defense",
			"Level", "Price" };

	final Gui form = this;

	public void init() {
		getContentPane().setLayout(new BorderLayout());

		JPanel left_panel = new JPanel(new GridLayout(0, 3));
		
		JPanel pLbl = new JPanel();
		JLabel lblInfo = new JLabel("Flokemon Demand");
		pLbl.add(lblInfo);
		left_panel.add(new JLabel());
		left_panel.add(lblInfo);
		left_panel.add(new JLabel());
		
		JLabel lblSkill = new JLabel("Power");
		JLabel lblExp = new JLabel("Defense");
		JLabel lblAct = new JLabel("Minimum Level");
		JLabel lblAge = new JLabel("Budget");

		Object panel_add = null;

		try {
			QueryResult info = Main.engine.runQueryStar("retrieve-info",
					new ValueVector());
			if (info.next()) {
				left_panel.add(lblSkill);
				left_panel.add(new JLabel("  :  "));
				left_panel.add(new JLabel(info.getString("power")));
				left_panel.add(lblExp);
				left_panel.add(new JLabel("  :  "));
				left_panel.add(new JLabel(info.getString("defense")));
				left_panel.add(lblAct);
				left_panel.add(new JLabel("  :  "));
				left_panel.add(new JLabel(info.getString("size")));
				left_panel.add(lblAge);
				left_panel.add(new JLabel("  :  "));
				left_panel.add(new JLabel(info.getString("price")));

				QueryResult all_artist = Main.engine.runQueryStar(
						"flokemon-found", new ValueVector());
				int count = 0, fcount = 0;
				columns.clear();
				fColumns.clear();
				data.clear();
				for (String s : fire_column_strings) {
					columns.add(s);
				}
				for (String s : water_column_strings) {
					fColumns.add(s);
				}
				while (all_artist.next()) {

					Vector<Object> row = new Vector<Object>();
					if (info.getString("type").equals("Water")) {
						row.add(++fcount);
						row.add(all_artist.getString("name"));
						row.add(all_artist.getInt("damage"));
						row.add(all_artist.getInt("defense"));
						row.add(all_artist.getInt("level"));
						row.add(all_artist.getInt("price"));
						fData.add(row);
					} else {
						row.add(++count);
						row.add(all_artist.getString("name"));
						row.add(all_artist.getInt("damage"));
						row.add(all_artist.getInt("defense"));
						row.add(all_artist.getInt("level"));
						row.add(all_artist.getInt("burn_damage"));
						row.add(all_artist.getInt("price"));
						data.add(row);
					}

				}

				if (count + fcount <= 0) {
					JLabel lbl_img = new JLabel();
					lbl_img.setPreferredSize(new Dimension(320, 180));
					try {
						System.out.println();
						ImageIcon icon = new ImageIcon(new ImageIcon(System.getProperty("user.dir")+"\\src\\java\\not_available.jpg").getImage().getScaledInstance(320, 180, Image.SCALE_DEFAULT));
						lbl_img.setIcon(icon);
						panel_add = lbl_img;
					} catch (Exception e) {
						e.printStackTrace();
					}
				} 
					else {

					JPanel panel = new JPanel(new GridLayout(0, 1));
					if (!data.isEmpty()) {

						JPanel mainP = new JPanel(new BorderLayout());

						JPanel p = new JPanel();
						p.add(new JLabel("Fire Flokemon"));

						mainP.add(p, BorderLayout.NORTH);
						mainP.add(pane, BorderLayout.CENTER);

						panel.add(mainP);
					}
					if (!fData.isEmpty()) {
						JPanel mainP = new JPanel(new BorderLayout());
						JPanel p = new JPanel();
						p.add(new JLabel("Water Flokemon"));

						mainP.add(p, BorderLayout.NORTH);
						mainP.add(fPane, BorderLayout.CENTER);

						panel.add(mainP);
					}

					pane.setPreferredSize(new Dimension(400, 100));
					// table.setFillsViewportHeight(true);
					table.setModel(new DefaultTableModel(data, columns));

					fPane.setPreferredSize(new Dimension(400, 100));
					// fTable.setFillsViewportHeight(true);
					fTable.setModel(new DefaultTableModel(fData, fColumns));

					panel_add = panel;
				}

				all_artist.close();
			}
			info.close();
		} catch (JessException e1) {
			e1.printStackTrace();
		}

		content_panel.add(left_panel);
		content_panel.add((Component) panel_add);
		content_panel.setPreferredSize(new Dimension(600, 250));

		getContentPane().add(content_panel, BorderLayout.CENTER);
		getContentPane().add(btnClose, BorderLayout.PAGE_END);

		btnClose.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				// TODO Auto-generated method stub
				form.dispose();
			}
		});
	}

	private Image getScaledImage(Image srcImage, int width, int height) {
		BufferedImage resizedImage = new BufferedImage(width, height,
				BufferedImage.TYPE_INT_ARGB);
		Graphics2D g2d = resizedImage.createGraphics();

		g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
				RenderingHints.VALUE_INTERPOLATION_BICUBIC);
		g2d.drawImage(srcImage, 0, 0, width, height, null);
		g2d.dispose();

		return resizedImage;
	}

	public Gui() {
		// TODO Auto-generated constructor stub
		setTitle("Flokemon Store");
		setSize(500, 500);
		setLocationRelativeTo(null);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		setResizable(false);

		init();
		setVisible(true);
	}
}
