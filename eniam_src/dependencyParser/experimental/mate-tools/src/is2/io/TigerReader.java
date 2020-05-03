/**
 *
 */
package is2.io;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.StringTokenizer;

import is2.data.PSTree;

/**
 * @author Dr. Bernd Bohnet, 17.01.2011
 *
 *         Reads a sentences in Penn Tree Bank bracket style and return
 *         sentences.
 */
public class TigerReader implements PSReader {

	BufferedReader inputReader;
	ArrayList<File> psFiles = new ArrayList<File>();
	ArrayList<PSTree> psCache = new ArrayList<PSTree>();

	String filter[] = null;
	int startFilter = -1;
	int endFilter = -1;

	public TigerReader() {
	}

	public TigerReader(String file) {

		try {
			inputReader = new BufferedReader(new InputStreamReader(new FileInputStream(file), "ISO-8859-1"), 32768);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * @param ps
	 */
	@Override
	public void startReading(String file, String[] filter) {

		try {
			this.filter = filter;
			startFilter = filter == null ? -1 : 1;
			endFilter = filter == null ? -1 : 1;

			inputReader = new BufferedReader(new InputStreamReader(new FileInputStream(file), "ISO-8859-1"), 32768);
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	public static class Line {
		String form;
		String lemma;
		String morph;
		String pos;
		int parent;
		String edge;

	}

	static int stop = 0;

	/**
	 * @return
	 */
	@Override
	public PSTree getNext() {

		PSTree ps = null;
		String l = null;
		ArrayList<Line> lines = new ArrayList<Line>();
		try {
			int state = 1, terminals = 0, nonterminals = 0;
			while ((l = inputReader.readLine()) != null) {

				if (startFilter == 1 && l.startsWith("#BOS " + filter[0])) {
					System.out.println("found start " + l);
					startFilter = 2;
				}
				if (endFilter == 1 && l.startsWith("#EOS " + filter[1])) {
					System.out.println("found end " + l);

					endFilter = 2;
				}

				if (startFilter == 1 || endFilter == 2)
					continue;

				if (l.startsWith("#BOS")) {

					state = 2;
					continue;
				}
				if (l.startsWith("#500"))
					state = 3;
				if (l.startsWith("#EOS"))
					state = 4;
				if (state < 2)
					continue;

				if (state == 4) {

					ps = new PSTree();
					ps.create(terminals, nonterminals);
					// System.out.println("terminals "+terminals);
					// build ps tree

					int cnt = 0;
					// ps.entries[0] =CONLLReader09.ROOT;
					// ps.head[0]=-1;
					int root = -1;
					for (Line line : lines) {

						/*
						 * if (cnt==terminals) { // insert root root =cnt;
						 * cnt++; }
						 */
						ps.entries[cnt] = line.form;
						if (cnt < terminals)
							ps.pos[cnt] = line.pos;
						else
							ps.entries[cnt] = line.pos;
						ps.lemmas[cnt] = line.lemma;
						ps.head[cnt] = line.parent == 0 ? lines.size() - 1
								: line.parent >= 500 ? line.parent - 500 + terminals : line.parent;
						// ps.head[cnt] =
						// line.parent==0?lines.size()-1:line.parent>=500?line.parent-500+terminals:line.parent;
						ps.morph[cnt] = line.morph;
						cnt++;

					}

					if (root == -1)
						root = terminals;
					ps.head[cnt - 1] = 0; // root
					ps.terminalCount = terminals;
					lines.clear();
					state = 1;

					/*
					 * for(int k=0;k<ps.head.length;k++) { if
					 * (ps.head[k]<terminals && k!=root) { ps.head[k]=root; //
					 * DB.println("error "+k+" "+ps.head[k]); } }
					 */
					// System.out.println(""+ps.toString());
					// if (stop++ == 4)System.exit(0);
					return ps;
				}

				StringTokenizer t = new StringTokenizer(l, "\t");
				int tc = 0;
				Line line = new Line();
				lines.add(line);
				while (t.hasMoreTokens()) {
					String token = t.nextToken();
					if (token.equals("\t"))
						continue;
					if (tc == 0) {
						if (token.startsWith("#5") || token.startsWith("#6")) {
							nonterminals++;

						} else {
							terminals++;

							// change it back to the wrong format since the
							// conll stuff was derived from this.
							// if (token.equals("durchblicken"))
							// token="durchblikken";
							line.form = token;
						}

					} else if (tc == 1) {
						line.lemma = token;
					} else if (tc == 2) {
						line.pos = token;
					} else if (tc == 3) {
						line.morph = token;
					} else if (tc == 4) {
						line.edge = token;
					} else if (tc == 5) {
						line.parent = Integer.parseInt(token);
					}

					if (token.length() > 0)
						tc++;
				}

				// read till #EOS

			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return ps;

	}

}
