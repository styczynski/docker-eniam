package is2.parserR2;

import java.util.concurrent.ExecutorService;

import extractors.ExtractorReranker;
import is2.data.Cluster;
import is2.data.Edges;
import is2.data.Instances;
import is2.data.MFB;
import is2.data.PipeGen;
import is2.data.SentenceData09;
import is2.io.CONLLReader09;
import is2.util.OptionsSuper;

final public class PipeReranker extends PipeGen {

	public ExtractorReranker extractor;
	final public MFB mf = new MFB();

	Cluster cl;

	private OptionsSuper options;
	public static long timeExtract;

	public PipeReranker(OptionsSuper o) {
		options = o;
	}

	public void createInstances(String file, Instances is)
	// throws Exception

	{

		CONLLReader09 depReader = new CONLLReader09(file);

		mf.register(REL, "<root-type>");

		// register at least one predicate since the parsing data might not
		// contain predicates as in
		// the Japaness corpus but the development sets contains some

		System.out.print("Registering feature parts of sentence: ");
		int ic = 0;
		int del = 0;
		while (true) {
			SentenceData09 instance = depReader.getNext();
			if (instance == null)
				break;
			ic++;

			if (ic % 1000 == 0) {
				del = outValue(ic, del);
			}

			String[] labs1 = instance.labels;
			for (String element : labs1)
				mf.register(REL, element);

			String[] w = instance.forms;
			for (String element : w)
				mf.register(WORD, depReader.normalize(element));

			w = instance.plemmas;
			for (String element : w)
				mf.register(WORD, depReader.normalize(element));

			w = instance.ppos;
			for (String element : w)
				mf.register(POS, element);

			w = instance.gpos;
			for (String element : w)
				mf.register(POS, element);

			if (instance.feats != null) {
				String fs[][] = instance.feats;
				for (String[] element : fs) {
					w = element;
					if (w == null)
						continue;
					for (String element2 : w)
						mf.register(FEAT, element2);
				}
			}

			if ((ic - 1) > options.count)
				break;
		}
		del = outValue(ic, del);

		System.out.println();
		ExtractorReranker.initFeatures();

		ExtractorReranker.maxForm = mf.getFeatureCounter().get(WORD);

		if (options.clusterFile == null)
			cl = new Cluster();
		else
			cl = new Cluster(options.clusterFile, mf, 6);

		mf.calculateBits();
		ExtractorReranker.initStat();

		System.out.println("" + mf.toString());

		extractor.init();
		depReader.startReading(file);

		int num1 = 0;

		is.init(ic, new MFB());

		Edges.init(mf.getFeatureCounter().get(POS));

		del = 0;

		del = outValue(num1, del);
		System.out.println();
	}

	public static ExecutorService executerService = java.util.concurrent.Executors.newFixedThreadPool(Parser.THREADS);

}
