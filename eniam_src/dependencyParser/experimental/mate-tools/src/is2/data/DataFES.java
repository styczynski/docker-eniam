package is2.data;

import java.util.Arrays;

final public class DataFES {

	final public short typesLen;
	final public int len;

	// first order features
	final public float[][] pl;

	// remove !!!!
	// final public float[][] highestLab;

	// final public FV[][][] label;
	final public float[][][] lab;

	public FV fv;

	final public float[][][][] sib;

	final public float[][][][] gra;

	public DataFES(int length, short types) {
		typesLen = types;
		len = length;

		pl = new float[length][length];
		lab = new float[length][length][types];

		sib = new float[length][length][length][];
		gra = new float[length][length][length][];

	}
	
	public void print2DFloatArray(float[][] a, String name) {
		for(int i = 0; i < a.length; ++i)
			if(a[i] != null)
				for(int j = 0; j < a[i].length; ++j)
					System.out.println(name + "[" + i + "][" + j + "] = " + a[i][j]);
	}
	
	public void print3DFloatArray(float[][][] a, String name) {
		for(int i = 0; i < a.length; ++i)
			if(a[i] != null)
				for(int j = 0; j < a[i].length; ++j)
					if(a[i][j] != null)
						System.out.println(name + "[" + i + "][" + j + "] = " + Arrays.toString(a[i][j]));
	}
	
	public void print4DFloatArray(float[][][][] a, String name) {
		for(int i = 0; i < a.length; ++i)
			if(a[i] != null)
				for(int j = 0; j < a[i].length; ++j)
					if(a[i][j] != null)
						for(int k = 0; k < a[i][j].length; ++k)
							if(a[i][j][k] != null)
								System.out.println(name + "[" + i + "][" + j + "][" + k + "] = " + Arrays.toString(a[i][j][k]));
	}
	
	public void print() {
		System.out.println("typesLen = " + typesLen);
		System.out.println("len = " + len);
		print2DFloatArray(pl, "pl");
		print3DFloatArray(lab, "lab");
		System.out.println("fv = a feature vector");
		print4DFloatArray(sib, "sib");
		print4DFloatArray(gra, "gra");
	}
}
