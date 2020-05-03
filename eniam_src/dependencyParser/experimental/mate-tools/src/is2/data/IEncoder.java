/**
 *
 */
package is2.data;

/**
 * @author Bernd Bohnet, 20.09.2009
 *
 *
 */
public interface IEncoder {
	public int getValue(String a, String v);

	default public void print() {
		throw new UnsupportedOperationException();
	};
	/**
	 * @param spath
	 * @param substring
	 */
	// public int register(String spath, String substring);

	/**
	 * @return
	 */
	// public HashMap<String,Integer> getFeatureCounter();
}
