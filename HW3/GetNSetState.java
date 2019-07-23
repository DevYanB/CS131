import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSetState(byte[] v){
	int[] passer = new int[v.length];
	for(int i = 0; i < v.length; i++){
	    passer[i] = v[i];
	}
	value = new AtomicIntegerArray(passer);
	maxval = 127;
    }

    GetNSetState(byte[] v, byte m){
	int[] passer = new int[v.length];
	for(int i = 0; i < v.length; i++){
	    passer[i] = v[i];
	}
	value = new AtomicIntegerArray(passer);
	maxval = m;
    }

    public int size() { return value.length(); }

    public byte[] current() {
	byte[] ret = new byte[value.length()];
	for(int i = 0; i < value.length(); i++){
	    ret[i] = (byte) value.get(i);
	}
	return ret;
    }

    public boolean swap(int i, int j) {
        if (value.get(i) <= 0 || value.get(j) >= maxval) {
            return false;
        }
    value.set(i, value.get(i) - 1);
	value.set(j, value.get(j) + 1);
	return true;
    }
}
