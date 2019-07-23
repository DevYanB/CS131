import java.util.concurrent.locks.ReentrantLock;

public class BetterSafeState implements State{
    private byte[] value;
    private byte max;
    private final ReentrantLock lock = new ReentrantLock();

    BetterSafeState(byte[] val){
        value = val;
        max = 127;
    }

    BetterSafeState(byte[] val, byte maxvalue){
        value = val;
        max = maxvalue;
    }

    public int size(){
        return value.length;
    }

    public byte[] current(){
        return value;
    }

    public boolean swap(int i, int j){
	lock.lock();
        if(value[i] <= 0 || value[j] >= max){
	    lock.unlock();
	    return false;
        }
        value[i]--;
        value[j]++;
        lock.unlock();
        return true;
    }

}
