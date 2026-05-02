public class Receiver implements Runnable
{
    static CommQueue q;

    public Receiver(CommQueue q){
        this.q = q;
        new Thread(this, "Receiver").start();
    }

    public void run(){
        while(true){
            q.send();
        }
    }
}