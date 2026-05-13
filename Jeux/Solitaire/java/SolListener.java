import lpsolve.*;

class SolListener implements LogListener {
    public int  numCalls = 0;
    static Communicator cU;
    public SolListener(Communicator cU){
        this.cU = cU;
    }

    public void logfunc(LpSolve prob, Object handle, String buf) {
        numCalls++;
        cU.Alert("From LpSolve: " + buf);
    }
};