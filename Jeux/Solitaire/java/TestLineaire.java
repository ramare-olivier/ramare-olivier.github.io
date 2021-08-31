import java.util.*; // pour Stack
import lpsolve.*;

public class TestLineaire
{
    static Communicator cU;
    static SolListener listener;

    public TestLineaire(Communicator commUnit){
        this.cU = commUnit;
        this.listener = new SolListener(cU);
    }

    public static boolean AnalyseReponseLP(int resultat){
        /* Answer is true if resultat is SOLVED or UNFINISHED */
        switch (resultat) {
        case LpSolve.NOMEMORY:  return true;
        case LpSolve.OPTIMAL: return true;
        case LpSolve.SUBOPTIMAL: return true;
        case LpSolve.INFEASIBLE: return false;
        case LpSolve.UNBOUNDED: return true;
        case LpSolve.DEGENERATE: return true; /* Don't know what to do !!! */
        case LpSolve.NUMFAILURE: return true; /* Don't know what to do !!! */
        case LpSolve.PROCFAIL: return true; /* Don't know what to do !!! */
        case LpSolve.PROCBREAK: return true; 
        case LpSolve.FEASFOUND: return true; 
        case LpSolve.NOFEASFOUND: return false; 
        default: return true;
        }
    }

    public static boolean FaisableEnRationnelsPositifs(){
        /** Construire le programme lineaire : **/
        /** le programme consiste a dire :
            on a myPlateau.nbcoups coups possibles, les fameux f = P + Q - R,
            qu'on utilise en quantite positive. 
            Les contraintes imposees sont

            la somme des f(X) sur toutes les niches X donne la difference
            entre le nombre de pions sur cette case et celui a l'arrivee,
            
            soit la plupart du temps 1 si le pion existait et est
            bouffe, 0 si il n'existait pas et n'est pas la a l'arrivee
            non plus, et -1 si il n'etait pas la au depart mais l'est
            a l'arrivee.

            Il y a donc myPlateau.nbcoups variables a coefficients entiers positifs,
            et myPlateau.nbncases contraintes. Ces contraintes sont precalculees
            et stockees dans myPlateau.listeContraintesLinEx.

            Comme fonction a minimiser, on peut prendre plein de choses, puisque
            c'est en fait la faisabilite qui nous interesse. 
            Ici on compte le nombre de coups, qui doit etre inferieur ...
            au nombre de coups calcule au depart, auquel on ajoute 0.5 pour etre
            protege de toute erreur de calcul.
        **/
        int result = LpSolve.OPTIMAL;
        
        cU.Alert("Entering FaisableEnRationnelsPositifs "+result);
        try{
            LpSolve mylp = LpSolve.makeLp(0, 0);
            mylp.putLogfunc(listener, new Integer(123));
            cU.Alert("In FaisableEnRationnelsPositifs ... ");
        } catch (LpSolveException lpse){
            cU.Alert("Exception in FaisableEnRationnelsPositifs!!");
            System.out.println("Ouinnnn ");
            lpse.printStackTrace();
        }
        cU.Alert("Out of FaisableEnRationnelsPositifs!");
        return AnalyseReponseLP(result);
    }


    /* All the constants from 
       http://web.mit.edu/lpsolve_v5520/java/docs/api/constant-values.html#lpsolve.LpSolve.SIMPLEX_DUAL_PRIMAL
    static final int ANTIDEGEN_BOUNDFLIP = 512;
    static final int ANTIDEGEN_COLUMNCHECK = 2;
    static final int ANTIDEGEN_DURINGBB = 128;
    static final int ANTIDEGEN_DYNAMIC = 64;
    static final int ANTIDEGEN_FIXEDVARS = 1;
    static final int ANTIDEGEN_INFEASIBLE = 32;
    static final int ANTIDEGEN_LOSTFEAS = 16;
    static final int ANTIDEGEN_NONE = 0;
    static final int ANTIDEGEN_NUMFAILURE = 8;
    static final int ANTIDEGEN_RHSPERTURB = 256;
    static final int ANTIDEGEN_STALLING = 4;
    static final int AUTOMATIC = 2;
    static final int BRANCH_AUTOMATIC = 2;
    static final int BRANCH_DEFAULT = 3;
    static final int BRANCH_CEILING = 0;
    static final int BRANCH_FLOOR = 1;
    static final int CRASH_MOSTFEASIBLE = 2;
    static final int CRASH_NOTHING = 0;
    static final int CRITICAL = 1;
    static final int DATAIGNORED = -4;
    static final int DEGENERATE = 4;
    static final int DETAILED = 5;
    static final int DYNAMIC = 4;
    static final int EQ = 3;
    static final int FALSE = 0;
    static final int FEASFOUND = 12;
    static final int FR = 0;
    static final int FULL = 6;
    static final int PRESOLVED = 9;
    static final int GE = 2;
    static final int IMPORTANT = 3;
    static final int IMPROVE_BBSIMPLEX = 8;
    static final int IMPROVE_DUALFEAS = 2;
    static final int IMPROVE_NONE = 0;
    static final int IMPROVE_SOLUTION = 1;
    static final int IMPROVE_THETAGAP = 4;
    static final int INFEASIBLE = 2;
    static final int LE = 1;
    static final int MSG_INITPSEUDOCOST = 8192;
    static final int MSG_INVERT = 4;
    static final int MSG_ITERATION = 2;
    static final int MSG_LPBETTER = 64;
    static final int MSG_LPEQUAL = 32;
    static final int MSG_LPFEASIBLE = 8;
    static final int MSG_LPOPTIMAL = 16;
    static final int MSG_MILPBETTER = 512;
    static final int MSG_MILPEQUAL = 256;
    static final int MSG_MILPFEASIBLE = 128;
    static final int MSG_MILPOPTIMAL = 2048;
    static final int MSG_MILPSTRATEGY = 1024;
    static final int MSG_NONE = 0;
    static final int MSG_PERFORMANCE = 4096;
    static final int MSG_PRESOLVE = 1;
    static final int NEUTRAL = 0;
    static final int NOBFP = -3;
    static final int NODE_AUTOORDER = 8192;
    static final int NODE_BRANCHREVERSEMODE = 16;
    static final int NODE_BREADTHFIRSTMODE = 4096;
    static final int NODE_DEPTHFIRSTMODE = 128;
    static final int NODE_DYNAMICMODE = 1024;
    static final int NODE_FIRSTSELECT = 0;
    static final int NODE_FRACTIONSELECT = 3;
    static final int NODE_GAPSELECT = 1;
    static final int NODE_GREEDYMODE = 32;
    static final int NODE_PSEUDOCOSTMODE = 64;
    static final int NODE_PSEUDOCOSTSELECT = 4;
    static final int NODE_PSEUDONONINTSELECT = 5;
    static final int NODE_PSEUDORATIOSELECT = 6;
    static final int NODE_RANDOMIZEMODE = 256;
    static final int NODE_RANGESELECT = 2;
    static final int NODE_RCOSTFIXING = 16384;
    static final int NODE_RESTARTMODE = 2048;
    static final int NODE_STRONGINIT = 32768;
    static final int NODE_USERSELECT = 7;
    static final int NODE_WEIGHTREVERSEMODE = 8;
    static final int NOFEASFOUND = 13;
    static final int NOMEMORY = -2;
    static final int NORMAL = 4;
    static final int NOTRUN = -1;
    static final int NUMFAILURE = 5;
    static final int OF = 4;
    static final int OPTIMAL = 0;
    static final int PRESOLVE_BOUNDS = 262144;
    static final int PRESOLVE_COLDOMINATE = 16384;
    static final int PRESOLVE_COLFIXDUAL = 131072;
    static final int PRESOLVE_COLS = 2;
    static final int PRESOLVE_DUALS = 524288;
    static final int PRESOLVE_ELIMEQ2 = 256;
    static final int PRESOLVE_IMPLIEDFREE = 512;
    static final int PRESOLVE_IMPLIEDSLK = 65536;
    static final int PRESOLVE_KNAPSACK = 128;
    static final int PRESOLVE_LINDEP = 4;
    static final int PRESOLVE_MERGEROWS = 32768;
    static final int PRESOLVE_NONE = 0;
    static final int PRESOLVE_PROBEFIX = 2048;
    static final int PRESOLVE_PROBEREDUCE = 4096;
    static final int PRESOLVE_REDUCEGCD = 1024;
    static final int PRESOLVE_REDUCEMIP = 64;
    static final int PRESOLVE_ROWDOMINATE = 8192;
    static final int PRESOLVE_ROWS = 1;
    static final int PRESOLVE_SENSDUALS = 1048576;
    static final int PRESOLVE_SOS = 32;
    static final int PRICE_ADAPTIVE = 32;
    static final int PRICE_AUTOPARTIAL = 512;
    static final int PRICE_HARRISTWOPASS = 4096;
    static final int PRICE_HYBRID = 64;
    static final int PRICE_LOOPALTERNATE = 2048;
    static final int PRICE_LOOPLEFT = 1024;
    static final int PRICE_METHODDEFAULT = 0;
    static final int PRICE_MULTIPLE = 8;
    static final int PRICE_PARTIAL = 16;
    static final int PRICE_PRIMALFALLBACK = 4;
    static final int PRICE_RANDOMIZE = 128;
    static final int PRICE_TRUENORMINIT = 16384;
    static final int PRICER_DANTZIG = 1;
    static final int PRICER_DEVEX = 2;
    static final int PRICER_FIRSTINDEX = 0;
    static final int PRICER_STEEPESTEDGE = 3;
    static final int PROCBREAK = 11;
    static final int PROCFAIL = 10;
    static final int RUNNING = 8;
    static final int SCALE_COLSONLY = 1024;
    static final int SCALE_CURTISREID = 7;
    static final int SCALE_DYNUPDATE = 256;
    static final int SCALE_EQUILIBRATE = 64;
    static final int SCALE_EXTREME = 1;
    static final int SCALE_GEOMETRIC = 4;
    static final int SCALE_INTEGERS = 128;
    static final int SCALE_LINEAR = 0;
    static final int SCALE_LOGARITHMIC = 16;
    static final int SCALE_MEAN = 3;
    static final int SCALE_NONE = 0;
    static final int SCALE_POWER2 = 32;
    static final int SCALE_QUADRATIC = 8;
    static final int SCALE_RANGE = 2;
    static final int SCALE_ROWSONLY = 512;
    static final int SCALE_USERWEIGHT = 31;
    static final int SEVERE = 2;
    static final int SIMPLEX_DEFAULT = 6;
    static final int SIMPLEX_DUAL_DUAL = 10;
    static final int SIMPLEX_DUAL_PRIMAL = 6;
    static final int SIMPLEX_PRIMAL_DUAL = 9;
    static final int SIMPLEX_PRIMAL_PRIMAL = 5;
    static final int SUBOPTIMAL = 1;
    static final int TIMEOUT = 7;
    static final int TRUE = 1;
    static final int UNBOUNDED = 3;
    static final int UNKNOWNERROR = -5;
    static final int USERABORT = 6;
     */
    /*
       public static void main(String[] args){
        int[] posIni = new int[16];
        int[] posFin = new int[16];
        int[] myTable = new int[16];
        for(int c = 0; c < 16; c++){
            posIni[c] = 1; posFin[c] = 1; myTable[c] = 1; 
        }
        PlateauSolitaire myPlateau = new PlateauSolitaire(4, 4, myTable, "Essai");
        
        System.out.println(FaisableEnRationnelsPositifs(posIni, posFin, myPlateau));
        
        }
    */
}