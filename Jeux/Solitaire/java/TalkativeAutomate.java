import netscape.javascript.*;
import java.applet.*;
import java.awt.*;
import java.util.*;

public class TalkativeAutomate
{
    static Communicator commUnit;
    static Automate myMachine;
    static Receiver funk;

    public TalkativeAutomate(Etude myJeu, PlateauSolitaire myPlateau, JSObject mainWindow){
        CommQueue q = new CommQueue(mainWindow);
        commUnit = new Communicator(mainWindow);
        funk = new Receiver(q);
        myMachine = new Automate(commUnit, q, myJeu, myPlateau);
    }
}