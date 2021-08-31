import netscape.javascript.*;
import java.awt.*; // pour TextArea
import java.util.*; // pour Stack

public class CommQueue {
    boolean valueIsSet = false;
    String mesg;
    static JSObject mainWindow;
    static JSObject derivedInner;
    JSObject branchInner;
    //TextArea derivedTextArea;

    public CommQueue (JSObject mainWindow){
        this.mainWindow = mainWindow;
        this.derivedInner = (JSObject) mainWindow.eval("document.getElementById('derived')");
        //derivedTextArea = new TextArea(10 /*rows*/, 50 /*columns*/);
        //derivedInnerHTML.setMember("innerHTML", derivedTextArea);
    }

    synchronized String send(){
        if(!valueIsSet){
            try{
                wait();
            } catch(InterruptedException e) {
            }
        }
        /* mesg is either
           first word is OPENBRANCH followed by a number 
           first word is BRANCH followed by a string 
           FOLD
           anything */
        String delims = "[ ]+";
        String[] tokens = mesg.split(delims);
        if(tokens[0].contentEquals("OPENBRANCH")){
            branchInner = 
                (JSObject) mainWindow.eval("document.getElementById('tobefolded-"
                                           + tokens[1] +"')");
        } else if(tokens[0].contentEquals("BRANCH")){
            branchInner.setMember("innerHTML", branchInner.getMember("innerHTML") + mesg.substring(7));
        } else if(mesg.contentEquals("FOLD")){
            branchInner.setMember("innerHTML", "--folded--");
        } else {
            derivedInner.setMember("innerHTML", derivedInner.getMember("innerHTML") + mesg);
            //derivedTextArea.append(mesg);
        }
        valueIsSet = false;
        notify();
        return mesg;
    }

    synchronized void receive(String mesg){
        if(valueIsSet){
            try{
                wait();
            } catch(InterruptedException e) {
            }
        }
        this.mesg = mesg;
        valueIsSet = true;
        notify();
    }
}