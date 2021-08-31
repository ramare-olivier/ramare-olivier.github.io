/*
  javac -cp /usr/lib/jvm/java-6-sun-1.6.0.26/jre/lib/plugin.jar:. Coup.java 
*/

public class Coup
{
    public Coup (int Px, int Py, int Qx, int Qy, int Rx, int Ry)
    {   this.Px = Px;
        this.Py = Py;
        this.Qx = Qx;
        this.Qy = Qy;
        this.Rx = Rx;
        this.Ry = Ry;
    }
    int Px, Py, Qx, Qy, Rx, Ry;
}