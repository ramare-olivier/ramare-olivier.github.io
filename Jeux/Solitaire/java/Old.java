 if(PlateauCourant != null){
            /* copy PlateauCourant from its Javascript counterpart. */
            PlateauCourant.largeur = 
                Integer.parseInt(mainWindow.eval("document.PlateauCourant.largeur").toString());
            PlateauCourant.hauteur = 
                Integer.parseInt(mainWindow.eval("document.PlateauCourant.hauteur").toString());
            int myTable[] = new int[PlateauCourant.largeur*PlateauCourant.hauteur];
            for(int c = 0; c< PlateauCourant.hauteur*PlateauCourant.largeur; c++){
                myTable[c] = 
                    Integer.parseInt(mainWindow.eval("document.PlateauCourant.getTable[c]").toString());}
            PlateauCourant.Table = myTable;
        }
        /* tell Javascript we are ok :*/
        return (PlateauCourant.hauteur*PlateauCourant.largeur);


    public void precalculs ()
    {
        try{
            this.nbcases = calculeNbcases();
            this.coups = calculeCoups();
            this.nbcoups = coups.length + 1;
            createPcodeToCode();
            //createCodeToPcode();
            //createPcodeToPoint();
            //createListeContraintesLin();
        } catch (WrongFormat wf){
            System.out.println(wf.getMessage());
        }
    }
   public void precalculs ()
    {
            this.nbcases = calculeNbcases();
            this.coups = calculeCoups();
            this.nbcoups = coups.length + 1;
            createPcodeToCode();
            createCodeToPcode();
            createPcodeToPoint();
            createListeContraintesLin();
    }
public void createListeContraintesLin() throws WrongFormat {
         // listeContraintesLinEx[pcodepoint][codecoup] = coup(point)
        this.listeContraintesLin = new double[this.nbcases][this.nbcoups];
        for(int c = 0; c < this.nbcoups; c++){
            int pcP = this.codeToPcode[this.xytocode(coups[c].Px, coups[c].Py)];
            int pcQ = this.codeToPcode[this.xytocode(coups[c].Qx, coups[c].Qy)];
            int pcR = this.codeToPcode[this.xytocode(coups[c].Rx, coups[c].Ry)];
            if(pcP < pcR){
                if((pcQ < pcP)||(pcQ > pcR)){ 
                    throw new WrongFormat("Code of "
                                           + "(" + coups[c].Qx + ", " + coups[c].Qy + ") "
                                           + " wrongly placed with respect to "
                                           + "(" + coups[c].Px + ", " + coups[c].Py + ") "
                                           + " or "
                                           + "(" + coups[c].Rx + ", " + coups[c].Ry + ") "
                                           );};
                this.listeContraintesLin[pcP][c] = 1;
                this.listeContraintesLin[pcQ][c] = 1;
                this.listeContraintesLin[pcR][c] = -1;
            } else {
                if((pcQ > pcP)||(pcQ < pcR)){ 
                    throw new WrongFormat("Code of "
                                           + "(" + coups[c].Qx + ", " + coups[c].Qy + ") "
                                           + " wrongly placed with respect to "
                                           + "(" + coups[c].Px + ", " + coups[c].Py + ") "
                                           + " or "
                                           + "(" + coups[c].Rx + ", " + coups[c].Ry + ") "
                                           );};
                this.listeContraintesLin[pcP][c] = 1;
                this.listeContraintesLin[pcQ][c] = 1;
                this.listeContraintesLin[pcR][c] = -1;
            }}
    }

LpSolve mylp = LpSolve.makeLp(0, myPlateau.nbcoups);
            /* Objective : on demande de minimiser le nombre total de coups */
            /* En fait on connait la reponse :) C'est la faisabilite qui nous interesse ! */
            double[] aMinimiser = new double[myPlateau.nbcoups+1];
            for(int c = 1; c < myPlateau.nbcoups+1; c++){
                aMinimiser[c] = 1;
            }
            mylp.setObjFn(aMinimiser);

            /* Add the constraints */
            for(int x = 0; x < myPlateau.hauteur; x++){
                for(int y = 0; y < myPlateau.largeur; y++){
                    if(myPlateau.table[myPlateau.xytocode(x,y)] != 9){
                        int code = myPlateau.xytocode(x,y);
                        int pcode = myPlateau.codeToPcode[code];
                        mylp.addConstraint(myPlateau.listeContraintesLin[pcode], EQ,
                                           posIni[code]-posFin[code]);
                    }}};

            mylp.setAntiDegen(ANTIDEGEN_FIXEDVARS);//cU.Alert("Oups!!");
            mylp.setBreakAtFirst(true);
            mylp.setBreakAtValue(-0.5);

            result = mylp.solve();
