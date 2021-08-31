var Planiket_selectedstart=-1; // -1: aucune case de selectionnee 
var Planiket_message="";
var Planiket_history=0;
var initialcolor="#2EFEF7";
var selectedcolor="#F3F781";
var intermediatecolor="#C8FE2E";
var explodecolor="#58FA58";
var Planiket_pos = [ 1, 2, 3, 4, 5, 6, 7, 8, 9];

function Planiket_win() {     
  if (Planiket_pos[0] == 1 & Planiket_pos[1] == 2 & Planiket_pos[2] == 3 & Planiket_pos[3] == 4
      & Planiket_pos[4] == 5 & Planiket_pos[5] == 6 & Planiket_pos[6] == 7 & Planiket_pos[7] == 8
      & Planiket_pos[8] == 9 ){
      if (confirm('You did it! Do you want to restart?')) newPlaniketgame();}
}

function Planiket_display() {
  for (var i = 0; i<9; i++)  {
       document.forms["Planiket"].elements[i+1].value = "  " + Planiket_pos[i] + "  "};
   if(Planiket_message != ""){
     if(Planiket_history == 7){
       Planiket_message = "\r"+Planiket_message;
       Planiket_history = 0};
     document.forms["Planiket"].Planikethistorique.value
       = document.forms["Planiket"].Planikethistorique.value + Planiket_message + ", ";
     Planiket_history++;
   }
   document.forms["Planiket"].Planikethistorique.scrollTop = document.forms["Planiket"].Planikethistorique.scrollHeight;
   Planiket_win();
}

function Planiket_mycoloring(num, mycolor){
 switch (num){
    case 1: document.forms["Planiket"].Planiket_button1.style.background=mycolor; break;
    case 2: document.forms["Planiket"].Planiket_button2.style.background=mycolor; break;
    case 3: document.forms["Planiket"].Planiket_button3.style.background=mycolor; break;
    case 4: document.forms["Planiket"].Planiket_button4.style.background=mycolor; break;
    case 5: document.forms["Planiket"].Planiket_button5.style.background=mycolor; break;
    case 6: document.forms["Planiket"].Planiket_button6.style.background=mycolor; break;
    case 7: document.forms["Planiket"].Planiket_button7.style.background=mycolor; break;
    case 8: document.forms["Planiket"].Planiket_button8.style.background=mycolor; break;
    case 9: document.forms["Planiket"].Planiket_button9.style.background=mycolor; break;
    default: break;
    }
}

function Planiket_shift(x,y){
   var aux;
   aux = Planiket_pos[x-1];
   Planiket_pos[x-1] = Planiket_pos[y-1];
   Planiket_pos[y-1] = aux;
}  

function Planiket_elementary_move(x,y){
   var z;
   z = (x+y)/2;
   Planiket_shift(x,y);
   Planiket_mycoloring(x, intermediatecolor);
   Planiket_mycoloring(y, intermediatecolor);
   Planiket_mycoloring(z, intermediatecolor);
   setTimeout(function(){
        Planiket_mycoloring(x, initialcolor);
        Planiket_mycoloring(z, initialcolor);
        Planiket_mycoloring(y, initialcolor);}, 400);
   Planiket_selectedstart = -1;
}

function Planiket_move(num) {
  <!-- Acceleration -->
  if (num == 4){  Planiket_selectedstart = 6
  } else if (num == 6){  Planiket_selectedstart = 4
  } else if (num == 2){  Planiket_selectedstart = 8
  } else if (num == 8){  Planiket_selectedstart = 2};

  if(Planiket_selectedstart == -1){
    Planiket_selectedstart = num;
    Planiket_message="";
    Planiket_mycoloring(num, selectedcolor);
  } else if (Planiket_selectedstart == num) {
    Planiket_message="";
    Planiket_selectedstart = -1;
    Planiket_mycoloring(num, initialcolor);
  } else if( (Planiket_selectedstart==1 & num == 3)|
             (Planiket_selectedstart==3 & num == 1)){
    Planiket_elementary_move(1,3);
    Planiket_message="a";
  } else if( (Planiket_selectedstart==4 & num == 6)|
             (Planiket_selectedstart==6 & num == 4)){
    Planiket_elementary_move(4,6);
    Planiket_message="b";
  } else if( (Planiket_selectedstart==7 & num == 9)|
             (Planiket_selectedstart==9 & num == 7)){
    Planiket_elementary_move(7,9);
    Planiket_message="c";
  } else if( (Planiket_selectedstart==1 & num == 7)|
             (Planiket_selectedstart==7 & num == 1)){
    Planiket_elementary_move(1,7);
    Planiket_message="A";
  } else if( (Planiket_selectedstart==2 & num == 8)|
             (Planiket_selectedstart==8 & num == 2)){
    Planiket_elementary_move(2,8);
    Planiket_message="B";
  } else if( (Planiket_selectedstart==3 & num == 9)|
             (Planiket_selectedstart==9 & num == 3)){
    Planiket_elementary_move(3,9);
    Planiket_message="C";
  } else {
    Planiket_message="Impossible !";
  };
  Planiket_display();
}

function Planiket_random_position (){
  var randomnumber, i, longueur;
  longueur = Math.floor(Math.random()*15);
  Planiket_pos = [ 1, 2, 3, 4, 5, 6, 7, 8, 9];
  for(i = 0; i < longueur; i++){
    randomnumber = Math.floor(Math.random()*6);
    switch(randomnumber){
    case 1: Planiket_shift( 1, 3);break;
    case 2: Planiket_shift( 4, 6);break;
    case 3: Planiket_shift( 7, 9);break;
    case 4: Planiket_shift( 1, 7);break;
    case 5: Planiket_shift( 2, 8);break;
    case 6: Planiket_shift( 3, 9);break;
    }}
}

function newPlaniketgame() {
  switch (document.forms["Planiket"].Debut.value){
    case "d1": Planiket_pos = [ 9, 2, 3, 4, 5, 6, 7, 8, 1]; break;
    case "d2": Planiket_random_position(); break;
    case "d3": Planiket_pos = [ 9, 2, 7, 4, 5, 6, 3, 8, 1]; break;
    default : Planiket_pos = [ 9, 2, 7, 4, 5, 6, 3, 8, 1]; break;
  };
  for(var i=1; i<10; i++){
    Planiket_mycoloring(i, initialcolor);};
  Planiket_selectedstart = -1;
  Planiket_message = "";
  Planiket_history = 0;
  document.forms["Planiket"].Planikethistorique.value = "";
  Planiket_display();   
}

<!-- ---------------------------------------------------------------------------- -->
 <BODY class="pas_surlignable page_rubrique"
      onLoad="window.newPlaniketgame()">

<!-- ---------------------------------------------------------------------------- -->

    <table class="fondgris">
            <tr>
              <td> <input type="button" name="Planiket_button1" value="  1  "
                          onClick="window.Planiket_move(1)"> </td>
              <td> <input type="button" name="Planiket_button2" value="  2  "
                          onClick="window.Planiket_move(2)"> </td>
              <td> <input type="button" name="Planiket_button3" value="  3  "
                      onClick="window.Planiket_move(3)"> </td>
            </tr>
            <tr>
              <td> <input type="button" name="Planiket_button4" value="  4  "
                          onClick="window.Planiket_move(4)"> </td>
              <td> <input type="button" name="Planiket_button5" value="  5  " > </td>
              <td> <input type="button" name="Planiket_button6" value="  6  "
                          onClick="window.Planiket_move(6)"> </td>
            </tr>
            <tr>
              <td> <input type="button" name="Planiket_button7" value="  7  "
                          onClick="window.Planiket_move(7)"> </td>
              <td> <input type="button" name="Planiket_button8" value="  8  "
                          onClick="window.Planiket_move(8)"> </td>
              <td> <input type="button" name="Planiket_button9" value="  9  "
                          onClick="window.Planiket_move(9)"> </td>
            </tr>
          </table>
