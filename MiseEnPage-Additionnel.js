window.onscroll = function() {myFunction()};

var navbar = document.getElementById("navbar");
var sticky = navbar.offsetTop;

function myFunction() {
  if (window.pageYOffset >= sticky+document.getElementById("logo").offsetHeight) {
      navbar.classList.add("sticky");
  } else {
    navbar.classList.remove("sticky");
  }
}
