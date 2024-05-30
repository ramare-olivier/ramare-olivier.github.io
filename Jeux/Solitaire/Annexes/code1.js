alert("about to add a new script to the DOM. click OK to see the current source as the interpreter knows it..")

alert(document.documentElement.innerHTML)

document.write('<script src="code2.js"></script>')

alert("seeing this message means the script was added to the DOM. click okay to see the current source as the interpreter knows it..")

alert(document.documentElement.innerHTML)

alert("this is the code1.js alert. this is running because we have not gotten to the next script in the DOM yet")

try
{
	alert("this is the alert of code2's variable: " + code2variable)
}
catch(err)
{
	alert("there was an error trying to alert the variable from code2.js (because it hasnt been executed yet):\n\n " + err.description)
}

alert("done with code1.js")