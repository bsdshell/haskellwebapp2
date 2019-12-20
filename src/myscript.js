function copyStringToClipboard (str) {
   // Create new element
   var el = document.createElement('textarea');
   // Set value (string to be copied)
   el.value = "my strnig";
   // Set non-editable to avoid focus and move outside of view
   el.setAttribute('readonly', '');
   el.style = {position: 'absolute', left: '-9999px'};
   document.body.appendChild(el);
   // Select text inside element
   el.select();
   alert(el.toString());
   // Copy text to clipboard
   document.execCommand('copy');
   // Remove temporary element
   document.body.removeChild(el);
}

function copyToClipboard(elementId) {

  // Create an auxiliary hidden input
  var aux = document.createElement("input");

  // Get the text from the element passed into the input
  aux.setAttribute("value", "my nice string");

  // Append the aux input to the body
  document.body.appendChild(aux);

  // Highlight the content
  aux.select();

  // Execute the copy command
  document.execCommand("copy");

  // Remove the input from the body
  document.body.removeChild(aux);

}

var clip = function(el) {
    'use strict';
  var range = document.createRange();
  var textNode = el.childNodes[0];
  range.selectNodeContents(el);
  range.setStart(textNode, 2);
  range.setEnd(textNode, 4);
  var sel = window.getSelection();
  sel.removeAllRanges();
  
  sel.addRange(range);
  alert(sel.toString());

  try{
      document.execCommand('copy');
  }catch(err){
      alert('Please press Ctrl/Cmd+C to copy');
  }
};


var clip1 = function(el) {
  'use strict';
  var range = document.createRange();
  range.selectNodeContents(el);
  var sel = window.getSelection();
  sel.removeAllRanges();
  sel.addRange(range);
  alert(sel.toString());
  var array = sel.toString().trim().split('\n');
  array.splice(0, 1);
  // var str = array.join("\r\n");
  var str = array.join("<br>");
  alert(str);

  sel.removeAllRanges();

  // 'input' only one line
  // 'textarea' should be used?, does not work so far
  var aux = document.createElement("textarea");

  // Get the text from the element passed into the input
  aux.setAttribute('value', str);
  // aux.setAttribute('style', 'white-space: pre;');

  // Append the aux input to the body

  document.body.appendChild(aux);

  // Highlight the content
  aux.select();

  // Execute the copy command
  document.execCommand("copy");

  // Remove the input from the body
  document.body.removeChild(aux);

};

function textAreaAdjust(o) {
    o.style.height = "1px";
    o.style.height = (25+o.scrollHeight)+"px";
}


function showandhide(id) {
     var formobj=document.getElementById('f' + id);
     if(formobj.className == 'hf'){  //check if classname is hide 
	formobj.style.display = 'block';
	formobj.className ='hfshow';
     }else if(formobj.className == 'hfshow'){
	formobj.style.display = 'none';
	formobj.className ='hf';
     }

     var selectedobj=document.getElementById('t' + id);
     if(selectedobj.className == 'hide'){  //check if classname is hide 
	selectedobj.style.display = 'block';
	selectedobj.className ='show';
     }else if(selectedobj.className == 'show'){
	selectedobj.style.display = 'none';
	selectedobj.className ='hide';
     }

     var butobj=document.getElementById('b' + id);
     if(butobj.className == 'submitButton'){  //check if classname is hide 
	butobj.style.display = 'inline-flex';
	butobj.className ='butshow';
     }else if(butobj.className == 'butshow'){
	butobj.style.display = 'none';
	butobj.className ='submitButton';
     }

     var butobj=document.getElementById('a' + id);
     if(butobj.className == 'submitButton'){  //check if classname is hide 
	butobj.style.display = 'inline-flex';
	butobj.className ='butshow';
     }else if(butobj.className == 'butshow'){
	butobj.style.display = 'none';
	butobj.className ='submitButton';
     }

     var butobj=document.getElementById('d' + id);
     if(butobj.className == 'submitButton'){  //check if classname is hide 
	butobj.style.display = 'inline-flex';
	butobj.className ='butshow';
     }else if(butobj.className == 'butshow'){
	butobj.style.display = 'none';
	butobj.className ='submitButton';
     }

     var butobj=document.getElementById('c' + id);
     var cname = 'co' + id;
     if(butobj.className == cname){  
	butobj.className = 'colorclick';
     }else if(butobj.className == 'colorclick'){
	butobj.className = cname; 
     }

}
