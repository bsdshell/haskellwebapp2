  // Load text from TextArea                                                
  // Highlight the text and add to div ID.RENDERID                          

function test(){
    alert('test it, it is from src/aronlib.js');
}

var clip = function(el) {
    'use strict';
  var range = document.createRange();
  range.selectNodeContents(el);
  var sel = window.getSelection();
  sel.removeAllRanges();
  sel.addRange(range);
  //alert(sel);
  try{
      document.execCommand('copy');
  }catch(err){
      alert('Please press Ctrl/Cmd+C to copy');
  }
}

function take(n, list){
    return list.slice(0, n);
}

function drop(n, list){
    return list.slice(n);
}

function compareArray(arr1, arr2){
    if(arr1.length == arr2.length){
        for(var i=0; i<arr1.length; i++){
            if(arr1[i] != arr2[i])
                return false;
        }
    }
    return true;
}

function prefixStr(str){
    var list = [];
    for(var i=0; i<str.length; i++){
        list.push(str.substr(0, i+1)); 
    }
    return list;
}


function geneHtmlTable(ncol, nrow, id){
    const sty = `style= 
                    "background-color:#DDDFFD;
                    font-size:20px;
                    boarder: 1px solid black;
                    ";
                `;
    var ret = '';
        ret += '<table ' + sty + '>';
        for(var i = 0; i < ncol; i++){
            ret += '<tr>';
            for(var j = 0; j < nrow; j++){
                ret += "<td contenteditable='true'>";
                ret += '0';
                ret += '</td>';
            }
            ret += '</tr>';
        }
        ret += '</table>';

        var results = document.getElementById(id);//myDiv is the div id
        results.innerHTML = ret;  
    return ret;
}

let g_numToLineMap = new Map();

const CONS = {
    NL : '\n',  // newline
    HTMLNL : '<br>', // html newline
    UNDEF : undefined,
    EMPTY : '' 
}

const ID={
    RENDER : 'render',
    TEXTAREA : 'txtAreaId'
}

// MacOS
const KEY = {
    DELETE : 8,
    ENTER  : 13,
    LEFT   : 37,
    UP     : 38,
    RIGHT  : 39,
    DOWN   : 40,
    TAB   : 9 
}

function chop(n, str){
    var fst = take(n, str);
    var snd = drop(n, str);
    return {
        fst : fst,
        snd : snd
    };
}

function take(n, str){
    return str.substr(0, n);
}
function drop(n, str){
    return str.substr(n, str.length);
}

function prefixMap2(list){
    var map = new Map();
    for(var i=0; i<list.length; i++){
        for(var j=0; j<list[i].length; j++){
            var ps = chop(j + 1, list[i]);
            if(ps.fst.length > 0 && ps.snd.length > 0){
                var v = map.get(ps.fst);
                if(v == undefined){
                    v = [];
                }
                v.push(ps.snd);
                map.set(ps.fst, v);
            }
        }
    }
    return map;
}

function prefixMap(list){
    var map = new Map();
    for(var i=0; i<list.length; i++){
        var preList = prefixStr(list[i]);
        for(var j=0; j<preList.length; j++){
            var v = map.get(preList[j]);
            if(v == undefined){
                v = [];
            }
            v.push(list[i]);
            map.set(preList[j], v);
        }
    }
    return map;
}


class LineTxtCSS{
    text = '';
    cssText = '';
    constructor(text, cssText){
        this.text = text;
        this.cssText = cssText;
    }
}

function color(s) {
    return "<span style='color:green;'>" + s + "</span>";
}

function colorx(s, col) {
    return "<span style='color:" + col + "';>" + s + "</span>";
}


function logKey(e) {
  // alert(e.code);
}

function hideById(id){
    document.getElementById(id).style.display = 'none';
}
function showById(id){
    document.getElementById(id).style.display = 'block';
}
function isHidden(id){
    return document.getElementById(id).style.display == 'none';
}
  function isAutoCompleteDivShown(){
      return document.getElementById('autoCompleteDiv').style.display == 'block';
  }

function getElem(id){
    return document.getElementById(id);
}

function isPrintableKey(keycode){
    const valid = 
          (keycode > 47 && keycode < 58)   || // number keys
        keycode == 32                    || // spacebar & return key(s) (if you want to allow carriage returns)
	(keycode > 64 && keycode < 91)   || // letter keys
        (keycode > 95 && keycode < 112)  || // numpad keys
        (keycode > 185 && keycode < 193) || // ;=,-./` (in order)
        (keycode > 218 && keycode < 223); 
    return valid;
}

// return the begin index of string
function begWord(str, start){
    var beg = 0;
    for(var i=start; i>=0; i--){
        if(str[i] == ' '){
            beg = i;
            break;
        }
    }
    return beg;
}

function buildPrefixMapFromId(id){
    var els = getElem(id);
    var strList = els.innerHTML.split('\n');
    var fList = strList.filter(function(x){ return x.length > 0 })
    var map = prefixMap2(fList);
    return map;
}

  function autoCompleteFun(event){
        if(event.keyCode == KEY.LEFT){                       
            var html = getSelectionHtml();
            var txtArea = getElem(ID.TEXTAREA);            	  
            var autoCompleteDiv = getElem('autoCompleteDiv');
	    txtArea.setRangeText(html);                	  
          
	    autoCompleteDiv.innerHTML = CONS.EMPTY;        	  
            hideById('autoCompleteDiv');                   
                                                             
            var renderId = getElem(ID.RENDER);             
            g_numToLineMap = buildLineMap(txtArea.value);  
            renderId.innerHTML = parse(txtArea.value);
	    
            var updatedTxtArea = getElem(ID.TEXTAREA);
            setTxtAreaCursorXYOffset(updatedTxtArea, html.length);
            var start = updatedTxtArea.selectionStart;
	    setCaretToPos(updatedTxtArea, start + html.length, start + html.length);
            txtArea.focus();                           	  
        } else if((event.keyCode == KEY.UP || event.keyCode == KEY.DOWN)){
            var autoCompleteDiv = getElem('autoCompleteDiv');
	}
  }                                                              
function setSelectionRange(input, selectionStart, selectionEnd) {
  if (input.setSelectionRange) {
    input.focus();
    input.setSelectionRange(selectionStart, selectionEnd);
  }
  else if (input.createTextRange) {
    var range = input.createTextRange();
    range.collapse(true);
    range.moveEnd('character', selectionEnd);
    range.moveStart('character', selectionStart);
    range.select();
  }
}

function setCaretToPos (input, pos) {
   setSelectionRange(input, pos, pos);
}
  
function autoComplete(txtAreaId){

    // if keyCode is not left
    //   
    // if keyCode is left

    var txtArea = getElem(txtAreaId);
    var str = txtArea.value;
    var start = txtArea.selectionStart;
    var beg = begWord(str, start);

    var map = buildPrefixMapFromId('completelist'); 

    //  If map contains key, then return a list of str
    var word = str.substr(beg, start - beg + 1).trim();
    var autoCompleteDiv = getElem('autoCompleteDiv');
    var value = map.get(word);
    if(value != CONS.UNDEF){
        // alert('val');
        showById('autoCompleteDiv');
        var autoStr = value.join('<br>');
        var txtAreaElem = getElem(ID.TEXTAREA);
        if (txtAreaElem.setRangeText) {
            // getElem('autoCompleteDiv').focus();

            autoCompleteDiv.innerHTML = autoStr;
	    
	    // Add string from autoCompleteDiv to ID.RENDER
            /*
	      var html = getSelectionHtml();
              txtAreaElem.setRangeText(html);
              autoCompleteDiv.innerHTML = CONS.EMPTY; 
              hideById('autoCompleteDiv');
            
              var renderId = getElem(ID.RENDER);
              var txtArea = getElem(ID.TEXTAREA);
              g_numToLineMap = buildLineMap(txtArea.value);
              renderId.innerHTML = parse(txtArea.value); 
              setTxtAreaCursorXY(txtAreaElem);
              txtAreaElem.focus();
            */
            document.addEventListener('keydown', logKey);
        } else {
            txtAreaElem.focus()
            document.execCommand('insertText', false /*no UI*/, value);
        }
    }
}


function getCurrWord(txtArea){
    var txtArea = getElem(ID.TEXTAREA);
    var str = txtArea.value;
    var start = txtArea.selectionStart;
    var beg = begWord(str, start);

    var map = buildPrefixMapFromId('completelist'); 

    
    //  If map contains key, then return a list of str
    var word = str.substr(beg, start - beg + 1).trim();
    var autoCompleteDiv = getElem('autoCompleteDiv');
    var value = map.get(word);
    if(value != CONS.UNDEF){
        showById('autoCompleteDiv');
        var autoStr = value.join('<br>');
        var txtAreaElem = getElem(ID.TEXTAREA);
        if (txtAreaElem.setRangeText) {
            getElem('autoCompleteDiv').focus();

            autoCompleteDiv.innerHTML = autoStr; 
            getElem('autoCompleteDiv').onkeyup = function(e) {
              var html = getSelectionHtml();
              if(e.keyCode == KEY.LEFT){
                  txtAreaElem.setRangeText(html);
                  autoCompleteDiv.innerHTML = CONS.EMPTY; 
                  hideById('autoCompleteDiv');

                  var renderId = getElem(ID.RENDER);
                  var txtArea = getElem(ID.TEXTAREA);
                  g_numToLineMap = buildLineMap(txtArea.value);
                  renderId.innerHTML = parse(txtArea.value); 
                  setTxtAreaCursorXY(txtArea);
                  txtAreaElem.focus();
              }
            };

            document.addEventListener('keydown', logKey);
        } else {
            txtAreaElem.focus()
            document.execCommand('insertText', false /*no UI*/, value);
        }
    }
}

function setTxtAreaCursorXYOffset(txtArea, offset){
    var xy = getCursorXY(txtArea, txtArea.selectionStart + offset);        
    var mycursor = getElem('fakeCursorId');                     
    const LEFT = -8;                                              
    const TOP = -7;                                             
    mycursor.style.left = xy.x + LEFT + xy.spanX + 'px';          
    mycursor.style.top  = xy.y + TOP + xy.spanY + 'px';         
                                                                  
    var autoCompleteDiv = getElem('autoCompleteDiv');           
    autoCompleteDiv.style.left = xy.x + LEFT + xy.spanX + offset + 'px'; 
    autoCompleteDiv.style.top  = xy.y + TOP + xy.spanY + 'px';    
}                                                               
  

  
function setTxtAreaCursorXY(txtArea){
    var xy = getCursorXY(txtArea, txtArea.selectionStart);
    var mycursor = getElem('fakeCursorId');
    const LEFT = -8;
    const TOP = -7;

    mycursor.style.left = xy.x + LEFT + xy.spanX + 'px';
    mycursor.style.top  = xy.y + TOP + xy.spanY + 'px';

    var autoCompleteDiv = getElem('autoCompleteDiv');
    autoCompleteDiv.style.left = xy.x + LEFT + xy.spanX + 'px';
    autoCompleteDiv.style.top  = xy.y + TOP + xy.spanY + 'px';
}

// element
function getCurrRow(txtArea){
    var str = txtArea.value;
    var arr = str.split('\n');

    var start = txtArea.selectionStart;
    var end = txtArea.selectionEnd;

    var sum = 0;
    var nrow = 0;
    var offset = -1;
    for(var i=0; i<arr.length; i++){
        if((sum + arr[i].length) < end){
           sum += (arr[i].length + 1);
           nrow++;
        }else if((sum + arr[i].length) >= end){
           offset = end - sum;
           break;
        } 
    }

    return {
        nrow : nrow,
        offset : offset,
        lines : arr
    };
}

  function donothing(event){
      if(event.keyCode == KEY.DOWN){
	  if(getElem('autoCompleteDiv').style.display == 'block'){
	      getElem('autoCompleteDiv').focus();
	      return autoCompleteFun(event);
	  }
	  return false;
      }
  }

function buildLineMap(str){
    var map = new Map();
    var list = str.split(CONS.NL);
    for(var i=0; i<list.length; i++){
        console.log('s=' + list[i]);
        var cssLine = parse(list[i]);
        var lineTxtCSS = new LineTxtCSS(list[i], cssLine);
        map.set(i, lineTxtCSS); 
    }
    return map;
}


function parseMap(map, nrow){
    var lineObj = map.get(nrow);
    var cssLine = parse(lineObj.cssText);
    // Update the global map, {"", new line, new css line}
    var lineTxtCSS = new LineTxtCSS(map.lines[nrow], cssLine);
    map.set(nrow, lineTxtCSS); 
    var retStr = '';
    for(var i=0; i<map.size; i++){
        if(i != nrow){
            retStr += map.get(i).cssText + CONS.HTMLNL; 
        }else{
            retStr += cssLine + CONS.HTMLNL; 
        }
    }
    return retStr;
}

  function hasFocus(elemId){
      var txtArea = getElem(elemId);
      return document.activeElement == txtArea && document.hasFocus();
  }

function onloadHighLight(elementId){                                        
    var divData = document.getElementById(elementId);                       
    g_numToLineMap = buildLineMap(divData.value);                           
    var outputStr = '';                                                     
    for(var i=0; i < g_numToLineMap.size; i++){                             
        outputStr += g_numToLineMap.get(i).cssText + '<br>';                
    }                                                                       
    var renderId = document.getElementById(ID.RENDER);                      
    renderId.innerHTML = outputStr;                                         
    var txtArea = getElem(ID.TEXTAREA);                                     
    setTxtAreaCursorXY(txtArea);                                            
    hideById('autoCompleteDiv');                                            
    txtArea.focus();                                                        
}                                                                           



  function keyPressHighlight(event){
    // Get the TextArea value
    var txtArea = document.getElementById(ID.TEXTAREA);
    // TextArea value might be updated, e.g. autocomplete
    // getCurrWord(txtArea); => autoComplete
    setTxtAreaCursorXY(txtArea);

    var triple = getCurrRow(txtArea);
    var nrow = triple.nrow;
    var currLine = triple.lines[nrow]; 
    var retStr = '';
    var results = document.getElementById(ID.RENDER);
    console.log('nrow=' + triple.nrow + ' offset=' + triple.offset + ' lines=' + triple.lines);
      if(isPrintableKey(event.keyCode)){
        // autoComplete(ID.TEXTAREA);
        var cssLine = parse(currLine);
        // Assume the nrow line is modified for now.
        // Assume the numbe of lines is not changed.
        // Assume ENTER is not pressed.
        for(var i=0; i < g_numToLineMap.size; i++){
            if(i != nrow){
                retStr += g_numToLineMap.get(i).cssText + CONS.HTMLNL; 
            }else{
                // Update the global map
                retStr += cssLine + CONS.HTMLNL; 
                var lineTxtCSS = new LineTxtCSS(currLine, cssLine);
                g_numToLineMap.set(nrow, lineTxtCSS); 
            }
        }
	setTxtAreaCursorXY(getElem(ID.TEXTAREA));
        results.innerHTML = retStr; 
    } else if(event.keyCode == KEY.ENTER || event.keyCode == KEY.DELETE){
        // update the global map here
        var txtArea = document.getElementById(ID.TEXTAREA);
        g_numToLineMap = buildLineMap(txtArea.value);
        results.innerHTML = parse(txtArea.value);
	setTxtAreaCursorXY(getElem(ID.TEXTAREA));
    }else if(event.keyCode == KEY.UP || event.keyCode == KEY.DOWN || event.keyCode == KEY.LEFT){
    }
}

function parse(str){
    var retStr = '';
    for(var i=0; i<str.length; i++) {
        if(str[i] == '{' || str[i] == '}') {
            retStr += colorx(str[i], 'red');
        } else if(str[i] == '[' || str[i] == ']') {
            retStr += colorx(str[i], 'pink');
        } else if(str[i] == '(' || str[i] == ')') {
            retStr += colorx(str[i], 'green');
        } else if(str[i] == CONS.NL){
            retStr += '<br>'
        } else if(str[i] == '<'){
            retStr +='&lt;'
        } else if(str[i] == '>'){
            retStr += '&gt;'
        }else if(str[i] == '"'){
            var tokenStr = '"';
            var j = i+1;
            for(j=i+1; j<str.length; j++){
                if(str[j] != '"'){
                    tokenStr += str[j]; 
                }else{
                    tokenStr += str[j];
                    retStr += colorx(str.substr(i, (j-i)+1), 'red');
                    i = j;
                    break;
                }
            }
        }else{
            retStr += str[i]; 
        }
    }
    return retStr;
}

function parseStr(str, inx){
    let ret = '';
    let retInx = -1;
    if(str[inx] == '"'){
	for(var i=inx + 1; i<str.length; i++){
	    if(str[i] != '"'){
		retInx = i;
		break;
	    }else{
		ret += str[i]; 
	    }
	}
    }
    if(retInx > -1){
	return {
	    str : ret,
	    i : retInx
	}
    }else{
	return {
	    str : undefined,
	    i : retInx
	}
    }
}
  function parse2(str){
    var listPair = [];
    var retStr = '';                                                     
    for(var i=0; i<str.length; i++) {                                      
        if(str[i] == '{' || str[i] == '}') {
            listPair.push([i, i]);
            retStr += colorx(str[i], 'red');                             
        } else if(str[i] == '[' || str[i] == ']') {
            listPair.push([i, i]);
            retStr += colorx(str[i], 'pink');                            
        } else if(str[i] == '(' || str[i] == ')') {
            listPair.push([i, i]);
            retStr += colorx(str[i], 'green');                           
        } else if(str[i] == CONS.NL){                                    
            retStr += '<br>'                                               
        } else if(str[i] == '<'){                                        
            retStr +='&lt;'                                              
        } else if(str[i] == '>'){                                          
            retStr += '&gt;'                                             
        }else if(str[i] == '"'){                                         
            var tokenStr = '"';                                          
            var j = i+1;                                                 
            for(j=i+1; j<str.length; j++){                                 
                if(str[j] != '"'){                                       
                    tokenStr += str[j];                                    
                }else{                                                   
                    tokenStr += str[j];                                  
                    retStr += colorx(str.substr(i, (j-i)+1), 'red');     
                    listPair.push([i, j]);
                    i = j;
		    // string: ["dogcat"]
                    break;                                                 
                }                                                        
            }                                                            
        }else{                                                           
            retStr += str[i];                                            
        }                                                                
    }                                                                      
    return listPair;                                                       
}                                                                        

  


function submitFunc() {
    // Sending and receiving data in JSON format using POST method
    //
    console.log('dog');
    let xhr = new XMLHttpRequest();
    let url = "http://localhost:8080/compiler";
    xhr.open("POST", url, true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let json = JSON.parse(xhr.responseText);
            var results = document.getElementById("output");

            results.innerHTML = json['stdout']; 
        }
    };
    var obj = new Object();
    obj.compiler = 'cpp'
    obj.option= '-std=c++14'
    obj.code = document.getElementById(ID.TEXTAREA).value;
    let data = JSON.stringify(obj);
    console.log(data);
    xhr.send(data);
}

/**
* Get line content from div element
* 
* Ref: https://stackoverflow.com/questions/46253901/javascript-get-html-of-current-line-in-contenteditable-div
* 
*/
function getSelectionHtml() {
  var selection = window.document.selection,
    range, oldBrowser = true;

  if (!selection) {
    selection = window.getSelection();
    range = selection.getRangeAt(0);
    oldBrowser = false;
  } else
    range = document.selection.createRange();

  selection.modify("move", "backward", "lineboundary");
  selection.modify("extend", "forward", "lineboundary");

  if (oldBrowser) {
    var html = document.selection.createRange().htmlText;
    range.select();
    return html;
  }

  var html = document.createElement("div");

  for (var i = 0, len = selection.rangeCount; i < len; ++i) {
    html.appendChild(selection.getRangeAt(i).cloneContents());
  }

  selection.removeAllRanges();
  selection.addRange(range);
  return html.innerHTML;
}

/**
 * returns x, y coordinates for absolute positioning of a span within a given text input
 * at a given selection point
 * @param {object} input - the input element to obtain coordinates for
 * @param {number} selectionPoint - the selection point for the input
 * Ref: https://medium.com/@jh3y/how-to-where-s-the-caret-getting-the-xy-position-of-the-caret-a24ba372990a
 */
const getCursorXY = (input, selectionPoint) => {
  const {
    offsetLeft: inputX,
    offsetTop: inputY,
  } = input
  // create a dummy element that will be a clone of our input
  const div = document.createElement('div')
  // get the computed style of the input and clone it onto the dummy element
  const copyStyle = getComputedStyle(input)
  for (const prop of copyStyle) {
    div.style[prop] = copyStyle[prop]
  }
  // we need a character that will replace whitespace when filling our dummy element if it's a single line <input/>
  const swap = '.'
  const inputValue = input.tagName === 'INPUT' ? input.value.replace(/ /g, swap) : input.value
  // set the div content to that of the textarea up until selection
  const textContent = inputValue.substr(0, selectionPoint)
  // set the text content of the dummy element div
  div.textContent = textContent
  if (input.tagName === 'TEXTAREA') div.style.height = 'auto'
  // if a single line input then the div needs to be single line and not break out like a text area
  if (input.tagName === 'INPUT') div.style.width = 'auto'
  // create a marker element to obtain caret position
  const span = document.createElement('span')
  // give the span the textContent of remaining content so that the recreated dummy element is as close as possible
  span.textContent = inputValue.substr(selectionPoint) || '.'
  // append the span marker to the div
  div.appendChild(span)
  // append the dummy element to the body
  document.body.appendChild(div)
  // get the marker position, this is the caret position top and left relative to the input
  const { offsetLeft: spanX, offsetTop: spanY } = span
  // lastly, remove that dummy element
  // NOTE:: can comment this out for debugging purposes if you want to see where that span is rendered
  document.body.removeChild(div)
  // return an object with the x and y of the caret. account for input positioning so that you don't need to wrap the input
  return {
    spanX: spanX,
    spanY: spanY,
    x: inputX,
    y: inputY
  }
}

