// Load text from TextArea                                                
// Highlight the text and add to div ID.RENDERID                          
//
//   Use symbol link in your local java/html code
//   $b/jslib/aronlib.js
// 
//  test it
//   <head>
//   <script src="aronlib.js"></script>
//  </head>

// const myHost = 'http://localhost:8080'
const myHost = 'http://localhost:8081'

const HOST = {
    LOCALHOST : 'localhost:8081',
    XFIDO     : 'http://xfido.com:8080'
}

const RUNOS = {
    MACOS : 'Darwin',
    LINUX : 'Linux'
}

function test(){
    alert('/Users/cat/myfile/bitbucket/jslib/aronlib.js test() 1');
}

var delay = function(value){
    return function(){
        doneTyping(value)
    };
}

// const inputField = document.getElementById('inputid')



// WaiLib.hs
// get OS type from <div id='osid'>os_name</div>
function getHostName(){
    let osid = document.getElementById('osid')
    let hostName = ''
    let osName = osid.value

    if (osName && len(osName) > 0){
        if ( osName == RUNOS.MACOS){
            hostName = HOST.LOCALHOST 
        }else if( osname == RUNOS.LINUX){
            hostName = HOST.XFIDO
        }else{
            console.log('ERROR: UNKNOWN OS Type:' + osName)
        }
    }else{
        if(osName != null){
            console.log('ERROR: osName is null')
        }else{
            console.log('ERROR: osName=[' + osName + ']')
        }
    }
    return hostName 
}

const delayFun = (value) => {
    console.log('delayFun[' + value + ']');
    if(len(value) > 2){
        let xhr = new XMLHttpRequest();
	// let url = 'http://localhost:8080/snippet?id=' + value;
        let url =  getHostName() + '/snippet?id=' + value;
        console.log(url);
        xhr.withCredentials = true;
        xhr.open('GET', url, true);
        // xhr.setRequestHeader("Content-Type", "application/json");
        xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            document.getElementById('searchdata').innerHTML = xhr.responseText;
          }
        };
        // document.getElementById('inputid').value = value;
        xhr.send();
    }
}; 

// wait ms (milliseconds) after user stops typing to execute func
const delayKeyUp = (() => {
    let timer = null;
    const delay = (func, ms) => {
        timer ? clearTimeout(timer): null
        timer = setTimeout(func, ms)
    }
    return delay
})();


window.onload = function(){
    const inputField = document.getElementById('inputid')
    if(inputField != null){
        inputField.addEventListener('keyup', (e) => {
            const query = e.target.value;
            console.log('query=' + query);
            delayKeyUp(() => {delayFun(query)}, 200);
        })
    }else{
	console.log('ERROR aronlib.js => window.onload ' + 'inputField is null');
    }
}


function doneTyping(value){
    console.log(value);

    let xhr = new XMLHttpRequest();
    // let url = 'http://localhost:8080/snippet?id=' + value;
    // let url = 'http://xfido.com:80/snippet?id=' + value;

    let url = getHostName() + '/snippet?id=' + value
    console.log(url);
    xhr.withCredentials = true;
    xhr.open('GET', url, true);
    // xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function () {
	if (xhr.readyState === 4 && xhr.status === 200) {
	    console.log(xhr.responseText)
	    document.getElementById('searchdata').innerHTML = xhr.responseText;
	}
    };
    // document.getElementById('inputid').value = value;
    xhr.send();
}

// reload css without reloading page
// https://stackoverflow.com/questions/2024486/is-there-an-easy-way-to-reload-css-without-reloading-the-page
function reloadCSS(){
    console.log('reloadCSS');
    var links = document.getElementsByTagName("link");
    for (var cl in links){
        var link = links[cl];
        if (link.rel === "stylesheet")
            link.href += "";
    }
}

function doneTypingReload(value){
    console.log(value);

    let xhr = new XMLHttpRequest();
    // let url = 'http://localhost:8080/snippet?id=' + value;
    // let url = 'http://xfido.com:80/snippet?id=' + value;
    let url = getHostName() + '/snippet?id=' + value
    console.log(url);
    xhr.withCredentials = true;
    xhr.open('GET', url, true);
    // xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function () {
	if (xhr.readyState === 4 && xhr.status === 200) {
	    document.getElementById('searchdata').innerHTML = xhr.responseText;
	}
    };
    // document.getElementById('inputid').value = value;
    xhr.send();
}

function testChange(value){
    console.log('testChange=' + value);
    if (len(value) > 2){
        // document.getElementById('inputid').value = value;
        let typingTimer;
        let typingInterval = 1000;
        let myInput = document.getElementById('inputid');

        // fun('event', () => {abc});
        // event call lambda function  ()
        clearTimeout(typingTimer);
        if(myInput.value){
            typingTimer = setTimeout(delay(value), typingInterval);
        }
    }



//        let xhr = new XMLHttpRequest();
//        let url = 'http://localhost:8080/snippet?id=' + value;
//        console.log(url);
//        xhr.open('GET', url, true);
//        // xhr.setRequestHeader("Content-Type", "application/json");
//        xhr.onreadystatechange = function () {
//        if (xhr.readyState === 4 && xhr.status === 200) {
//            // location.reload();
//            // document.write(xhr.responseText);
//            // append data inside <div id='searchdata'></div> in WaiLib.hs htmlBody::String -> String
//            document.getElementById('searchdata').innerHTML = xhr.responseText;
//            // document.getElementById('searchdata').innerHTML = '<div>XMLHttpRequest(), construct header for get() xhr.send() from testChange()</div>'; 
//
//           // alert(xhr.responseText);
//            // let json = JSON.parse(xhr.responseText);
//                // var results = document.getElementById("myDiv");//myDiv is the div id
//                // results.innerHTML = geneTable(json['matrix']); 
//                // console.log(json['responsecmd']);
//              }
//        };
//        document.getElementById('inputid').value = value;
//        
//        xhr.send();
//        // alert('end send');
//    }
}

function getTag(tag){
    var arrTag = document.getElementsByTagName(tag);
    return arrTag;
}

function changeColor(s){
    document.body.style.backgroundColor = s;
    // alert('color' + s);
}



// KEY: Change background color, modify background color, change color, modifycolor.css
// See: modifycolor.css
// /Users/cat/myfile/bitbucket/haskellwebapp2/src/WaiLib.hs  geneRectMat::Application
// updateBackground
//
//
// See updateBackground::Application => WaiLib.hs
// See searchForm => WaiLib.hs
// <input id='bgidcolor' type="color" onchange="submitUpdateBackground(this.value)" name="colorname"
function submitUpdateBackground(cvalue) {
    // Sending and receiving data in JSON format using POST method
    // See geneRectMat::Application in WaiLib.hs
    let keyValue = cvalue
    let xhr = new XMLHttpRequest();
    // let url = "http://localhost:8080/updatebackground";
    // let url = "http://xfido.com:80/updatebackground";
    let url = getHostName() + '/snippet?id=' + cvalue
    xhr.withCredentials = true;
    xhr.open("POST", url, true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            // let json = JSON.parse(xhr.responseText);
            console.log(xhr.responseText);
	    console.log('get response from Server');
	    // window.location.href = 'http://localhost:8080/snippet?id=s+list'
	    
        const inputField = document.getElementById('inputid');
	    // console.log('inputField.value=' + inputField.value);
	    // doneTypingReload(inputField.value);
	    // let url = 'http://localhost:8080/snippet?id=' + value;
	    // location.reload();
            // var results = document.getElementById("myDiv");//myDiv is the div id
            // results.innerHTML = geneTable(json['matrix']); 
            // console.log(json['responsecmd']);
	}
    };

    /*
      <div style="background-color:green;">
      <input id="mname" name="cmd" value="genematrix"></input>
      <input id="mncol" name="ncol" value="2"></input>
      <input id="mnrow" name="nrow" value="4"></input>
      </div>

     Json object obj:
     '{
         "cmd" : "mycmd",
         "ncol" : 2, 
         "nrow" : 3 
      }'
      
      grap the value of 'colorname' from Input field.
      <div>
      <input id='bgidcolor' type="color" onchange="changeColor(this.value)" id="colorid" name="colorname"
      value="#e66465">
      <label for="body_background">Background</label>
      </div>

      Json
      '{"colorname" : "#e66465" }'

    */
    var obj = new Object();
    // obj.colorname = document.getElementById('bgidcolor').value;
    obj.colorname = keyValue;
    // document.body.style.pre.background = cvalue;
    console.log('obj.colorname=' + obj.colorname);
    let data = JSON.stringify(obj);
    xhr.send(data);
    var pres = getTag('pre');
    for(var i = 0; i < pres.length; i++){
	//pres[i].innerHTML = 'document.getElementsByTagName() = ' + i;
	// alert(pres[i].innerHTML);
	pres[i].style.backgroundColor = cvalue;
    }
}

// Update text color and background color in bgidcolor and textcolorInput
// See src/aronlib.js
// See getPreFromRedis::Application in WaiLib.hs
function requestPreFromRedis() {
    let xhr = new XMLHttpRequest();
    // let url = "http://localhost:8080/getcolor";
    // let url = "http://xfido.com:80/getcolor";
    let url = getHostName() + '/getcolor'
    xhr.withCredentials = true;
    xhr.open("POST", url, true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let json = JSON.parse(xhr.responseText);
            console.log(xhr.responseText);
	    console.log('get response from Server');
	    // window.location.href = 'http://localhost:8080/snippet?id=s+list'
	    
            const bgcolorInput = document.getElementById('bgidcolor');
	    const textcolorInput = document.getElementById('textcolorid');
	    textcolorInput.value = json['color'];
            bgcolorInput.value = json['background'];
	}
    };
    
    var obj = new Object();
    // obj.colorname = document.getElementById('bgidcolor').value;
    obj.color = "color"
    obj.background = "background"
    // document.body.style.pre.background = cvalue;
    let data = JSON.stringify(obj);
    xhr.send(data);
}



/*
WaiLib.hs
JSON => 
data UpCodeBlock = UpCodeBlock{ok::String, retcmd::String, retbegt::Integer, retendt::Integer} deriving (Generic, Show)
*/
function updateCodeBlock(pid){
    let xhr = new XMLHttpRequest();
    let url = myHost + '/' + 'updatecode';
    xhr.withCredentials = true;
    xhr.open("POST", url, true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let json = JSON.parse(xhr.responseText);
            console.log(xhr.responseText);
	    json.retendt = Date.now()
	    console.log('updateCodeBlock: get response from Server');
            console.log(xhr.responseText)
	    console.log('json=' + json)
	    console.log('--')
	    console.log('json.ok=' + json.ok);
	    console.log('json.retcmd=' + json.retcmd);
	    console.log('json.retbegt=' + json.retbegt);
	    console.log('json.retendt=' + json.retendt);
	    console.log('diff =' + (json.retendt - json.retbegt));
            // window.location.href = 'http://localhost:8080/snippet?id=s+list'
        }
    };

    // pid is primary key in CodeBlock table
    // pid is also a Record UpdateCodeBlock
    // data UpdateCodeBlock = UpdateCodeBlock{pid::Integer, newcode::String} deriving (Generic, Show)
    // See: WaiLib.hs
    // in Client side,
    // <textarea id="t294" name="myblock" spellcheck="false" autofocus="true" onfocus="textAreaAdjust(this);"  class="hide"> updated code block</textarea>
    // id = t + '294' = t + pid = t + 'Primary key'
    var obj = new Object();
    const txtId = 't' + pid;
    const newcode = document.getElementById(txtId).value;
    obj.pid = strToInt(pid)
    obj.begt = Date.now()
    obj.endt = 0;
    obj.newcode = newcode 
    let data = JSON.stringify(obj);
    xhr.send(data);
}

/*
WaiLib.hs
JSON => 
data UpCodeBlock = UpCodeBlock{ok::String, retcmd::String, retbegt::Integer, retendt::Integer} deriving (Generic, Show)
*/
function deleteCodeBlock(pid){
    let xhr = new XMLHttpRequest()
    let url = myHost + '/' + 'deletecode'
    xhr.withCredentials = true
    xhr.open("POST", url, true)
    xhr.setRequestHeader("Content-Type", "application/json")
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let json = JSON.parse(xhr.responseText)
            console.log(xhr.responseText)
	    json.retendt = Date.now()
	    console.log('deleteCodeBlock: get response from Server');
            console.log(xhr.responseText)
	    console.log('json=' + json)
	    console.log('--')
	    console.log('json.ok=' + json.ok);
	    console.log('json.retcmd=' + json.retcmd);
	    console.log('json.retbegt=' + json.retbegt);
	    console.log('json.retendt=' + json.retendt);
	    console.log('diff =' + (json.retendt - json.retbegt));
	    
            // window.location.href = 'http://localhost:8080/snippet?id=s+list'
        }
    }

    // pid is primary key in CodeBlock table
    // pid is also a Record UpdateCodeBlock
    // data UpdateCodeBlock = UpdateCodeBlock{pid::Integer, newcode::String} deriving (Generic, Show)
    // See: WaiLib.hs
    // in Client side,
    // <textarea id="t294" name="myblock" spellcheck="false" autofocus="true" onfocus="textAreaAdjust(this);"  class="hide"> updated code block</textarea>
    // id = t + '294' = t + pid = t + 'Primary key'
    var obj = new Object()
    const txtId = 't' + pid
    const newcode = document.getElementById(txtId).value
    obj.pid = strToInt(pid)
    obj.begt = Date.now()
    obj.endt = 0
    obj.newcode = ''
    let data = JSON.stringify(obj)
    xhr.send(data)
}

/*
  :NOTE: Map to following Haskell Record
  WaiLib.hs
  JSON => 
  data UpCodeBlock = UpCodeBlock{ok::String, retcmd::String, retbegt::Integer, retendt::Integer} deriving (Generic, Show)
*/
function insertCodeBlock_xxx(pid){
    let xhr = new XMLHttpRequest()
    let url = myHost + '/' + 'insertcode'
    xhr.withCredentials = true
    xhr.open("POST", url, true)
    xhr.setRequestHeader("Content-Type", "application/json")
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let json = JSON.parse(xhr.responseText)
	    json.retendt = Date.now()
	    console.log('insertCodeBlock: get response from Server');
            console.log(xhr.responseText)
	    console.log('json=' + json)
	    console.log('--')
	    console.log('json.ok=' + json.ok);
	    console.log('json.retcmd=' + json.retcmd);
	    console.log('json.retbegt=' + json.retbegt);
	    console.log('json.retendt=' + json.retendt);
	    console.log('diff =' + (json.retendt - json.retbegt));
            // window.location.href = 'http://localhost:8080/snippet?id=s+list'
        }
    }

    // pid is primary key in CodeBlock table
    // pid is also a Record UpdateCodeBlock
    // data UpdateCodeBlock = UpdateCodeBlock{pid::Integer, newcode::String} deriving (Generic, Show)
    // See: WaiLib.hs
    // in Client side,
    // <textarea id="t294" name="myblock" spellcheck="false" autofocus="true" onfocus="textAreaAdjust(this);"  class="hide"> updated code block</textarea>
    // id = t + '294' = t + pid = t + 'Primary key'
    var obj = new Object()
    const txtId = 't' + pid
    const newcode = document.getElementById(txtId).value
    obj.pid = strToInt(pid)
    obj.begt = Date.now();
    obj.endt = 0
    obj.newcode = newcode 
    let data = JSON.stringify(obj)
    xhr.send(data)
}

function insertCodeBlock2(newcode){
    let xhr = new XMLHttpRequest()
    let url = myHost + '/' + 'insertcode'
    xhr.withCredentials = true
    xhr.open("POST", url, true)
    xhr.setRequestHeader("Content-Type", "application/json")
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let json = JSON.parse(xhr.responseText)
	    json.retendt = Date.now()
	    console.log('insertCodeBlock: get response from Server');
            console.log(xhr.responseText)
	    console.log('json=' + json)
	    console.log('--')
	    console.log('json.ok=' + json.ok);
	    console.log('json.retcmd=' + json.retcmd);
	    console.log('json.retbegt=' + json.retbegt);
	    console.log('json.retendt=' + json.retendt);
	    console.log('diff =' + (json.retendt - json.retbegt));
        }
    }

    // pid is primary key in CodeBlock table
    // pid is also a Record UpdateCodeBlock
    // data UpdateCodeBlock = UpdateCodeBlock{pid::Integer, newcode::String} deriving (Generic, Show)
    // See: WaiLib.hs
    // in Client side,
    // <textarea id="t294" name="myblock" spellcheck="false" autofocus="true" onfocus="textAreaAdjust(this);"  class="hide"> updated code block</textarea>
    // id = t + '294' = t + pid = t + 'Primary key'
    var obj = new Object()
    obj.pid = 0
    obj.begt = Date.now();
    obj.endt = 0
    obj.newcode = newcode 
    let data = JSON.stringify(obj)
    xhr.send(data)
}

function insertCodeBlock22(pid){
    const txtId = 't' + pid
    const newcode = document.getElementById(txtId).value
    insertCodeBlock2(newcode)
}


function postRequestJson(pid, url, obj){
    let xhr = new XMLHttpRequest()
    xhr.withCredentials = true
    xhr.open("POST", url, true)
    xhr.setRequestHeader("Content-Type", "application/json")
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let json = JSON.parse(xhr.responseText)
            console.log(xhr.responseText)
            console.log('get response from Server')
            // window.location.href = 'http://localhost:8080/snippet?id=s+list'
        }
    }
    let data = JSON.stringify(obj)
    xhr.send(data)
}

function postEditorData(str){
    let url = myHost + '/' + 'editordata'
    var obj = new Object()
    obj.editorcmd = "post Editor Data"
    obj.editorcode = str
    // NO stringify here
    postRequestJson(0, url, obj);
}

function insertEmbeddedPDF(){

    /*
    var divStr = `<div style="width:400px; height:100px;">
                  <embed src="latex.pdf" type="application/pdf" width="400px" height="400px" />
                  </div>
                 `;
    */
    // var divStr = `<iframe src="latex.pdf" style="width:400px; height:400px;" frameborder="0"></iframe>`;
    var divStr = `<canvas id="the-canvas"></canvas>`;
    document.getElementsByTagName('body')[0].innerHTML += divStr;
    loadPDF('./latex.pdf');
}

function submitUpdateTextColor(cvalue) {
    // Sending and receiving data in JSON format using POST method
    // See geneRectMat::Application in WaiLib.hs
    let keyValue = "color:" + cvalue;  // color:#333333
    let xhr = new XMLHttpRequest();
    // let url = "http://localhost:8080/updatetextcolor";
    // let url = "http://xfido.com:80/updatetextcolor";
    let url = getHostName() + '/snippet?id=' + value
    xhr.withCredentials = true;
    xhr.open("POST", url, true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            console.log(xhr.responseText);
	}
    };
    
    var obj = new Object();
    obj.textcolor = keyValue;  // color:#333333
    console.log('obj.textcolor=' + obj.textcolor);
    let data = JSON.stringify(obj);
    xhr.send(data);
    var pres = getTag('pre');
    for(var i = 0; i < pres.length; i++){
	pres[i].style.color = cvalue;  // #333333
    }
}

var clip_kk = function(el) {
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
  alert('from clip()');

  try{
      document.execCommand('copy');
  }catch(err){
      alert('Please press Ctrl/Cmd+C to copy');
  }
}


const copyToClipboard = str => {
  var el = document.createElement('textarea');  // Create a <textarea> element
  el.setAttribute('readonly', '');                // Make it readonly to be tamper-proof
  el.style.position = 'absolute';                 
  el.style.left = '-9999px';                      // Move outside the screen to make it invisible

  alert('str=' + str);

  el.value = str;                                 // Set its value to the string that you want copied

  document.body.appendChild(el);                  // Append the <textarea> element to the HTML document
  el.focus();
  el.select();                                    // Select the <textarea> content

  // const selected =            
    // document.getSelection().rangeCount > 0        // Check if there is any content selected previously
      // ? document.getSelection().getRangeAt(0)     // Store selection if found
      // : false;                                    // Mark as false to know no selection existed before

  // if(document.execCommand('copy')){                   // Copy - only works as a result of a user action (e.g. click events)
  // }else{
    // alert('sth wrong with copy');
  // }

  try {
    var successful = document.execCommand('copy');
    var msg = successful ? 'successful' : 'unsuccessful';
    alert('Copying text command was ' + msg);
  } catch (err) {
    alert('Oops, unable to copy');
  }

  // document.body.removeChild(el);                  // Remove the <textarea> element
  // if (selected) {                                 // If a selection existed before copying
    // alert('el.value=' + str);
    // document.getSelection().removeAllRanges();    // Unselect everything on the HTML document
    // document.getSelection().addRange(selected);   // Restore the original selection
  // }else{
      // alert('does not select any thing');
  // }
  alert('1');
 
};

// Remove The first line, and keep the rest of line
// The function is used in The snippet search
// TestCode: file:///Users/cat/myfile/bitbucket/html/copyToClipBoard.html
//
var copyToClipboardFromTextArea = function(elementId){
    var textAreaData = document.getElementById(elementId).value;
    var newArr = stringToArray(textAreaData)
    alert('textAreaData=' + textAreaData);
    alert('newArr=' + newArr);
    alert('newArr.length=' + newArr.length);
    var codeStr = '';
    if(newArr.length > 0){
        var codeArr = tailList(newArr);
        codeStr = codeArr.join('\n'); 
    }else{
        alert('length should be len > 1, but len=' + newArr.length);
    }
    
    alert('codeStr=' + codeStr);
    // copyToClipboard(codeStr);
    copyTextToClipboard_2020Apri(codeStr);
}

// split string with newline '\n'
// trim all string, filter out zero length string
// return an array
function stringToArray(strs){
    var arr = strs.split('\n');
    var newArr = arr.filter(x => x.trim().length > 0)  
    return newArr;
}

function strToInt(s){
    return parseInt(s);
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

function takeList(n, list){
    return list.slice(0, n);
}

function dropList(n, list){
    return list.slice(n);
}
function headList(list){
    return list.slice(1, n);
}

function lastList(list){
    return list[list.length - 1]; 
}

function tailList(list){
    return dropList(1, list);
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


// NOT BEEN USED
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

function len(s){
    return s.length;
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
    // let url = "http://localhost:8080/compiler";
    // let url = "http://xfido.com:80/compiler";
    let url = getHostName() + '/snippet?id=' + value
    xhr.withCredentials = true;
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

     var butobj=document.getElementById('c' + id);
     var cname = 'co' + id;
     if(butobj.className == cname){  
	butobj.className = 'colorclick';
     }else if(butobj.className == 'colorclick'){
	butobj.className = cname; 
     }
    
}


// Test http://localhost:8080/matrix
// Send genematrix to WaiLib.hs
// WaiLib.hs call geneRectMat::Application
// geneRectMat::Application genrate matrix and serialize it
// Send back to client side
// json['matrix'] contains Javascript array  
// geenTable() create Html Matrix from json['matrix']
function submitMatrixInfo() {
    // Sending and receiving data in JSON format using POST method
    // See geneRectMat::Application in WaiLib.hs 
    let xhr = new XMLHttpRequest();
    // let url = "http://localhost:8080/json";
    // let url = "http://xfido.com:80/json";
    let url = getHostName() 
    console.log('url=' + url)
    xhr.withCredentials = true;
    xhr.open("POST", url, true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let json = JSON.parse(xhr.responseText);

	    // console.log(json.matrix);
	    // 'matrix' will be in 'myDiv'
            var results = document.getElementById("myDiv");//myDiv is the div id

	    // Json object 'matrix' from WaiLib.hs geneRectMat::Application
            results.innerHTML = geneTable(json['matrix']); 
        }
    };
    
    /*
      <div style="background-color:green;">
      <input id="mname" name="cmd" value="genematrix"></input>
      <input id="mncol" name="ncol" value="2"></input>
      <input id="mnrow" name="nrow" value="4"></input>
      </div>

     Json object obj:
     '{
         "cmd" : "mycmd",
         "ncol" : 2, 
         "nrow" : 3 
      }'
    */
    var obj = new Object();
    obj.cmd = document.getElementById('mname').value;
    console.log(obj.cmd);
    obj.ncol = parseInt(document.getElementById('mncol').value);
    console.log(obj.ncol);
    obj.nrow = parseInt(document.getElementById('mnrow').value);
    console.log(obj.nrow);
    let data = JSON.stringify(obj);
    xhr.send(data);
}

// build a html table here
function geneTable(arr2d){
    const sty = `style= 
                    "background-color:#DDDFFD;
                    font-size:20px;
                    ";
                `;
    var ret = '';
        ret += '<table ' + sty + '>';
        for(var i = 0; i < arr2d.length; i++){
            console.log(arr2d[i].length);
            ret += '<tr>';
            for(var j = 0; j < arr2d[i].length; j++){
                console.log(arr2d[i][j]);
                ret += "<td contenteditable='true'>";
                ret += arr2d[i][j];
                ret += '</td>';
            }
            ret += '</tr>';
        }
        ret += '</table>';
    return ret;
}

function geneHTMLTable(arr){
    var ret = '';
    for(var i = 0; i < arr.length; i++){
	ret += arr[i] + '\n';
    }
    return ret;
}

function escapeHtml(unsafe) {
    return unsafe
         .replace(/&/g, "&amp;")
         .replace(/</g, "&lt;")
         .replace(/>/g, "&gt;")
         .replace(/"/g, "&quot;")
         .replace(/'/g, "&#039;");
}

function submitHTMLTableInfo() {
    // Sending and receiving data in JSON format using POST method
    // See geneRectMat::Application in WaiLib.hs
    let xhr = new XMLHttpRequest();
    // let url = "http://localhost:8080/htmltable";
    // let url = "http://xfido.com:80/htmltable";

    let url = getHostName() + '/snippet?id=' + value
    xhr.withCredentials = true;
    xhr.open("POST", url, true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function () {
        if (xhr.readyState === 4 && xhr.status === 200) {
            let json = JSON.parse(xhr.responseText);

	    // console.log(json.matrix);
	    // 'matrix' will be in 'htmltablediv'
            var divResult = document.getElementById("htmltablediv"); // htmltablediv is the div id
	    var preResult = document.getElementById("preid"); // htmltablediv is the div id
	    console.log(json);
	    console.log('json=' + json);
	    console.log('json[matrix]=' + json['matrix']);
	    console.log(json['matrix']);
	    // Json object 'matrix' from WaiLib.hs geneRectMat::Application
            divResult.innerHTML = geneHTMLTable(json['matrix']);

	    preResult.innerHTML = geneHTMLTable( json['matrix'].map(x => escapeHtml(x)) ); 
        }
    };
    
    /*
      <div style="background-color:green;">
      <input id="mname" name="cmd" value="genematrix"></input>
      <input id="mncol" name="ncol" value="2"></input>
      <input id="mnrow" name="nrow" value="4"></input>
      </div>

     Json object obj:
     '{
         "cmd" : "mycmd",
         "ncol" : 2, 
         "nrow" : 3 
      }'
    */
    var obj = new Object();
    obj.cmd = document.getElementById('mname').value;
    console.log(obj.cmd);
    obj.ncol = parseInt(document.getElementById('mncol').value);
    console.log(obj.ncol);
    obj.nrow = parseInt(document.getElementById('mnrow').value);
    console.log(obj.nrow);
    let data = JSON.stringify(obj);
    xhr.send(data);
}


function copyTextToClipboard_2020Apri(text) {
  var textArea = document.createElement("textarea");

  //
  // *** This styling is an extra step which is likely not required. ***
  //
  // Why is it here? To ensure:
  // 1. the element is able to have focus and selection.
  // 2. if element was to flash render it has minimal visual impact.
  // 3. less flakyness with selection and copying which **might** occur if
  //    the textarea element is not visible.
  //
  // The likelihood is the element won't even render, not even a
  // flash, so some of these are just precautions. However in
  // Internet Explorer the element is visible whilst the popup
  // box asking the user for permission for the web page to
  // copy to the clipboard.
  //

  // Place in top-left corner of screen regardless of scroll position.
  textArea.style.position = 'fixed';
  textArea.style.top = 10;
  textArea.style.left = 10;

  // Ensure it has a small width and height. Setting to 1px / 1em
  // doesn't work as this gives a negative w/h on some browsers.
  textArea.style.width = '10em';
  textArea.style.height = '10em';

  // We don't need padding, reducing the size if it does flash render.
  textArea.style.padding = 0;

  // Clean up any borders.
  textArea.style.border = 'none';
  textArea.style.outline = 'none';
  textArea.style.boxShadow = 'none';

  // Avoid flash of white box if rendered for any reason.
  // textArea.style.background = 'transparent';


  textArea.value = text;

  document.body.appendChild(textArea);
  textArea.focus();
  textArea.select();
  alert('value=' + textArea.value);

  try {
    var successful = document.execCommand('copy');
    var msg = successful ? 'successful' : 'unsuccessful';
    alert('Copying text command was ' + msg);
  } catch (err) {
    alert('Oops, unable to copy' + err);
  }

  document.body.removeChild(textArea);
}


function loadPDF(pdfName){
    var url = 'https://raw.githubusercontent.com/mozilla/pdf.js/ba2edeae/examples/learning/helloworld.pdf';

    // Loaded via <script> tag, create shortcut to access PDF.js exports.
    var pdfjsLib = window['pdfjs-dist/build/pdf'];

    // The workerSrc property shall be specified.
    pdfjsLib.GlobalWorkerOptions.workerSrc = 'https://mozilla.github.io/pdf.js/build/pdf.worker.js';

    // Asynchronous download of PDF
    // var loadingTask = pdfjsLib.getDocument(url);
    var loadingTask = pdfjsLib.getDocument(pdfName);
    loadingTask.promise.then(function(pdf) {
	console.log('PDF loaded');
	
	// Fetch the first page
	var pageNumber = 1;
	pdf.getPage(pageNumber).then(function(page) {
	    console.log('Page loaded');
	    
	    var scale = 1.5;
	    var viewport = page.getViewport({scale: scale});

	    // Prepare canvas using PDF page dimensions
	    var canvas = document.getElementById('the-canvas');
	    var context = canvas.getContext('2d');
	    canvas.height = viewport.height;
	    canvas.width = viewport.width;

	    // Render PDF page into canvas context
	    var renderContext = {
		canvasContext: context,
		viewport: viewport
	    };
	    var renderTask = page.render(renderContext);
	    renderTask.promise.then(function () {
		console.log('Page rendered');
	    });
	});
    }, function (reason) {
	// PDF loading error
	console.error(reason);
    });
}




