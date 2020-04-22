{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- empty the map (ref HMap) 
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HtmlForm where

import Text.RawString.QQ (r)         -- Need QuasiQuotes too 

      
{-| 
    === user input autocomplete

    * search field, search input, search form

    <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/color change background color Javascript>

    * search form has moved to htmlBody.htmlbs


    submitUpdateBackground => aronlib.js
-} 
searchForm::String -> String  -- USE IT NOW
searchForm s = [r| 
             <div   style="text-align:center;">
             <form action="/snippet" method="get" target=""> 
             <input type="text" style="font-size:18pt;height:50px;width:400px;" id="inputid" value="s van" onchange="testChange(this.value);" name="id" list="autocomplete">  
                 <datalist id="autocomplete">" |] <> s <> [r| </datalist><br>  
             </form> 
             </div> 
                <div>
                
                <input id='bgidcolor' type="color" onchange="submitUpdateBackground('background:' + this.value)" name="colorname"
                value="#e66465">
                <label for="body_background">Background</label>
                
                <input id='textcolorid' type="color" onchange="submitUpdateBackground('color:' + this.value)" name="textcolorname"
                value="#e66465">
                <label for="text_color">TextColor</label>
                <svg width="20" height="20">
                  <rect width="20" height="20" style="fill:rgb(10,10,255);stroke-width:3;stroke:rgb(0,0,0)" />
                </svg>
                
                <svg width="20" height="20">
                  <rect width="20" height="20" style="fill:rgb(0,33,55);stroke-width:3;stroke:rgb(0,9,20)" />
                </svg>
                </div>
             |]
