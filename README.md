# This is Haskell Web Server Application
## The app dependents on many libraries
* Wai - low level Web service API
* AESON - Serialize and deserialize Json data
* Redis - Key and Value data store. NoSql
* Sqlite3 - file-based Relational Data Engine, it is similar to MySql but less functionarities.
* more

# What the app can do?
* Submit data to Server, and store data in database or local file.
* Use websocket to send Json
* Use Redis to query data from Redis server

# Remove Neat interpolation code from code because it is very slow. (12-11-2019)
* Keep the Raw String QQ
# Fix javascript copy block code to clipboard with id in TextArea which is not working, use id in pre instread (Wed Nov 13 14:52:12 2019)
* Not sure why id inside the textarea is not working.
* [commit here](https://bitbucket.org/zsurface/haskellwebapp2/commits/5bea84536cdafec8bbe9188339c02b94611cf5ee#Lsrc/WaiLib.hsT533)
* Remove some Javascript alert() code from clip function.

# Add Latex editor 
* Use Javascript ACE9 
* Use different color theme
* Support Languages: Latex, Haskell and Javascript

# Copy haskellwebapp2 to $HOME/myfile/mybin (Thu 25 Feb 23:18:43 2021)
* NOTE: Change/Rename hakellwebapp2_test to haskellwebapp2 in haskellwebapp2.cabal when the project is copy to $HOME/myfile/mybin
* NOTE: Modify config.txt file to use different hostname and port number
* NOTE: Rename HOSTNAME to different host name and port number in $b/jslib/aronlib.js
* NOTE: Goto $ff/mybin/haskellwebapp2Bin  and run `stack build haskellwebapp2 && stack exec haskellwebapp2`

* Install project: `install.sh in`

## Add function to find the full path of binary like the following

``` bash
which pdflatex
```

## Add code to padding CodeBlock header (Thursday, 21 October 2021 12:47 PDT)
* If the following format is found, no padding

``` bash
	dot:cat:pig
	line 1
```
* If other format, then padding the header

``` bash
    dog cat
	line 1
	
	# Padding with random number 314
	314:*:dog cat
	line 1
```

## Change CodeBlock table to the following (Sunday, 31 October 2021 13:37 PDT)
### Old CodeBlock schema
``` sql
CREATE TABLE CodeBlock (
    id INTEGER PRIMARY KEY AUTOINCREMENT, 
    header TEXT, 
    codeblock TEXT
);
```
### New CodeBlock schema

``` sql
CREATE TABLE CodeBlock (
    id INTEGER PRIMARY KEY AUTOINCREMENT, 
    header TEXT, 
    codeblock TEXT, 
    addedtime DATETIME DEFAULT (stftime('%s', 'now')), 
    score INTEGER DEFAULT 0
);
```
* **score** can be **NEGATIVE**

### Add code to sort score column according to *score* (from table **CodeBlock**)
* Currently, we sort score in reverse order. In other words, the highest score will be on top.
* The code can be changed to sort according addedtime

### Background color and Font color are store in Redis
``` sql
    redis-cli
	keys 'HTMLPre.color'
	keys 'HTMLPre.background-color'

	Redis KEYS 'color'
	Redis KEYS 'background-color'
```
### BUG: Background color and Font color are not change after Javascript changes css attribute.
* It seems to me Firefox is caching css attribute. I have no idea how to remove or reload with Javascript
* If private mode is used, the colors are changed.

### Add buttons to increase or decrease score

Sun 20 Nov 11:50:23 2022 
### Fixed bug: Send the wrong Http header
```
    ("Content-type", "application/pdf") => ("Content-type", "image/png")
```
Sun 20 Nov 11:47:58 2022
### Move all *.png file to **pdfimage** dir

Wed 23 Nov 14:17:01 2022 
### Fixed missing png file in Safari browser 
* Move all png files to pdfimage folder

#### Wed 30 Nov 12:10:03 2022 
### NEW: After Update, Delete, Add score, Subtract score buttons are pressed => Reload data from Server
* Store the last command in Redis database
    * redisSet 'keyLastKey' 's tt'
* upateCodeBlock
    * lastCmd <- redisGet 'keyLastKey'
    * Send lastCmd `jason.retdata` in JSON to Client
    * In aronlib.js, capture the JSON object => lastCmd
        * updateCodeBlock
        * deleteCodeBlock
        * addScoreCodeBlock
        * subtractScoreCodeBlock

    * In aronlib.js, call doneTyping(json.retdata);
        * json.retdata = 's tt'

    * Test me

