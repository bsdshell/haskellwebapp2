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

