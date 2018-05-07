# haskell-webservice

This project contains a web service written in Haskell using Happstack and SQLite.
The web service provides an interface to (fictitious, randomly generated) weather 
readings taken at the North Pole during 2017. It has one endpoint, `weather/date/`,  
the response to which will depend on the request type.

If the webservice is running on `localhost` on port 8000 and receives a `GET` or
`POST` request to `http://localhost:8000/weather/date/YYYY-mm-dd` then it will
return a JSON object with the data for that date. If weather data is held for that
date, the response code will be 200 and the body will consist of JSON something like 
this:

    [{"date":"2017-01-01","temperature":-23.164497}]

If no data exists for the date in question, the response code will be 404 and the body 
will consist of an empty JSON object.

We can also make `PUT` requests to insert or update a date/temperature pair in the database.
In this case the URL needs to include the temperature, e.g.

    http://localhost:8000/date/1970-01-01/0

If the date already exists, the temperature is updated. If not, a new record is added to the 
database. This call will return 200 and an empty JSON object if everything went OK.

## Installation

**If you are using Windows, you should install CygWin and use the CygWin terminal instead 
of the Windows command prompt, as several of the packages you need require a bash-like environment.**

**If you are working on this code in the labs, do not install it on a Windows drive (e.g. `~/W_DRIVE` 
or `~/M_DRIVE`), as cabal is known to have problems with Windows network shares.**

To set the project up you need to start by making sure that you have the latest version of
`cabal-install` on this machine:

    $ cabal update
    $ cabal install cabal-install
	
Now you can grab the code and start building it:

    $ git clone https://github.com/jimburton/haskell-webservice
    $ cd haskell-webservice
    $ cabal sandbox init
    $ cabal configure
	
This last step may report that several libraries are missing. For example:

    $ cabal configure
    Resolving dependencies...
    Configuring haskell-webservice-0.1.0.0...
    cabal: At least the following dependencies are missing:
    aeson -any,
    happstack-server -any,
    hslogger -any,
    sqlite-simple -any

If so, install them now:

	$ cabal install aeson happstack-server hslogger sqlite-simple

Now you should be able to continue building the application:

    $ cabal configure
	$ cabal build
	$ cabal install
	$ cabal run DB-setup

The step to build the database only needs to be done once but you can
run it any time you want to restore the database to its original
state. Now you can start the webservice:

    $ cabal run webservice 
    Listening for http:// on port 8000


Once the service is running you can interact with it in the
browser or using something like `curl`:

    $ curl http://localhost:8000/weather/date/2017-01-01
	[{"date":"2017-01-01","temperature":-23.164497}]
	
While the service is running open a new terminal and run the tests with `cabal test`. 
Read the log, which will be in a file called something like 
`dist/test/haskell-webservice-0.1.0.0-test-webservice.log`.

## Assignment

Your assignment is to extend the webservice in various ways. Before doing so you should read 
the first two chapters of the [HappStack Book](http://happstack.com/docs/crashcourse/index.html). 
You may also need to check the docs for [HappStack](https://hackage.haskell.org/package/happstack-server)
and [sqlite-simple](https://hackage.haskell.org/package/sqlite-simple-0.4.14.0/docs/Database-SQLite-Simple.html), 
the library we are using to interact with the database. As you work on the methods, keep running the tests
and calling the webservice with `curl` or a browser to see what comes back.

1. Add a new endpoint to the webservice, `weather/range/d1/d2`, where `d1` and `d2` are dates in the format 
`YYYY-mm-dd`. When this endpoint receives a `GET` request it should return all records in the database that
fall between `d1` and `d2` (i.e. greater than or equal to `d1` and less than or equal to `d2`) as an array of 
JSON objects. 

2. Add a new endpoint to the webservice, `weather/max/d1/d2`, where `d1` and `d2` are dates in the format 
`YYYY-mm-dd`. When this endpoint receives a `GET` request it should return the details of the day with the
maximum temperature between `d1` and `d2` (i.e. greater than or equal to `d1` and less than or equal to `d2`) 
as an array containing a single JSON object. 

3. Add a new endpoint to the webservice, `weather/above/t`, where `t` is a signed floating point number. 
When this endpoint receives a `GET` request it should return all records in the database where the
temperature is greater than or equal to `t` as an array of JSON objects. Other request methods should result 
in a 405 response code ("Method not allowed") and an empty JSON object.

4. **Extension** Convert the web service to use the `web-routes` library as described in 
[chapter 7 of the HappStack Book](http://happstack.com/docs/crashcourse/WebRoutes.html#web-routes).

## Submission instructions

The submission instructions for Assignment 2 were originally to store your work in an online git repository
and to submit the link on studentcentral. These instructions need to be changed for several reasons.
What I'd now like you to do is to submit a zip file containing the project to studentcentral in the usual way.
Before zipping up your work, use cabal to remove the sandbox and clean the project to reduce the size of the final archive.
That is, you need to run the following commands:

    $ cabal sandbox delete
    $ cabal clean
    
If your work is in a public repository online please remove it straight away or make the repository private.
Github provides free accounts for students that allow for private repositories.
See https://help.github.com/articles/applying-for-a-student-developer-pack/

Please double check that your zip file contains your work before you submit it.
Do not use 7z, rar or other proprietary compression tools.

## Marking criteria

20 marks are available for each of the four criteria, with 20 marks available for the elegance, efficiency
and readability of the code. Read one of the Haskell style guides linked to from studentcentral and use
the `hlint` tool to improve your mark in this criterion. These marking criteria are indicative, meaning that
I may award extra marks for work that uses an interesting approach and shows independent learning. 
