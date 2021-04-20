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
will consist of an empty JSON array.

We can also make `PUT` requests to insert or update a date/temperature pair in the database.
In this case the URL needs to include the temperature. The easiest way to make a `PUT`
request is by using `curl`:

    $ curl -X PUT http://localhost:8000/date/1970-01-01/42
	[]
	$ curl http://localhost:8000/weather/date/1970-01-01
    [{"temperature":42,"date":"1970-01-01"}]

If the date already exists, the temperature is updated. If not, a new record is added to the 
database. This call will return 200 and an empty JSON array if everything went OK.

## Installation

<!---
**If you are using Windows, you should install CygWin and use the CygWin terminal instead 
of the Windows command prompt, as several of the packages you need require a bash-like environment.**

**If you are working on this code in the labs, do not install it on a Windows drive (e.g. `~/W_DRIVE` 
or `~/M_DRIVE`), as cabal is known to have problems with Windows network shares.**
-->

This project relies on a local SQLite database, so you need to have
SQLite installed to run the code. On Debian-based Linux systems
(including Ubuntu) you can do this with the command `sudo apt install
sqlite3`. [These
instructions](https://www.sqlitetutorial.net/download-install-sqlite/)
cover other operating systems.

Set up the project in the usual way:

    $ git clone https://github.com/jimburton/haskell-webservice
    $ cd haskell-webservice
    $ cabal configure
	
Now you can start setting up the application. There is a cabal target
to install the database:

	$ cabal run DB-setup

The step to build the database only needs to be done once but you can
run it any time you want to restore the database to its original
state. Now you can start the webservice:

    $ cabal run webservice 
    Listening for http:// on port 8000


Once the service is running you can interact with it in the browser by
going to http://localhost:8000/weather/date/2017-01-01 or in a
terminal by using something like `curl`:

    $ curl http://localhost:8000/weather/date/2017-01-01
	[{"date":"2017-01-01","temperature":-23.164497}]
	
While the service is running open a new terminal and run the tests with 

    cabal run test-webservice

Note that one test passes but three fail. You can get them all
working by completing the exercise below.

## Exercises

Your job is to extend the webservice in various ways. Before doing so
you should read the first two chapters of the [HappStack
Book](http://happstack.com/docs/crashcourse/index.html).  You may also
need to look at the docs for
[HappStack](https://hackage.haskell.org/package/happstack-server) and
[sqlite-simple](https://hackage.haskell.org/package/sqlite-simple-0.4.14.0/docs/Database-SQLite-Simple.html),
the library we are using to interact with the database. As you work on
the methods, keep running the tests and calling the webservice with
`curl` or a browser to see what comes back.

1. Add a new endpoint to the webservice, `weather/range/d1/d2`, where
   `d1` and `d2` are dates in the format `YYYY-mm-dd`. When this
   endpoint receives a `GET` request it should return all records in
   the database that fall between `d1` and `d2` (i.e. greater than or
   equal to `d1` and less than or equal to `d2`) as an array of JSON
   objects.

   Start by changing the `main` function by adding a new route `dirs
   "weather/range" ...` to the list it contains. Copy one of the
   existing routes for the format. This route needs to call a new
   function that you will define in `WeatherService.Service`. Call it
   `rangeHandler`. Again, you can copy and adapt one of the existing
   functions such as `dayHandler` or `dayPutHandler` to get this
   working. Test your function in the browser and/or a terminal.

2. Add a new endpoint to the webservice, `weather/max/d1/d2`, where
   `d1` and `d2` are dates in the format `YYYY-mm-dd`. When this
   endpoint receives a `GET` request it should return the details of
   the day with the maximum temperature between `d1` and `d2`
   (i.e. greater than or equal to `d1` and less than or equal to `d2`)
   as an array containing a single JSON object.

3. Add a new endpoint to the webservice, `weather/above/t`, where `t`
   is a signed floating point number.  When this endpoint receives a
   `GET` request it should return all records in the database where
   the temperature is greater than or equal to `t` as an array of JSON
   objects. Other request methods should result in a 405 response code
   ("Method not allowed") and an empty JSON object.

4. Convert the web service to use the `web-routes` library as
   described in [chapter 7 of the HappStack
   Book](http://happstack.com/docs/crashcourse/WebRoutes.html#web-routes).

Compare your work to mine in the `solutions` branch. There is a 
version of the webservice that uses `web-routes` in `src/Main.hs`
and a version that doesn't in `src/Main-NoRoutes.hs`.
