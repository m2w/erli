# erli

**erli** is an Erlang based URL-shortener, with simple statistics and 'nsfw' functionality.

## Statistics

Every shortened URL keeps track of visitor counts (which are unique due to 301 caching) and provides estimates for the geographic location of visitors as well as what time of the day (UTC) they visited the link.

## 'nsfw' functionality

Appending `?landing=true` to a shortened URL, prevents the automatic redirect, instead a landing page with the full target URL and a "are you sure you want to proceed" button, is displayed.

# community curated

Reporting inappropriate URLs is kept extremely simple, just add `/report` to the URL. When the number of reports for a URL exceeds a configurable threshold it is banned.

## Local setup

Make sure you are running > R12B. To experiment locally:

* `git clone [git@github.com:m2w/erli.git](git@github.com:m2w/erli.git)`
* `cd erli`
* `make all`
* `./start-erli`
* open [http://localhost:8000](http://localhost:8000)

Alternatively:

* `git clone [git@github.com:m2w/erli.git](git@github.com:m2w/erli.git)`
* `cd erli`
* `make generate`
* `./rel/erli/bin/erli start`
* open [http://localhost:8000](http://localhost:8000)

Run `make docs` for edocs.

## Todo

code quality:

+ more tests (goal is around 60% coverage with eunit and full API coverage with the integration tests)

## the API

### root_resource.erl

	GET /            -> displays the index
	POST /           -> create a new shortened URL with a server-generated path

### path_resource.erl

	GET /path        -> grab the redirect to the target URL
	DELETE /path     -> report the target
	PUT /path        -> create a new shortened url with a preferred path
	-----------------
	GET /path/report -> report the target URL
	-----------------
	GET /path/stats  -> view stats for the path
	-----------------
	GET /path/check  -> utility URL to facility 'low' overhead
	                    checking whether a path is already taken
					    via ajax
