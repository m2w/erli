# erli

**erli** is an Erlang based URL-shortener, with statistics and 'nsfw' functionality.

## statistics

Every shortened URL keeps track of visitor counts (non-real time total and unique visit counts) and provides estimates for the geographic location of visitors. 

## 'nsfw' functionality

Appending `?landing=true` to a shortened URL, prevents the automatic redirect, instead a landing page, showing the full target URL and an "are you sure you want to proceed" button, is displayed. 

Reporting inappropriate URLs is kept extremely simple, just add `/report` to the URL.  URLs that reach a certain threshold - this mechanism will be expanded in the future (pull requests are most welcome ;)) - are permanetly banned.

## demo

Live demo will be up as soon as I get around to it. 

To play around with it on localhost, just clone, `make compile`, `./start.sh` and open [http://localhost:8000](http://localhost:8000).

## todo

usability:

+ timeline of visits by (UTC) time-range: morning (6-11), mid-day (11-16), evening (16-22), night (22-6)
+ thumbnails on the landing pages?
+ ensure that no matter the `STATS_COLLECT_INTERVAL`, statistics don't get corrupted

code quality:

+ enforce the Erlang coding standards: [http://www.erlang.se/doc/programming_rules.shtml](http://www.erlang.se/doc/programming_rules.shtml)
+ eunit tests

## the api

### root_resource.erl

	GET /  -> displays the index
	POST / -> create a new shortened URL with a generated path

### path_resource.erl

	GET /path    -> grab the redirect to the target URL
	DELETE /path -> report the target
	PUT /path    -> create a new shortened url with a preferred path
	-----------------
	GET /path/report -> report the target URL
	-----------------
	GET /path/stats  -> view stats for the path
	-----------------
	GET /path/check -> utility URL to facility 'low' overhead 
	                   checking whether a path is already taken 
					   via ajax (@ W3C please give us an option 
					   to disable the automatic redirect on 30x!)
