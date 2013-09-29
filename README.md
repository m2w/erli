# erli

**NOTE**: erli is currently undergoing major changes, it is not stable in any way nor feature complete.

**erli** is an Erlang based URL shortener.

It started as a more involved experiment with REST and webmachine as an abstraction of it. 
**erli** comes with custom URLs, simple statistics for each shortened URL and optional landing pages (that include screenshots of the target website).

## Status

**erli** is currently still undergoing a rewrite. Remaining TODOs:

- [ ] Redesign the storage backend to be a separate application to be included in the release that allows mnesia node management etc
- [ ] A simple HTTP frontend
- [ ] (Re-)Implement an error handler and adapt tests accordingly
- [ ] Review and optimize test quality and coverage
- [ ] An internal API endpoint either as based on a binary protocol over tcp (cool) or as rpc calls (simple)
- [ ] Test suite(s) for thumbnail generation related components
- [ ] Refactor the entire code base into 4 distinct applications (erli, erli\_frontend, erli\_backend, erli\_storage)
- [ ] A proper release
- [ ] A demo instance (without thumbnail generation)

## The backend

The backend is a CRUD API written in webmachine using mnesia as storage backend. Its root URL is at ``/api/``. The API is designed to be self-explanatory and -discoverable. However, here is a quick overview:

### API Endpoints and Applicable Methods

    /api/paths                          GET, HEAD, OPTIONS, POST
    /api/paths/{PATH_ID}                GET, HEAD, OPTIONS, DELETE
    /api/paths/{PATH_ID}/targets        GET, HEAD, OPTIONS
    /api/paths/{PATH_ID}/visits         GET, HEAD, OPTIONS
    /api/targets                        GET, HEAD, OPTIONS, POST
    /api/targets/{TARGET_ID}            GET, HEAD, OPTIONS, DELETE
    /api/targets/{TARGET_ID}/paths      GET, HEAD, OPTIONS
    /api/targets/{TARGET_ID}/visits     GET, HEAD, OPTIONS
    /api/visits                         GET, HEAD, OPTIONS
    /api/visits/{VISIT_ID}              GET, HEAD, OPTIONS

### Interna

For collection types pagination is handled via the ``Range`` header. The data returned by the server is intended to suffice for client driven exploration.

Purging of unwanted or infringing content is community driven in the form of infringement flagging. Every DELETE request towards a path or target resource counts towards a configurable limit (15 by default). The limit is per target, since multiple paths may point to the same target. Once the limit is reached all paths pointing to said target are marked as banned, as is the target itself. All requests to banned resources are answered with a ``410 GONE``.
Note that the current implementation is "dumb". To avoid storing user-related data on the server there is no way to prevent users from abusing the flagging process.
