%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc The API root resource, with links to all API endpoints.
%%% @end
%%%==========================================================

-module(erli_root_resource).

%% Webmachine Callbacks
-export([init/1,
	 allowed_methods/2,
	 options/2,
	 generate_etag/2,
	 content_types_provided/2,
	 as_json/2]).

-include("models.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%-----------------------------------------------------------
%% Webmachine Callbacks
%%-----------------------------------------------------------

init(_Options) ->
    {ok, {}}.


allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'OPTIONS'], RD, Ctx}.


options(RD, Ctx) ->
    {[{"Content-Length", "0"},
      {"Allow", "GET, HEAD, OPTIONS"}], RD, Ctx}.


generate_etag(RD, Ctx) ->
    {"", RD, Ctx}.


content_types_provided(RD, Ctx) ->
    {[{"application/json", as_json}], RD, Ctx}.


as_json(RD, Ctx) ->
    Data = jsx:encode([{<<"title">>, <<"erli API">>},
		      {<<"description">>,
		       <<"simple REST API to the erli URL shortening service">>},
		      {<<"rels">>, [{<<"visits">>, <<"/api/visits/">>},
				    {<<"paths">>, <<"/api/paths/">>},
				    {<<"targets">>, <<"/api/targets">>}]}]),
    {Data, RD, Ctx}.
