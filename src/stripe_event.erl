% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
-module(stripe_event).

-export([retrieve/1, retrieve/2,
         list/1, list/2
         ]).

%% @doc Retrieve an event
retrieve(Id) ->
    retrieve(Id, []).

%% @doc Retrieve an event
retrieve(Id, Headers) ->
    {200,_,Event} =
        stripe_client:get(lists:flatten(["events","/",Id]), [], Headers),
    {ok, Event}.

%% @doc List all events
list(Options) ->
    list(Options, []).

%% @doc List all events
list(Options, Headers) ->
    {200,_,#{<<"data">>:=Events}} =
        stripe_client:get("events", Options, Headers),
    {ok, Events}.
