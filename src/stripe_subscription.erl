-module(stripe_subscription).

-export([create/3, create/4,
         retrieve/1, retrieve/2,
         update/2, update/3,
         cancel/2, cancel/3,
         list/1, list/2
        ]).

%% @doc Create a subscription
create(Customer, Items, Options) ->
    create(Customer, Items, Options, []).

%% @doc Create a subscription
create(#{<<"id">>:=Customer,<<"object">>:=<<"customer">>},
       Items, Options, Headers) ->
    create(Customer, Items, Options, Headers);
create(Customer, Items, Options, Headers)
  when is_binary(Customer), is_list(Items),
       is_list(Options), is_list(Headers) ->
    Body = [{"customer", binary_to_list(Customer)},
            {"items", Items}
           ] ++ Options,
    {200,_,Subscription} =
        stripe_client:post("subscriptions", Headers, Body),
    {ok, Subscription}.

%% @doc Retrieve a subscription
retrieve(Id) ->
    retrieve(Id, []).

%% @doc Retrieve a subscription
retrieve(Id, Headers) ->
    {200,_,Subscription} =
        stripe_client:get(
          lists:flatten(["subscriptions","/",Id]), Headers, []),
    {ok, Subscription}.

%% @doc Update a subscription
update(Id, Options) ->
    update(Id, Options, []).

%% @doc Update a subscription
update(Id, Options, Headers) ->
    {200,_,Subscription} =
        stripe_client:post(
          lists:flatten(["subscriptions","/",Id]), Headers, Options),
    {ok, Subscription}.

%% @doc Cancel a subscription
cancel(Id, Options) ->
    cancel(Id, Options, []).

%% @doc Cancel a subscription
cancel(Id, Options, Headers) ->
    {200,_,Subscription} =
        stripe_client:delete(
          lists:flatten(["subscriptions","/",Id]), Headers, Options),
    {ok, Subscription}.

%% @doc List subscriptions
list(Options) ->
    list(Options, []).

%% @doc List subscriptions
list(Options, Headers) ->
    {200,_,#{<<"data">>:=Subscriptions}} =
        stripe_client:get("subscriptions", Options, Headers),
    {ok, Subscriptions}.
