-module(stripe_customer).

-export([create/1, create/2,
         retrieve/1, retrieve/2,
         update/2, update/3,
         delete/1, delete/2,
         list/1, list/2
        ]).

%% @doc Create a customer
create(Options) ->
    create(Options, []).

%% @doc Create a customer
create(Options, Headers) ->
    {200,_,Customer} =
        stripe_client:post("customers", Headers, Options),
    {ok, Customer}.

%% @doc Retrieve a customer
retrieve(Id) ->
    retrieve(Id, []).

%% @doc Retrieve a customer
retrieve(Id, Headers) ->
    {200,_,Customer} =
        stripe_client:get(lists:flatten(["customers","/",Id]), Headers, []),
    {ok, Customer}.

%% @doc Update a customer
update(Id, Options) ->
    update(Id, Options, []).

%% @doc Update a customer
update(Id, Options, Headers) ->
    {200,_,Customer} =
        stripe_client:post(
          lists:flatten(["customers","/",Id]), Headers, Options),
    {ok, Customer}.

%% @doc Delete a customer
delete(Id) ->
    delete(Id, []).

%% @doc Delete a customer
delete(Id, Headers) ->
    {200,_,_} = stripe_client:delete(
                  lists:flatten(["customers","/",Id]), Headers, []),
    ok.

%% @doc List all customers
list(Options) ->
    list(Options, []).

%% @doc List all customers
list(Options, Headers) ->
    {200,_,#{<<"data">>:=Customers}} =
        stripe_client:get("customers", Options, Headers),
    {ok, Customers}.
