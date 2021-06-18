-module(stripe_product).

-export([create/2, create/3,
         retrieve/1, retrieve/2,
         update/2, update/3,
         delete/1, delete/2,
         list/1, list/2
        ]).

%% @doc Create a product
create(Name, Options) ->
    create(Name, Options, []).

%% @doc Create a product
create(Name, Options, Headers) when is_binary(Name),
                                    is_list(Options),
                                    is_list(Headers) ->
    Body = [{"name", binary_to_list(Name)}] ++ Options,
    {200,_,Product} =
        stripe_client:post("products", Headers, Body),
    {ok, Product}.

%% @doc Retrieve a product
retrieve(Id) ->
    retrieve(Id, []).

%% @doc Retrieve a product
retrieve(Id, Headers) ->
    {200,_,Product} =
        stripe_client:get(
          lists:flatten(["products","/",Id]), Headers, []),
    {ok, Product}.

%% @doc Update a product
update(Id, Options) ->
    update(Id, Options, []).

%% @doc Update a product
update(Id, Options, Headers) ->
    {200,_,Product} =
        stripe_client:post(
          lists:flatten(["products","/",Id]), Headers, Options),
    {ok, Product}.

%% @doc Delete a product
delete(Id) ->
    delete(Id, []).

%% @doc Delete a product
delete(Id, Headers) ->
    {200,_,Product} =
        stripe_client:delete(
          lists:flatten(["products","/",Id]), Headers, []),
    {ok, Product}.

%% @doc List all products
list(Options) ->
    list(Options, []).

%% @doc List all products
list(Options, Headers) ->
    {200,_,#{<<"data">>:=Products}} =
        stripe_client:get("products", Options, Headers),
    {ok, Products}.
