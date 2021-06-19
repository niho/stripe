% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
-module(stripe_checkout).

-export([create/5, create/6,
         retrieve/1, retrieve/2,
         list/1, list/2,
         line_items/2, line_items/3
        ]).

%% @doc Create a checkout session
create(Mode, PaymentMethodTypes, SuccessUrl, CancelUrl, Options) ->
    create(Mode, PaymentMethodTypes, SuccessUrl, CancelUrl, Options, []).

%% @doc Create a checkout session
create(Mode, PaymentMethodTypes, SuccessUrl, CancelUrl, Options, Headers)
  when Mode =:= payment orelse
       Mode =:= setup orelse
       Mode =:= subscription,
       is_list(PaymentMethodTypes),
       length(PaymentMethodTypes) > 0,
       is_binary(SuccessUrl) or is_list(SuccessUrl),
       is_binary(CancelUrl) or is_list(CancelUrl) ->
    Body = [{"mode", atom_to_list(Mode)},
            {"success_url", SuccessUrl},
            {"cancel_url", CancelUrl}
           ]
        ++ lists:map(fun(PaymentMethod) ->
                             {"payment_method_types[]", PaymentMethod}
                     end, PaymentMethodTypes)
        ++ Options,
    {200,_,Session} =
        stripe_client:post("checkout/sessions", Headers, Body),
    {ok, Session}.

%% @doc Retrieve a checkout session
retrieve(Id) ->
    retrieve(Id, []).

%% @doc Retrieve a checkout session
retrieve(Id, Headers) ->
    {200,_,Session} =
        stripe_client:get(
          lists:flatten(["checkout/sessions","/",Id]), [], Headers),
    {ok, Session}.

%% @doc List all checkout session
list(Options) ->
    list(Options, []).

%% @doc List all checkout sessions
list(Options, Headers) ->
    {200,_,#{<<"data">>:=Sessions}} =
        stripe_client:get("checkout/sessions", Options, Headers),
    {ok, Sessions}.

%% @doc Retrieve a checkout session's line items
line_items(Id, Options) ->
    line_items(Id, Options, []).

%% @doc Retrieve a checkout session's line items
line_items(Id, Options, Headers) ->
    {200,_,#{<<"data">>:=LineItems}} =
        stripe_client:get(
          lists:flatten(["checkout/sessions","/",Id,"/","line_items"]),
                        Options, Headers),
    {ok, LineItems}.
