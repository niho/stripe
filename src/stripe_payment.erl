-module(stripe_payment).

-export([create/3, create/4,
         client_secret/1,
         retrieve/1, retrieve/2, retrieve/3,
         update/2, update/3,
         confirm/2, confirm/3,
         capture/2, capture/3,
         cancel/2, cancel/3
        ]).

%% @doc Create a PaymentIntent
create(Amount, Currency, Options) ->
    create(Amount, Currency, Options, []).

%% @doc Create a PaymentIntent
create(Amount, Currency, Options, Headers)
  when is_integer(Amount), is_binary(Currency),
       is_list(Options), is_list(Headers) ->
    Body = [{"amount", integer_to_list(Amount)},
            {"currency", string:lowercase(Currency)}
           ] ++ Options,
    {200,_,PaymentIntent} =
        stripe_client:post("payment_intents", Headers, Body),
    {ok, PaymentIntent}.

%% @doc Get the client secret from a PaymentIntent
client_secret(_PaymentIntent = #{<<"client_secret">> := ClientSecret}) ->
    ClientSecret;
client_secret(_) ->
    undefined.

%% @doc Retrieve a PaymentIntent
retrieve(Id) ->
    retrieve(Id, []).

%% @doc Retrieve a PaymentIntent
retrieve(Id, ClientSecret) when is_binary(Id), is_binary(ClientSecret) ->
    retrieve(Id, ClientSecret, []);
retrieve(Id, Headers) when is_binary(Id), is_list(Headers) ->
    {200,_,PaymentIntent} =
        stripe_client:get({"payment_intents",Id}, Headers, []),
    {ok, PaymentIntent}.

%% @doc Retrieve a PaymentIntent
retrieve(Id, ClientSecret, Headers) when is_binary(Id), is_binary(ClientSecret),
                                         is_list(Headers) ->
    {200,_,PaymentIntent} =
        stripe_client:get({"payment_intents",Id}, Headers,
                          [{"client_secret", binary_to_list(ClientSecret)}]),
    {ok, PaymentIntent}.

%% @doc Update a PaymentIntent
update(Id, Options) ->
    update(Id, Options, []).

%% @doc Update a PaymentIntent
update(Id, Options, Headers) ->
    {200,_,PaymentIntent} =
        stripe_client:post({"payment_intents",Id}, Headers, Options),
    {ok, PaymentIntent}.

%% @doc Confirm a PaymentIntent
confirm(Id, Options) ->
    confirm(Id, Options, []).

%% @doc Confirm a PaymentIntent
confirm(Id, Options, Headers) ->
    {200,_,PaymentIntent} =
        stripe_client:post({"payment_intents",Id,"confirm"}, Headers, Options),
    {ok, PaymentIntent}.

%% @doc Capture a PaymentIntent
capture(Id, Options) ->
    capture(Id, Options, []).

%% @doc Capture a PaymentIntent
capture(Id, Options, Headers) ->
    {200,_,PaymentIntent} =
        stripe_client:post({"payment_intents",Id,"capture"}, Headers, Options),
    {ok, PaymentIntent}.

%% @doc Cancel a PaymentIntent
cancel(Id, Reason) ->
    cancel(Id, Reason, []).

%% @doc Cancel a PaymentIntent
cancel(Id, Reason, Headers) when Reason =:= duplicate orelse
                                 Reason =:= fraudulent orelse
                                 Reason =:= requested_by_customer orelse
                                 Reason =:= abandoned ->
    Body = [{"cancellation_reason", atom_to_list(Reason)}],
    {200,_,PaymentIntent} =
        stripe_client:post({"payment_intents",Id,"cancel"}, Headers, Body),
    {ok, PaymentIntent}.
