% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Cowboy request handler for Stripe webhook requests.
%
% Usage:
%
%  -module(stripe_webhook_handler).
%  -behaviour(stripe_webhook).
%
%  -export([init/2, handle_event/2]).
%
%  init(Req, State) ->
%    {stripe_webhook, Req, State}.
%
%  handle_event({<<"charge.succeeded">>, _EventData}, _State) ->
%    ok;
%  handle_event({<<"invoice.paid">>, _EventData, _Account}, _State) ->
%    %% An event from a Connect account.
%    ok;
%  handle_event(_Event, _State) ->
%    ok.
%
-module(stripe_webhook).

-export([upgrade/4]).
-export([upgrade/5]).

-define(DEFAULT_TOLERANCE, 300).
-define(EXPECTED_SCHEME, "v1").
-define(IP_WHITELIST,
        [{3,18,12,63},
         {3,130,192,231},
         {13,235,14,237},
         {13,235,122,149},
         {35,154,171,200},
         {52,15,183,38},
         {54,187,174,169},
         {54,187,205,235},
         {54,187,216,72},
         {54,241,31,99},
         {54,241,31,102},
         {54,241,34,107}
        ]).

-type event() :: {binary(), binary(), map()} |
                 {binary(), map()} |
                 map().

-callback handle_event(Event :: event(), State :: any()) -> ok.

upgrade(Req, Env, Handler, HandlerState, _Opts) ->
    upgrade(Req, Env, Handler, HandlerState).

upgrade(Req0=#{method:=<<"POST">>,peer:=Ip}, Env, Handler, HandlerState0) ->
    case lists:member(Ip, ?IP_WHITELIST) of
        true ->
            {ok, Payload, Req1} = cowboy_req:read_body(Req0),
            StripeSignature = cowboy_req:header(<<"stripe-signature">>, Req1),
            case verify_signature(StripeSignature, Payload, webhook_secret()) of
                true ->
                    Event = decode_event(jsx:decode(Payload)),
                    case Handler:handle_event(Event, HandlerState0) of
                        ok ->
                            {ok, cowboy_req:reply(204, Req1), Env};
                        {ok, _State} ->
                            {ok, cowboy_req:reply(204, Req1), Env}
                    end;
                false ->
                    {ok, cowboy_req:reply(403, Req1), Env}
            end;
        false ->
            {ok, cowboy_req:reply(403, Req0), Env}
    end;
upgrade(Req, Env, _Handler, _HandlerState) ->
    {ok, cowboy_req:reply(405, Req), Env}.

verify_signature(StripeSignature, Payload, Secret) ->
    Scheme = application:get_env(stripe, webhook_signature_scheme,
                                 ?EXPECTED_SCHEME),
    case timestamp_and_signature(StripeSignature, Scheme) of
        {T,V1} ->
            ExpectedSig = compute_signature(T, Payload, Secret),
            string:equal(ExpectedSig, V1) and verify_timestamp(T);
        undefined ->
            false
    end.

verify_timestamp(T) ->
    Tolerance = application:get_env(stripe, webhook_tolerance,
                                    ?DEFAULT_TOLERANCE),
    (T < (os:system_time(second) + Tolerance)) and
        (T > (os:system_time(second) - Tolerance)).

timestamp_and_signature(Header, Scheme) when is_binary(Header) ->
    timestamp_and_signature(binary_to_list(Header), Scheme);
timestamp_and_signature(Header, Scheme) ->
    try
        Sig = lists:map(fun(X) ->
                                [K,V] = string:split(string:trim(X), "="),
                                {K,V}
                        end,
                        string:split(Header, ",", all)),
        {list_to_integer(proplists:get_value("t", Sig)),
         proplists:get_value(Scheme, Sig)}
    catch
        error:_ -> undefined
    end.

compute_signature(Timestamp, Payload, Secret) ->
    TimestampedPayload = io_lib:format("~p.~s", [Timestamp, Payload]),
    Hmac = crypto:mac(hmac, sha256, Secret, TimestampedPayload),
    binary_to_list(string:lowercase(binary:encode_hex(Hmac))).

webhook_secret() ->
    case application:get_env(stripe, webhook_secret) of
        {ok, Secret} -> Secret;
        undefined -> throw({missing_env, webhook_secret})
    end.

decode_event(#{<<"type">>:=Type,
               <<"data">>:=Data,
               <<"account">>:=Account,
               <<"object">>:=<<"event">>
              }) ->
    {Type, Data, Account};
decode_event(#{<<"type">>:=Type,
               <<"data">>:=Data,
               <<"object">>:=<<"event">>
              }) ->
    {Type, Data};
decode_event(Event) ->
    Event.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

compute_signature_test() ->
    ?assertEqual(
       "0b9ad52aabc34a623cc6a8aa23c10c5843d8027635c185098039d6982e6e5ed6",
       compute_signature(1492774577, "test", "secret")).

verify_timestamp_test() ->
    ?assertEqual(true, verify_timestamp(os:system_time(second))),
    ?assertEqual(true, verify_timestamp(os:system_time(second) + 100)),
    ?assertEqual(true, verify_timestamp(os:system_time(second) - 100)),
    ?assertEqual(false, verify_timestamp(os:system_time(second) + 301)),
    ?assertEqual(false, verify_timestamp(os:system_time(second) - 301)).

timestamp_and_signature_test() ->
    Sig = "t=1492774577, " ++
        "v1=5257a869e7ecebeda32affa62cdca3fa51cad7e77a0e56ff536d0ce8e108d8bd, " ++
        "v0=6ffbb59b2300aae63f272406069a9788598b792a944a07aba816edb039989a39",
    ?assertEqual({1492774577,"5257a869e7ecebeda32affa62cdca3fa51cad7e77a0e56ff536d0ce8e108d8bd"},
                 timestamp_and_signature(Sig, "v1")),
    ?assertEqual({1492774577,"5257a869e7ecebeda32affa62cdca3fa51cad7e77a0e56ff536d0ce8e108d8bd"},
                 timestamp_and_signature(list_to_binary(Sig), "v1")),
    ?assertEqual(undefined, timestamp_and_signature("", "v1")),
    ?assertEqual(undefined, timestamp_and_signature(<<"">>, "v1")).

verify_signature_test() ->
    Payload = "test",
    Secret = "secret",
    T = os:system_time(second),
    V1 = compute_signature(T, Payload, Secret),
    Sig = io_lib:format("t=~p, v1=~s", [T, V1]),
    ?assertEqual(true, verify_signature(Sig, Payload, Secret)),
    ?assertEqual(true, verify_signature(list_to_binary(Sig), Payload, Secret)),
    ?assertEqual(false, verify_signature(Sig, "wrong", Secret)),
    ?assertEqual(false, verify_signature(Sig, Payload, "wrong")),
    ?assertEqual(false, verify_signature(undefined, Payload, Secret)),
    ?assertEqual(false, verify_signature("", Payload, Secret)).

decode_event_test() ->
    ?assertEqual({<<"test.event">>, #{}},
                 decode_event(#{<<"type">> => <<"test.event">>,
                                <<"object">> => <<"event">>,
                                <<"data">> => #{}
                               })),
    ?assertEqual({<<"test.event">>, #{}, <<"acc_xyz">>},
                 decode_event(#{<<"type">> => <<"test.event">>,
                                <<"object">> => <<"event">>,
                                <<"account">> => <<"acc_xyz">>,
                                <<"data">> => #{}
                               })),
    ?assertEqual(#{}, decode_event(#{})).

-endif.
