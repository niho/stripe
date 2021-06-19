% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
-module(stripe).

-export([endpoint/0, api_key/0, api_version/0]).

-define(ENDPOINT, "https://api.stripe.com/v1").
-define(API_VERSION, "2020-08-27").


%% @doc The Stripe API endpoint URL to use. Can be configured using the
%% application environment configuration (set the `endpoint' key) or the
%% OS environment variable `STRIPE_ENDPOINT'. Environment variable has
%% precedence over application config.
%%
%% Default is "https://api.stripe.com/v1".
endpoint() ->
    os:getenv("STRIPE_ENDPOINT",
              application:get_env(stripe, endpoint, ?ENDPOINT)).

%% @doc The API key to use for requests to Stripe. Can be configured using the
%% application environment configuration (set the `api_key' key) or the
%% OS environment variable `STRIPE_API_KEY'. Environment variable has
%% precedence over application config.
api_key() ->
    case os:getenv("STRIPE_API_KEY") of
        false ->
            case application:get_env(stripe, api_key) of
                {ok, ApiKey} ->
                    ApiKey;
                undefined ->
                    throw({error, missing_stripe_api_key})
            end;
        ApiKey ->
            ApiKey
    end.

%% @doc The Stripe API version that this library supports (2020-08-27).
api_version() ->
    ?API_VERSION.
