Stripe Erlang Library
=====================

![Erlang CI](https://github.com/niho/stripe/workflows/Erlang%20CI/badge.svg)

The Stripe Erlang library provides access to some of the most commonly used Stripe features.

- Payment intents
- Customers
- Products
- Subscriptions
- Checkout
- Connect
- Webhooks

This library is **not** an official Stripe library and only implements a subset of
all functionality.

Documentation
-------------

Please see the generated (edoc documentation)[https://niho.github.io/stripe/doc/].

Usage
-----

### Example: Payment intent

```erlang
{ok, PaymentIntent} = stripe_payment:create(2000, <<"EUR">>, [], []),
ClientSecret = stripe_payment:client_secret(PaymentIntent).
```

### Configuration

The API key can be configured using either the application configuration key `api_key`
or using the `STRIPE_API_KEY` environment variable.

```erlang
application:set_env(stripe, api_key, "sk_test_123").
```

### Webhooks

The library includes a (Cowboy)[https://github.com/ninenines/cowboy] compatible webhook
handler that handles verification of the Stripe signature and other details. Use it by
writing a Cowboy handler that implements the `stripe_webhook` behaviour and upgrades the
request using the `stripe_webhook` module (similar to how `cowboy_rest` works).

```
-module(my_stripe_webhook_handler).
-behaviour(stripe_webhook).

-export([init/2, handle_event/2]).

init(Req, State) ->
    {stripe_webhook, Req, State}.

handle_event({<<"charge.succeeded">>, _EventData}, _State) ->
    ok;
handle_event({<<"invoice.paid">>, _EventData, _Account}, _State) ->
    %% An event from a Connect account.
    ok;
handle_event(_Event, _State) ->
    ok.
```

The webhook secret that is used to verify the signature of webhook requests can be
configured using the `webhook_secret` application configuration key.

```erlang
application:set_env(stripe, webhook_secret, "...").
```


Copyright and License
---------------------

Copyright (c) 2021, Niklas Holmgren.

Released under the terms of the Apache License 2.0. See [LICENSE](./LICENSE) for details.
