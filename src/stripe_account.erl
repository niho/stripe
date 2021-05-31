-module(stripe_account).

-export([create/1, create/2, retrieve/1, update/2, delete/1, reject/2, list/0, list/1]).
-export([create_link/4, create_link/5]).

-define(RESOURCE_NAME, "accounts").

-type type() :: custom | express | standard.
-type options() :: list({string(), string() | number()}).
-type account() :: map().
-type id() :: binary().

%% @doc Create an account
-spec create(type()) -> {ok, account()}.
create(Type) ->
    create(Type, []).

%% @doc Create an account
-spec create(type(), options()) -> {ok, account()}.
create(Type, Options) when Type =:= custom orelse
                           Type =:= express orelse
                           Type =:= standard,
                           is_list(Options) ->
    Body = [{"type", atom_to_list(Type)}] ++ Options,
    {200,_,Account} = stripe_client:post(?RESOURCE_NAME, [], Body),
    {ok, Account}.

%% @doc Retrieve an account
-spec retrieve(id()) -> {ok, account()}.
retrieve(Id) when is_binary(Id) ->
    {200,_,Account} = stripe_client:get({?RESOURCE_NAME, Id}, [], []),
    {ok, Account}.

%% @doc Update an account
-spec update(id(), options()) -> {ok, account()}.
update(Id, Options) when is_binary(Id), is_list(Options) ->
    {200,_,Account} = stripe_client:post({?RESOURCE_NAME, Id}, [], Options),
    {ok, Account}.

%% @doc Delete an account
-spec delete(id()) -> ok.
delete(Id) when is_binary(Id) ->
    {200,_,_} = stripe_client:delete({?RESOURCE_NAME, Id}, []),
    ok.

%% @doc Reject an account
-spec reject(id(), fraud | terms_of_service | other) -> {ok, account()}.
reject(Id, Reason) when is_binary(Id),
                        Reason =:= fraud orelse
                        Reason =:= terms_of_service orelse
                        Reason =:= other ->
    Body = [{"reason", atom_to_list(Reason)}],
    {200,_,Account} = stripe_client:post({?RESOURCE_NAME, Id, "reject"}, [], Body),
    {ok, Account}.

%% @doc List all connected accounts
-spec list() -> {ok, list(account())}.
list() ->
    list([]).

%% @doc List all connected accounts
-spec list(options()) -> {ok, list(account())}.
list(Options) when is_list(Options) ->
    {200,_,#{<<"data">>:=Accounts}} = stripe_client:get(?RESOURCE_NAME, [], Options),
    {ok, Accounts}.

%% @doc Create an account link
create_link(Account, RefreshUrl, ReturnUrl, Type) ->
    create_link(Account, RefreshUrl, ReturnUrl, Type, currently_due).

%% @doc Create an account link (and specify which information to collect)
-spec create_link(map() | id(), string() | binary(), string() | binary(),
                  account_onboarding | account_update,
                  currently_due | eventually_due) ->
          {ok, binary()}.
create_link(#{<<"id">>:=Id}, RefreshUrl, ReturnUrl, Type, Collect) ->
    create_link(Id, RefreshUrl, ReturnUrl, Type, Collect);
create_link(Account, RefreshUrl, ReturnUrl, Type, Collect) when is_binary(Account),
                                                                is_list(RefreshUrl) orelse is_binary(RefreshUrl),
                                                                is_list(ReturnUrl) orelse is_binary(ReturnUrl),
                                                                Type =:= account_onboarding orelse
                                                                Type =:= account_update,
                                                                Collect =:= currently_due orelse
                                                                Collect =:= eventually_due ->
    {200,_,#{<<"url">>:=Url}} =
        stripe_client:post("account_links", [], [{"account", Account},
                                                 {"refresh_url", RefreshUrl},
                                                 {"return_url", ReturnUrl},
                                                 {"type", atom_to_list(Type)},
                                                 {"collect", atom_to_list(Collect)}
                                                ]),
    {ok, Url}.
