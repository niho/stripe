% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Stripe HTTP client.

-module(stripe_client).

-define(ENDPOINT, "https://api.stripe.com/v1").
-define(TIMEOUT, 5000).

-export([get/3,
         post/3,
         put/3,
         delete/2
        ]).

-type query() :: list({unicode:chardata(), unicode:chardata()}).
-type response() :: {httpc:status_code(),
                     httpc:headers(),
                     string() | binary() | jsx:json_term()
                    }.

-spec get(httpc:path(), query(), httpc:headers()) -> response().
get(Path, Query, Headers) ->
    request(get,
            {request_uri(Path, Query),
             Headers ++ request_headers()
            }).

-spec post(httpc:path(), httpc:headers(), jsx:json_term()) -> response().
post(Path, Headers, Body) ->
    request(post,
            {request_uri(Path),
             Headers ++ request_headers(),
             "application/x-www-form-urlencoded",
             uri_string:compose_query(Body)
            }).

-spec put(httpc:path(), httpc:headers(), jsx:json_term()) -> response().
put(Path, Headers, Body) ->
    request(put,
            {request_uri(Path),
             Headers ++ request_headers(),
             "application/x-www-form-urlencoded",
             uri_string:compose_query(Body)
            }).

-spec delete(httpc:path(), httpc:headers()) -> response().
delete(Path, Headers) ->
    request(delete, 
            {request_uri(Path),
             Headers ++ request_headers()
            }).


%%%%%%%%%%%%%%
%% INTERNAL %%
%%%%%%%%%%%%%%

-spec request_uri(httpc:path() |
                  {httpc:path(), binary()} |
                  {httpc:path(), binary(), string()}) ->
          uri_string:uri_string().
request_uri({Path,Id}) ->
    lists:flatten([request_uri(Path), "/", binary_to_list(Id)]);
request_uri({Path,Id,Action}) ->
    lists:flatten([request_uri(Path), "/", binary_to_list(Id), "/", Action]);
request_uri(Path) ->
    Endpoint = application:get_env(stripe, endpoint, ?ENDPOINT),
    lists:flatten([Endpoint, "/", Path]).

-spec request_uri(httpc:path(), query()) -> uri_string:uri_string().
request_uri(Path, #{}) ->
    request_uri(Path);
request_uri(Path, Query) ->
    lists:flatten([request_uri(Path), "?", uri_string:compose_query(Query)]).

-spec authorization_bearer(binary()) -> string().
authorization_bearer(AccessToken) ->
    io_lib:format("Bearer ~s", [AccessToken]).

-spec request_headers() -> httpc:headers().
request_headers() ->
    {ok, ApiKey} = application:get_env(stripe, api_key),
    [{"Authorization", authorization_bearer(ApiKey)}
    ].

-spec request(httpc:method(), httpc:request()) -> response().
request(Method, Request) ->
    logger:debug(#{method => Method,
                   request => Request
                  }),
    {ok,{{_,Status,_},ResponseHeaders,ResponseBody}} =
        httpc:request(Method, Request,
                      [{timeout, application:get_env(stripe, timeout, ?TIMEOUT)},
                       {ssl, ssl_options()}
                      ],
                      []),
    logger:debug(#{status => Status,
                   headers => ResponseHeaders,
                   body => ResponseBody
                  }),
    handle_response(Status, ResponseHeaders, ResponseBody).

-spec handle_response(httpc:status_code(), httpc:headers(), string() | binary()) ->
          response().
handle_response(Status, ResponseHeaders, []) -> {Status,ResponseHeaders,[]};
handle_response(Status, ResponseHeaders, ResponseBody) ->
    case content_type(ResponseHeaders) of
        {"application/json", _} ->
            BinaryBody = unicode:characters_to_binary(ResponseBody, utf8),
            {Status,ResponseHeaders,jsx:decode(BinaryBody)};
        "application/json" ->
            {Status,ResponseHeaders,jsx:decode(iolist_to_binary(ResponseBody))};
        _ ->
            {Status,ResponseHeaders,ResponseBody}
    end.

content_type(Headers) ->
    case proplists:get_value("content-type", Headers) of
        undefined -> undefined;
        ContentType when is_list(ContentType) ->
            case string:split(string:lowercase(ContentType), ";") of
                [MimeType] -> MimeType;
                [MimeType, Charset] -> {MimeType, Charset}
            end
    end.

ssl_options() ->
    [{verify, verify_peer},
     {cacertfile, code:priv_dir(stripe) ++ "/ca-certificates.crt"}
    ].