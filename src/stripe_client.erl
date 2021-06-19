% @author Niklas Holmgren <niklas.holmgren@gmail.com>
% @copyright 2021 Niklas Holmgren
% @doc Stripe HTTP client.
-module(stripe_client).

-define(TIMEOUT, 5000).

-export([get/3,
         post/3,
         delete/3
        ]).

-type path() :: string().
-type status_code() :: integer().
-type headers() :: [header()].
-type header() :: {field(), value()}.
-type field() :: string().
-type value() :: string() | binary() | integer() | float() | atom().
-type body() :: list({field(), value()}).

-type query() :: list({unicode:chardata(), unicode:chardata()}).
-type response() :: {status_code(),
                     headers(),
                     string() | binary() | jsx:json_term()
                    }.

-spec get(path(), query(), headers()) -> response().
get(Path, Query, Headers) ->
    request(get,
            {request_uri(Path, Query),
             encode_headers(Headers) ++ request_headers()
            }).

-spec post(path(), headers(), body()) -> response().
post(Path, Headers, Body) ->
    request(post,
            {request_uri(Path),
             encode_headers(Headers) ++ request_headers(),
             "application/x-www-form-urlencoded",
             encode_form_body(Body)
            }).

-spec delete(path(), headers(), body()) -> response().
delete(Path, Headers, Body) ->
    request(delete,
            {request_uri(Path),
             encode_headers(Headers) ++ request_headers(),
             "application/x-www-form-urlencoded",
             encode_form_body(Body)
            }).


%%%%%%%%%%%%%%
%% INTERNAL %%
%%%%%%%%%%%%%%

request_uri({Path,Id}) ->
    lists:flatten([request_uri(Path), "/", binary_to_list(Id)]);
request_uri({Path,Id,Action}) ->
    lists:flatten([request_uri(Path), "/", binary_to_list(Id), "/", Action]);
request_uri(Path) ->
    lists:flatten([stripe:endpoint(), "/", Path]).

request_uri(Path, #{}) ->
    request_uri(Path);
request_uri(Path, Query) ->
    lists:flatten([request_uri(Path), "?", uri_string:compose_query(Query)]).

authorization_bearer(AccessToken) ->
    io_lib:format("Bearer ~s", [AccessToken]).

request_headers() ->
    [{"Authorization", authorization_bearer(stripe:api_key())},
     {"Stripe-Version", stripe:api_version()}
    ].

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

handle_response(Status, ResponseHeaders, []) -> {Status,ResponseHeaders,[]};
handle_response(Status, ResponseHeaders, ResponseBody) ->
    case content_type(ResponseHeaders) of
        {"application/json", _} ->
            BinaryBody = unicode:characters_to_binary(ResponseBody, utf8),
            {Status,ResponseHeaders,jsx:decode(BinaryBody)};
        "application/json" ->
            {Status,ResponseHeaders,jsx:decode(iolist_to_binary(ResponseBody))};
        _ ->
            {Status,ResponseHeaders,jsx:decode(iolist_to_binary(ResponseBody))}
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

encode_form_body(Body) ->
    uri_string:compose_query(
      lists:filtermap(
        fun({K,V}) ->
                case V of
                    undefined -> false;
                    _ -> {true, {K, encode_form_value(V)}}
                end
        end, Body)).

encode_form_value(Value) when is_list(Value) -> Value;
encode_form_value(Value) when is_binary(Value) -> unicode:characters_to_list(Value, utf8);
encode_form_value(Value) when is_integer(Value) -> integer_to_list(Value);
encode_form_value(Value) when is_float(Value) -> io_lib:format("~.2f",[Value]);
encode_form_value(Value) when is_atom(Value) -> atom_to_list(Value).

encode_headers(Headers) ->
    lists:filtermap(
        fun({K,V}) ->
                case V of
                    undefined -> false;
                    _ -> {true, {K, encode_form_value(V)}}
                end
        end, Headers).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_form_body_test() ->
    ?assertEqual([], encode_form_body([])),
    ?assertEqual("string=string&binary=binary&integer=42&float=3.14&atom=true",
                 encode_form_body([{"string", "string"},
                                   {"binary", <<"binary">>},
                                   {"integer", 42},
                                   {"float", 3.1415},
                                   {"atom", true},
                                   {"undefined", undefined}
                                  ])).

encode_headers_test() ->
    ?assertEqual([], encode_headers([])),
    ?assertEqual([{"string","string"},
                  {"binary","binary"},
                  {"integer","42"},
                  {"float","3.14"},
                  {"atom","true"}
                 ],
                 encode_headers([{"string", "string"},
                                 {"binary", <<"binary">>},
                                 {"integer", 42},
                                 {"float", 3.1415},
                                 {"atom", true},
                                 {"undefined", undefined}
                                ])).

-endif.
