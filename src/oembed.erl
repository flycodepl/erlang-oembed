-module(oembed).

-export([request/2, request/3]).

-import(fmt, [percent_encode/1]).


request(URL, Endpoint) ->
  request(URL, Endpoint, []).

request(URL, Endpoint, Opts) ->
  Params = [{url, URL}, {format, json}|Opts],
  do_get(Endpoint ++ [$?|to_query_string(Params)]).

to_query_string(Params) when is_list(Params) ->
  string:join(lists:map(fun to_query_string/1, Params), "&");
to_query_string({K, V}) ->
  lists:flatten([percent_encode(K), $=, percent_encode(V)]).

do_get(URL) ->
  case http:request(URL) of
    {ok, Response} ->
      case status_code(Response) of
        200 ->
          process_response(rfc4627:decode(body(Response)));
        401 ->
          {error, unauthorized};
        404 ->
          {error, not_found};
        501 ->
          {error, not_implemented};
        _ ->
          Response
      end;
    Else ->
      Else
  end.

status_code(Response) ->
  element(2, element(1, Response)).

body(Response) ->
  element(3, Response).

process_response({ok, {obj, Props}, []}) ->
  {ok, lists:map(fun process_prop/1, Props)};
process_response(Else) ->
  Else.

process_prop({K, V}) when is_list(K) ->
  process_prop({list_to_atom(K), binary_to_list(V)});
process_prop({type, V}) ->
  {type, list_to_atom(V)};
process_prop({K, V}) ->
  case is_integer_prop(K) of
    true ->
      {K, list_to_integer(V)};
    false ->
      {K, V}
  end.

is_integer_prop(cache_age) -> true;
is_integer_prop(thumbnail_width) -> true;
is_integer_prop(thumbnail_height) -> true;
is_integer_prop(width) -> true;
is_integer_prop(height) -> true;
is_integer_prop(_) -> false.
