-module(oembed).

-export([request/2, request/3]).


request(URL, Endpoint) ->
  request(URL, Endpoint, []).

request(URL, Endpoint, Params) ->
  get_json(string:concat(Endpoint, query_params_string([{url, URL}, {format, json}|Params]))).

get_json(URL) ->
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
  {ok, [process_prop(KV) || KV <- Props]};
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

query_params_string([]) ->
  [];
query_params_string(Params) ->
  [$?|string:join([query_param_string(Param) || Param <- Params], "&")].

query_param_string({Name, Value}) ->
  lists:concat([percent_encode(Name), "=", percent_encode(Value)]).

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

percent_encode(Term) when is_integer(Term) ->
  integer_to_list(Term);
percent_encode(Term) when is_atom(Term) ->
  atom_to_list(Term);
percent_encode(Term) when is_list(Term) ->
  percent_encode(lists:reverse(Term, []), []).

percent_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
  percent_encode(T, [X | Acc]);
percent_encode([X | T], Acc) ->
  NewAcc = [$%, hexchr(X bsr 4), hexchr(X band 16#0f) | Acc],
  percent_encode(T, NewAcc);
percent_encode([], Acc) ->
  Acc.

-compile({inline, [{hexchr, 1}]}).

hexchr(N) when N >= 10 ->
  N + $A - 10;
hexchr(N) when N < 10 ->
  N + $0.
