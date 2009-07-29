-module(oembed).

-export([request/2, request/3, test/0]).


test() ->
  Endpoint = "http://www.flickr.com/services/oembed/",
  URL = "http://flickr.com/photos/apelad/2351180594/",
  {ok, Props} = oembed:request(URL, Endpoint),
  error_logger:info_msg("oEmbed response: ~p~n~n", [Props]),
  "Laugh-Out-Loud Cats #792" = proplists:get_value("title", Props),
  "photo" = proplists:get_value("type", Props),
  "500" = proplists:get_value("width", Props),
  ok.

request(URL, Endpoint) ->
  request(URL, Endpoint, []).

request(URL, Endpoint, Params) ->
  get_json(string:concat(Endpoint, query_params_string([{url, URL}, {format, json}|Params]))).

get_json(URL) ->
  case http:request(URL) of
    {ok, Response} ->
      case status_code(Response) of
        200 ->
          process(rfc4627:decode(body(Response)));
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

process({ok, {obj, Props}, []}) ->
  {ok, [{K, binary_to_list(V)} || {K, V} <- Props]};
process(Else) ->
  Else.

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
