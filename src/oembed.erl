-module(oembed).

-export([request/2, request/3, test/0]).

-include_lib("xmerl/include/xmerl.hrl").


test() ->
  Endpoint = "http://www.flickr.com/services/oembed/",
  URL = "http://flickr.com/photos/apelad/2351180594/",
  {ok, Props} = oembed:request(URL, Endpoint),
  error_logger:info_msg("oEmbed response: ~p~n~n", [Props]),
  "Laugh-Out-Loud Cats #792" = proplists:get_value(title, Props),
  "photo" = proplists:get_value(type, Props),
  "500" = proplists:get_value(width, Props),
  ok.

request(URL, Endpoint) ->
  request(URL, Endpoint, [{format, xml}]).

request(URL, Endpoint, Params) ->
  String = string:concat(Endpoint, query_params_string([{url, URL}|Params])),
  handle(httpc:request(String)).

handle({ok, {{_, Code, _}, Headers, Body}}) ->
  handle(Code, Headers, Body);
handle(Else) ->
  Else.

handle(501, _, _) ->
  {error, not_implemented};
handle(404, _, _) ->
  {error, not_found};
handle(401, _, _) ->
  {error, unauthorized};
handle(200, Headers, Body) ->
  case proplists:lookup("content-type", Headers) of
    {_, ContentType} ->
      case hd(string:tokens(ContentType, ";")) of
        "application/json" ->
          case mochijson2:decode(Body) of
            {ok, {obj, Props}, []} ->
              {ok, [{K, binary_to_list(V)} || {K, V} <- Props]};
            Else ->
              Else
          end;
        "text/xml" ->
          {XML, []} = xmerl_scan:string(Body),
          {ok, [xml_prop(Element) || Element <- xmerl_xpath:string("//oembed/*", XML)]};
        _ ->
          {error, invalid_media_type}
      end;
    none ->
      {error, unknown_content_type}
  end.

xml_prop(#xmlElement{name=Name, content=[Text]}) ->
  {Name, Text#xmlText.value}.

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
