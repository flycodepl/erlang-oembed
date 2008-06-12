-module(oembed_test).

-export([all/0]).

-import(proplists, [get_value/2]).


all() ->
  Endpoint = "http://www.flickr.com/services/oembed/",
  
  URL = "http://flickr.com/photos/apelad/2351180594/",

  {ok, Props} = oembed:request(URL, Endpoint),

  error_logger:info_msg("oEmbed response: ~p~n~n", [Props]),

  photo = get_value(type, Props),

  500 = get_value(width, Props),

  "Laugh-Out-Loud Cats #792" = get_value(title, Props).
