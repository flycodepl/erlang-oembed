An Erlang wrapper for oEmbed (http://oembed.com/) requests.

Quick start:

  $ make
  ...
  $ erl -pa ebin -s inets
  ...
  1> oembed:test().
  ...
  ok
  2> oembed:request("...URL...", "...Endpoint..."). % defaults to format=xml
  ...
  3> oembed:request("...URL...", "...Endpoint...", [{format, json}]).
  ...


Depends on erlang-rfc4627 (http://www.lshift.net/~tonyg/erlang-rfc4627/)
for decoding JSON responses.
