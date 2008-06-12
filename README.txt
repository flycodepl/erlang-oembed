=============
erlang-oembed
=============


What is this?
-------------

An Erlang wrapper for oEmbed requests.


What is oEmbed?
---------------

To quote http://oembed.com/:

  oEmbed is a format for allowing an embedded representation of a URL on
  third party sites. The simple API allows a website to display embedded
  content (such as photos or videos) when a user posts a link to that
  resource, without having to parse the resource directly.


What do I need?
---------------

Other than Erlang, you'll need the following pieces of code:

  erlang-rfc4627: http://www.lshift.net/~tonyg/erlang-rfc4627/

  erlang-fmt: http://tfletcher.com/dev/erlang-fmt


The Makefile assumes that these are contained in the parent directory of
this one, so you might want to edit the Makefile if not.


How do I use it?
----------------

The inets application needs to be running, and although it may seem obvious
it's probably worth noting that the above dependencies need to be compiled.

Requests can be made by using oembed:request/2, i.e.,

  {ok, Props} = oembed:request(URL, Endpoint).


Props is a proplist containing the data from the oEmbed response. All keys
are converted to atoms. Values are mostly strings, but some are converted to
integers (e.g., width) or atoms (e.g., type). If you need to pass additional
parameters to the request, use oembed:request/3.

Errors can also be returned, including but not limited to the following:

  {error, unauthorized}

  {error, not_found}

  {error, not_implemented}


Run "make test" to see an example response.


Who can I contact if I have another question?
---------------------------------------------

Tim Fletcher (http://tfletcher.com/).
