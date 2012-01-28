# UrlPiper

<img src="urlpiper.jpg"
 alt="UrlPiper logo" title="UrlPiper Logo" align="right" />

UrlPiper is an Erlang OTP application that allows callers to start and stop
automatic periodic fetching of URLs.  The cache stores either a raw result, 
the body, or the body of result parsed as XML (using xmerl).  The fetching
interval is configurable.

## Installation

1. Make sure rebar is in your path
2. git clone https://github.com/Erlang-Communitivity/UrlPiper
3. cd UrlPiper
4. rebar compile
5. cd ..
6. erl -pa UrlPiper/ebin

## Example Usage

    start() ->
        urlpiper_app:start_deps(),
        application:start(urlpiper),
        wait_for(urlpiper, fun fetch/0).
    
    wait_for(AppName, Fun) ->
        io:format("waiting for ~w~n", [AppName]),
        case lists:keyfind(AppName, 1, application:which_applications()) of
	    false -> timer:apply_after(3000, ?MODULE, wait_for, [AppName, Fun]);
	    _Found -> Fun()
        end.

    fetch() ->
        io:format("Fetch called~n",[]),
        Url = "http://www.npr.org/rss/rss.php?id=1049",
        urlpiper:start_piping(Url, xml),
        urlpiper:content_for(Url, fun (Url, Content) ->
            			  io:format("Received content for ~s~n", [Url]),
    				  urlpiper:stop_piping(Url)
    			      end
			 ),
    