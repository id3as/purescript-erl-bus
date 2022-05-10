-module(simpleBus@foreign).

-include_lib("gproc/src/gproc_int.hrl").

-export([ subscribeImpl/3
        , raise/2
        , enable/1
        , disable/1
        , unsubscribe/1
        ]).

-define(key(Name), {p,l, {sb, Name}}).
-define(enabled(MapperFn), {e, MapperFn}).
-define(disabled(MapperFn), {d, MapperFn}).
-define(unit, {unit}).

subscribeImpl(enabled, BusName, Mapper) ->
  fun() ->
      true = gproc:reg(?key(BusName), ?enabled(Mapper)),
      ?unit
  end;
subscribeImpl(disabled, BusName, Mapper) ->
  fun() ->
      true = gproc:reg(?key(BusName), ?disabled(Mapper)),
      ?unit
  end.



%%------------------------------------------------------------------------------
%% This code is taken from gproc.erl but slightly modified to extract and use
%% a stored mapping function per recipient and also to enable active / stopped
%% toggling on busses
%%------------------------------------------------------------------------------
raise(BusName, Msg) ->
  fun() ->
      Key = ?key(BusName),
      ?CATCH_GPROC_ERROR(send1(Key, Msg), [Key, Msg])
  end.

disable(BusName) ->
  fun() ->
      Key = ?key(BusName),
      case gproc:lookup_value(Key) of
        ?enabled(Fn) -> gproc:set_value(Key, ?disabled(Fn));
        ?disabled(_) -> ok
      end,
      ?unit
  end.

enable(BusName) ->
  fun() ->
      Key = ?key(BusName),
      case gproc:get_value(Key) of
        ?enabled(_) -> ok;
        ?disabled(Fn) -> gproc:set_value(Key, ?enabled(Fn))
      end,
      ?unit
  end.

unsubscribe(BusName) ->
  fun() ->
      Key = ?key(BusName),
      gproc:unreg(Key),
      ?unit
  end.


%%------------------------------------------------------------------------------
%% Internal functions - being slight variants of the functions by the same name
%% in gproc.erl
%%------------------------------------------------------------------------------
send1(Key, Msg) ->
  lists:foreach(fun({Pid, Fn}) ->
                    Pid ! Fn(Msg)
                end, lookup_pids(Key)).

lookup_pids(Key) ->
  ets:select(gproc, [{{{Key,'_'}, '$1', ?enabled('$2')},[],[{{'$1','$2'}}]}]).
