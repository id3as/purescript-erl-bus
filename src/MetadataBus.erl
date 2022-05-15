-module(metadataBus@foreign).

-include_lib("gproc/src/gproc_int.hrl").

-export([ create/2
        , delete/1
        , disable/1
        , enable/1
        , raiseMsg/2
        , subscribeImpl/4
        , subscribeExistingImpl/3
        , updateMetadata/2
        , unsubscribe/1
        ]).

-define(unit, {unit}).
-define(just(A), {just, A}).
-define(nothing, {nothing}).

-define(gprocPropertyKey(Name), {p,l,{mb, Name}}).
-define(gprocNameKey(Name), {n,l,{mb, Name}}).
-define(metadataKey, md).
-define(metadataAttribute(Md), {?metadataKey, Md}).
-define(enabled(MapperFn), {e, MapperFn}).
-define(disabled(MapperFn), {d, MapperFn}).

-define(locked, locked).

create(BusName, InitalMetadata) ->
  fun() ->
      NameKey = ?gprocNameKey(BusName),
      gproc:reg(NameKey, undefined, [?metadataAttribute(InitalMetadata)]),
      _ = spawn_link(fun() -> create_watcher(BusName, NameKey) end),
      raiseMsgInt(BusName, {metadataMsg, InitalMetadata}),
      BusName
  end.


create_watcher(BusName, NameKey) ->
  Ref = gproc:monitor(NameKey),
  receive
    {gproc, unreg, Ref, NameKey } ->
      ok
  end,
  raiseMsgInt(BusName, {busTerminated}).

delete(BusName) ->
  fun() ->
      gproc:unreg(?gprocNameKey(BusName)),
      ?unit
  end.

updateMetadata(BusName, Metadata) ->
  fun() ->
      gproc:set_attributes(?gprocNameKey(BusName), [?metadataAttribute(Metadata)]),
      raiseMsgInt(BusName, {metadataMsg, Metadata}),
      ?unit
  end.

subscribeImpl(Enabled, MetadataConstructor, BusName, Mapper) ->
  fun() ->
      MaybeMetadata = subscribeLocked(Enabled, BusName),
      case MaybeMetadata of
        ?just(ExistingMetadata) ->
          maybe_send(self(), Mapper(MetadataConstructor(ExistingMetadata)));
        ?nothing ->
          ok
      end,
      true = gproc:set_value(?gprocPropertyKey(BusName), ?enabled(Mapper)),
      ?unit
  end.

subscribeExistingImpl(Enabled, BusName, Mapper) ->
  fun() ->
      MaybeMetadata = subscribeLocked(Enabled, BusName),
      case MaybeMetadata of
        ?just(_) ->
          true = gproc:set_value(?gprocPropertyKey(BusName), ?enabled(Mapper));
        ?nothing ->
          catch gproc:unreg(?gprocPropertyKey(BusName))
      end,
      MaybeMetadata
  end.

subscribeLocked(enabled, BusName) ->
  try
    true = gproc:reg(?gprocPropertyKey(BusName), ?locked),
    Metadata = gproc:get_attribute(?gprocNameKey(BusName), ?metadataKey),
    ?just(Metadata)
  catch
    error:badarg ->
      ?nothing
  end.

raiseMsg(BusName, Msg) ->
  fun() ->
      raiseMsgInt(BusName, Msg)
  end.

raiseMsgInt(BusName, Msg) ->
  Key = ?gprocPropertyKey(BusName),
  ?CATCH_GPROC_ERROR(send1(Key, Msg), [Key, Msg]).

disable(BusName) ->
  fun() ->
      Key = ?gprocPropertyKey(BusName),
      case gproc:lookup_value(Key) of
        ?enabled(Fn) -> gproc:set_value(Key, ?disabled(Fn));
        ?disabled(_) -> ok
      end,
      ?unit
  end.

enable(BusName) ->
  fun() ->
      Key = ?gprocPropertyKey(BusName),
      case gproc:get_value(Key) of
        ?enabled(_) -> ok;
        ?disabled(Fn) -> gproc:set_value(Key, ?enabled(Fn))
      end,
      ?unit
  end.

unsubscribe(BusName) ->
  fun() ->
      Key = ?gprocPropertyKey(BusName),
      catch gproc:unreg(Key),
      ?unit
  end.

%%------------------------------------------------------------------------------
%% Internal functions - being slight variants of the functions by the same name
%% in gproc.erl
%%------------------------------------------------------------------------------
send1(Key, Msg) ->
  Entries = ets:select(gproc, [{{{Key,'_'}, '$1', '$2'},[],[{{'$1','$2'}}]}]),
  case lists:any(fun({_, ?locked}) -> true;
                    (_) -> false
                 end,
                 Entries) of
    true ->
      timer:sleep(0),
      send1(Key, Msg);
    false ->
      lists:foreach(fun({Pid, ?enabled(Fn)}) ->
                        maybe_send(Pid, Fn(Msg));
                       (_) -> ok
                    end, Entries)
  end.


maybe_send(_, ?nothing) -> ok;
maybe_send(Pid, ?just(MappedMsg)) ->
  %% io:format(user, "Sending ~p to ~p~n", [MappedMsg, Pid]),
  Pid ! MappedMsg.
