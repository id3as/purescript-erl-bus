-module(stateBus@foreign).

-include_lib("gproc/src/gproc_int.hrl").

-export([ create/2
        , delete/1
        , raiseImpl/3
        , subscribeImpl/3
        , subscribeExistingImpl/2
        , unsubscribe/1
        ]).

%%------------------------------------------------------------------------------
%% FFI helpers
%%------------------------------------------------------------------------------
-define(unit, {unit}).
-define(just(A), {just, A}).
-define(nothing, {nothing}).
-define(versionedMsg(Gen, Msg), {versionedMsg, Gen, Msg}).
-define(initialState(Gen, State), #{state => State, generation => Gen}).
-define(initialStateMsg(Msg), {initialStateMsg, Msg}).

-define(gprocPropertyKey(Name), {p,l,{mb, Name}}).
-define(gprocNameKey(Name), {n,l,{mb, Name}}).
-define(stateKey, md).
-define(stateAttribute(Md), {?stateKey, Md}).
-define(locked, locked).

create(BusName, InitalState) ->
  fun() ->
      NameKey = ?gprocNameKey(BusName),
      StateAndGeneration = {0, InitalState},
      gproc:reg(NameKey, undefined, [?stateAttribute(StateAndGeneration)]),
      _ = spawn_link(fun() -> create_watcher(BusName, NameKey) end),
      raiseMsgInt(BusName, ?initialStateMsg(?initialState(0, InitalState))),
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

subscribeImpl(StateMsgConstructor, BusName, Mapper) ->
  fun() ->
      MaybeState = subscribeLocked(BusName),
      case MaybeState of
        ?just({Generation, ExistingState}) ->
          maybe_send(self(), Mapper(StateMsgConstructor(?initialState(Generation, ExistingState))));
        ?nothing ->
          ok
      end,
      true = gproc:set_value(?gprocPropertyKey(BusName), Mapper),
      ?unit
  end.

subscribeExistingImpl(BusName, Mapper) ->
  fun() ->
      MaybeState = subscribeLocked(BusName),
      case MaybeState of
        ?just({Generation, State}) ->
          true = gproc:set_value(?gprocPropertyKey(BusName), Mapper),
          ?just(?initialState(Generation, State));
        ?nothing ->
          catch gproc:unreg(?gprocPropertyKey(BusName)),
          ?nothing
      end
  end.

subscribeLocked(BusName) ->
  try
    true = gproc:reg(?gprocPropertyKey(BusName), ?locked),
    State = gproc:get_attribute(?gprocNameKey(BusName), ?stateKey),
    ?just(State)
  catch
    error:badarg ->
      ?nothing
  end.

raiseImpl(UpdateState, BusName, Msg) ->
  fun() ->
      NameKey = ?gprocNameKey(BusName),
      {Generation, State} = gproc:get_attribute(NameKey, ?stateKey),

      NewState = (UpdateState(Msg))(State),
      NextGen = Generation + 1,
      raiseMsgInt(BusName, {dataMsg, {versionedMsg, NextGen, Msg}}),
      gproc:set_attributes(NameKey, [?stateAttribute({NextGen, NewState})])

  end.

raiseMsgInt(BusName, Msg) ->
  Key = ?gprocPropertyKey(BusName),
  ?CATCH_GPROC_ERROR(send1(Key, Msg), [Key, Msg]).

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
      lists:foreach(fun({Pid, Fn}) ->
                        maybe_send(Pid, Fn(Msg))
                    end, Entries)
  end.


maybe_send(_, ?nothing) -> ok;
maybe_send(Pid, ?just(MappedMsg)) ->
  %% io:format(user, "Sending ~p to ~p~n", [MappedMsg, Pid]),
  Pid ! MappedMsg.
